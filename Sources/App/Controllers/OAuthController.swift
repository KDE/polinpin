import Vapor
import Fluent
import Foundation

struct OAuthService: Codable {
    var id: String
    var name: String
    var clientID: String
    var clientSecret: String
    var discoveryDocument: String
    var authorizationURL: String
}

struct OAuthConfig: Codable {
    var redirectURL: String
    var clientLoggedInURL: String
    var services: [OAuthService]
    var normalLoginEnabled: Bool
}

struct DiscoveryDocument: Codable {
    var tokenEndpoint: String
    var userInfoEndpoint: String

    enum CodingKeys: String, CodingKey {
        case tokenEndpoint = "token_endpoint"
        case userInfoEndpoint = "userinfo_endpoint"
    }
}

let config = try! JSONDecoder().decode(OAuthConfig.self, from: try! String(contentsOfFile: "Polinpin OAuth Configuration.json").data(using: .utf8)!)

func randomString(length: Int) -> String {
  let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  return String((0..<length).map{ _ in letters.randomElement()! })
}

actor NoncePile {
    private var pile: Set<String> = []

    func add(_ value: String) {
        pile.insert(value)
    }
    func remove(_ value: String) {
        pile.remove(value)
    }
    func contains(_ value: String) -> Bool {
        pile.contains(value)
    }
}

extension Sequence {
    func asyncMap<T>(
        _ transform: (Element) async throws -> T
    ) async rethrows -> [T] {
        var values = [T]()

        for element in self {
            try await values.append(transform(element))
        }

        return values
    }
}

extension Optional {
    func throwing() throws -> Wrapped {
        if let wrapped = self {
            return wrapped
        } else {
            throw "Trying to access nil value"
        }
    }
}

final class OAuth2Controller: RouteCollection {
    var noncePile = NoncePile()

    func boot(routes: RoutesBuilder) throws {
        let oauth2 = routes.grouped("oauth2")
        oauth2.get("services", use: services)
        oauth2.get("callback", use: callback)
    }
    struct ResponseOAuthService: Content {
        var name: String
        var url: String
    }
    struct ResponseOAuthServices: Content {
        var services: [ResponseOAuthService]
        var normalLoginEnabled: Bool
    }
    func services(req: Request) async throws -> ResponseOAuthServices {
        return ResponseOAuthServices(
            services: try await config.services.asyncMap {
                guard var base = URLComponents(string: $0.authorizationURL) else {
                    throw Abort(.internalServerError)
                }

                let nonce = randomString(length: 30)
                await noncePile.add(nonce)

                var queryItems: [URLQueryItem] = []
                queryItems.append(URLQueryItem(name: "client_id", value: $0.clientID))
                queryItems.append(URLQueryItem(name: "response_type", value: "code"))
                queryItems.append(URLQueryItem(name: "redirect_uri", value: config.redirectURL))
                queryItems.append(URLQueryItem(name: "nonce", value: nonce))
                queryItems.append(URLQueryItem(name: "scope", value: "openid profile"))
                queryItems.append(URLQueryItem(name: "state", value: $0.id))

                base.queryItems = queryItems

                return ResponseOAuthService(name: $0.name, url: base.url!.absoluteString)
            },
            normalLoginEnabled: config.normalLoginEnabled
        )
    }
    func callback(req: Request) async throws -> Response {
        guard let kindID = try? req.query.get(String.self, at: "state") else {
            throw Abort(.badRequest, reason: "Request didn't have state \(req.parameters)")
        }
        guard let code = try? req.query.get(String.self, at: "code") else {
            throw Abort(.badRequest, reason: "Request didn't have code")
        }
        guard let index = config.services.firstIndex(where: { $0.id == kindID }) else {
            throw Abort(.badRequest, reason: "Couldn't locate service \(kindID)")
        }
        let service = config.services[index]
        let discoveryDocument = try await req.client.get(URI(string: service.discoveryDocument))
        guard let body = discoveryDocument.body else {
            throw Abort(.internalServerError, reason: "Failed to get body for discovery document")
        }
        let discoveryDoc = try JSONDecoder().decode(DiscoveryDocument.self, from: body)
        let claims = try await req.client.post(URI(string: discoveryDoc.tokenEndpoint)) { req in
            struct TokenRequest: Encodable {
                var code: String
                var client_id: String
                var client_secret: String
                var redirect_uri: String
                let grant_type: String = "authorization_code"
            }
            try req.content.encode(TokenRequest(
                code: code,
                client_id: service.clientID,
                client_secret: service.clientSecret,
                redirect_uri: config.redirectURL
            ), as: .urlEncodedForm)
        }
        guard let claimBody = claims.body else {
            throw Abort(.internalServerError, reason: "Failed to get content body")
        }
        struct TokenResponse: Decodable {
            var access_token: String
        }
        let response = try JSONDecoder().decode(TokenResponse.self, from: claimBody)
        struct UserData: Decodable {
            var sub: String
            var name: String?
            var email: String?
        }
        let userInfoResp = try await req.client.get(URI(string: discoveryDoc.userInfoEndpoint), headers: ["Authorization": "Bearer \(response.access_token)"])
        guard let userInfoBody = userInfoResp.body else {
            throw Abort(.internalServerError, reason: "Failed to get body for user info")
        }
        let userData = try JSONDecoder().decode(UserData.self, from: userInfoBody)

        let token: String
        let theUser: User
        if let user = try await User.query(on: req.db)
            .filter(\.$externalOAuthProvider == service.id)
            .filter(\.$externalOAuthIdentity == userData.sub)
            .first() {

            let session = Session(belongingTo: user)
            try await session.create(on: req.db)

            token = session.id!.uuidString
            theUser = user
        } else {
            guard let userName = userData.name else {
                throw Abort(.internalServerError, reason: "i don't have a name, TODO: request one")
            }

            let user = User(
                name: userName,
                username: clean(string: userName) + randomString(length: 4),
                provider: service.id,
                identity: userData.sub
            )

            token = try await req.db.transaction { db in
                try await user.create(on: db)

                let session = Session(belongingTo: user)
                try await session.create(on: db)

                return session.id!.uuidString
            }
            theUser = user
        }

        guard var clientURL = URLComponents(string: config.clientLoggedInURL) else {
            throw Abort(.internalServerError)
        }

        var queryItems: [URLQueryItem] = []
        queryItems.append(URLQueryItem(name: "name", value: theUser.name))
        queryItems.append(URLQueryItem(name: "username", value: theUser.username))
        queryItems.append(URLQueryItem(name: "token", value: token))

        clientURL.queryItems = queryItems

        return req.redirect(to: clientURL.url!.absoluteString, type: .temporary)
    }
}

