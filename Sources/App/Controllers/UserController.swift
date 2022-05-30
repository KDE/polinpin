import Vapor
import Fluent

final class UserController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let auth = routes.grouped("auth")
        auth.post("login", use: login)
        auth.post("register", use: register)
        auth.get("me", use: me)
    }

    struct UserSession: Content {
        var token: String
    }

    func login(req: Request) async throws -> UserSession {
        struct LoginRequest: Content {
            var username: String
            var password: String
        }
        let request = try req.content.decode(LoginRequest.self)

        guard let user = try await User.query(on: req.db)
            .filter(\.$username == request.username)
            .first() else {
                throw Abort(.notFound, reason: "User \(request.username) doesn't exist")
            }

        guard try await req.password.async.verify(request.password, created: user.password) else {
            throw Abort(.unauthorized, reason: "Incorrect password")
        }

        let session = Session(belongingTo: user)
        try await session.create(on: req.db)

        return UserSession(token: session.id!.uuidString)
    }
    func register(req: Request) async throws -> UserSession {
        struct RegisterRequest: Content {
            var name: String
            var username: String
            var password: String
        }
        let request = try req.content.decode(RegisterRequest.self)
        let user = User(
            name: request.name,
            username: request.username,
            password: try await req.password.async.hash(request.password)
        )

        return try await req.db.transaction { db in
            try await user.create(on: db)

            let session = Session(belongingTo: user)
            try await session.create(on: db)

            return UserSession(token: session.id!.uuidString)
        }
    }
    struct UserInformation: Content {
        var name: String
        var username: String
    }
    func me(req: Request) async throws -> UserInformation {
        let user: User = try req.auth.require()
        return UserInformation(name: user.name, username: user.username)
    }
}