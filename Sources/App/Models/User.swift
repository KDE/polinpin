import Fluent
import Vapor

final class User: Model, Authenticatable {
    static let schema = "users"

    @ID(key: .id)
    var id: UUID?

    @Field(key: "name")
    var name: String

    @Field(key: "username")
    var username: String

    @Field(key: "password")
    var password: String

    @OptionalField(key: "oauth2_provider")
    var externalOAuthProvider: String?

    @OptionalField(key: "oauth2_identity")
    var externalOAuthIdentity: String?

    @Children(for: \.$owner)
    var files: [DBFile]

    init() { }

    init(name: String, username: String, password: String) {
        self.name = name
        self.username = username
        self.password = password
    }
    init(name: String, username: String, provider: String, identity: String) {
        self.name = name
        self.username = username
        self.password = "."
        self.externalOAuthProvider = provider
        self.externalOAuthIdentity = identity
    }
}

extension Collection {
    subscript (safe index: Index) -> Element? {
        return indices.contains(index) ? self[index] : nil
    }
}

struct UserAuthenticator: AsyncRequestAuthenticator {
    typealias User = App.User

    func authenticate(request req: Request) async throws {
        guard let token = req.headers["Authorization"][safe: 0] else {
            return
        }
        guard let uuid = UUID(uuidString: token) else {
            return
        }
        guard let session = try await Session.find(uuid, on: req.db) else {
            return
        }
        guard let user = try await User.find(session.$user.id, on: req.db) else {
            return
        }
        req.auth.login(user)
    }
}
