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

    @Children(for: \.$user)
    var studies: [StudyModel]

    init() { }

    init(id: UUID? = nil, name: String, username: String, password: String) {
        self.id = id
        self.name = name
        self.username = username
        self.password = password
    }
}

extension Collection {
    subscript (safe index: Index) -> Element? {
        return indices.contains(index) ? self[index] : nil
    }
}

struct UserAuthenticator: AsyncRequestAuthenticator {
    typealias User = App.User

    func authenticate(
        request req: Request
    ) async throws {
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
