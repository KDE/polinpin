import Fluent
import Vapor

final class Session: Model {
    static let schema = "sessions"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "user_id")
    var user: User

    init() { }

    init(belongingTo user: User) {
        self.$user.id = user.id!
    }
}