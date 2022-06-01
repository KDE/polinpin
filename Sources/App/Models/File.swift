import Fluent
import Vapor

// each individual observation corresponds to one submitted completion from a user
final class DBFile: Model {
    static let schema = "files"

    @ID(key: .id)
    var id: UUID?

    @Field(key: "mimetype")
    var mimetype: String

    @Parent(key: "owner_id")
    var owner: User

    init() { }
}
