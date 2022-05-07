import Fluent
import Vapor

final class DesirabilityStudyModel: Model {
    static let schema = "desirability_studies"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "user_id")
    var user: User

    @Field(key: "name")
    var name: String

    @Field(key: "slug")
    var slug: String
}
