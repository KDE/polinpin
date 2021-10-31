import Fluent
import Vapor

final class StudyModel: Model {
    static let schema = "studies"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "user_id")
    var user: User

    @Field(key: "name")
    var name: String

    @Field(key: "slug")
    var slug: String

    @Field(key: "study")
    var study: Study

    init() { }

    init(id: UUID? = nil, study: Study, name: String, slug: String, belongsTo user: User) {
        self.id = id
        self.$user.id = user.id!
        self.study = study
        self.slug = slug
        self.name = name
    }
}
