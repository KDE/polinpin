import Fluent
import Vapor

enum StudyKind: String, Codable {
    case treeTest
}

struct StudyData: Codable {
    var title: String
    var slug: String
    var published: Bool
    var password: String?
}

final class Study: Model {
    static let schema = "studies"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "user_id")
    var user: User

    @Field(key: "title")
    var title: String

    @Field(key: "slug")
    var slug: String

    @Field(key: "published")
    var published: Bool

    @OptionalField(key: "password")
    var password: String?

    @Enum(key: "kind")
    var kind: StudyKind

    @OptionalChild(for: \.$study)
    var treeTestStudy: TreeTestStudy?

    init() { }

    func toData() -> StudyData {
        StudyData(title: self.title, slug: self.slug, published: self.published, password: self.password)
    }
}
