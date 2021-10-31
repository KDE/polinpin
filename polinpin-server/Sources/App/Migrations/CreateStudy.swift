import Fluent

struct CreateStudy: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema(StudyModel.schema)
            .id()
            .field("slug", .string, .required)
            .unique(on: "slug")
            .field("name", .string, .required)
            .field("study", .dictionary, .required)
            .field("user_id", .uuid, .required, .references(User.schema, "id"))
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema(StudyModel.schema)
            .delete()
    }
}
