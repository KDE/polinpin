import Fluent

struct CreateDesirabilityStudy: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema(DesirabilityStudyModel.schema)
            .id()
            .field("slug", .string, .required)
            .unique(on: "slug")
            .field("name", .string, .required)
            .field("user_id", .uuid, .required, .references(User.schema, "id"))
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema(ObservationModel.schema)
            .delete()
    }
}
