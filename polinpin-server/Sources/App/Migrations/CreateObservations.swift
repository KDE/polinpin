import Fluent

struct CreateObservation: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema(ObservationModel.schema)
            .id()
            .field("observation", .dictionary, .required)
            .field("study_id", .uuid, .required, .references(StudyModel.schema, "id"))
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema(ObservationModel.schema)
            .delete()
    }
}
