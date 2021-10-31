import Fluent

struct CreateSession: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema(Session.schema)
            .id()
            .field("user_id", .uuid, .required, .references(User.schema, "id"))
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema(Session.schema)
            .delete()
    }
}
