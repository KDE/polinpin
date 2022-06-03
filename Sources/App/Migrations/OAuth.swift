import Fluent

struct OAuthMigration: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("users")
            .field("oauth2_provider", .string)
            .update()

        try await database.schema("users")
            .field("oauth2_identity", .string)
            .update()
    }

    func revert(on database: Database) async throws {
        try await database.schema("users")
            .deleteField("oauth2_provider")
            .update()

        try await database.schema("users")
            .deleteField("oauth2_identity")
            .update()
    }
}
