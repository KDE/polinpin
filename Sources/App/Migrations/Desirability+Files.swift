import Fluent

struct DesirabilityFilesMigration: AsyncMigration {
    func prepare(on database: Database) async throws {
        _ = try await database.enum("study_kind")
            .case("desirabilityTest")
            .update()

        try await database.schema("desirability_test_studies")
            .id()
            .field("study_id", .uuid, .required, .references("studies", "id", onDelete: .cascade))
            .field("word_tags", .array(of: .dictionary), .required)
            .field("word_bank", .array(of: .dictionary), .required)
            .field("items", .array(of: .dictionary), .required)
            .field("number_of_words_to_select", .int, .required)
            .create()

        try await database.schema("files")
            .id()
            .field("owner_id", .uuid, .required, .references("users", "id", onDelete: .cascade))
            .field("mimetype", .string, .required)
            .create()

        try await database.schema("desirability_test_study_observations")
            .id()
            .field("desirability_study_id", .uuid, .required, .references("desirability_test_studies", "id", onDelete: .cascade))
            .field("response", .array(of: .dictionary), .required)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("desirability_test_studies").delete()
        try await database.schema("desirability_test_study_observations").delete()
        try await database.schema("files").delete()
        _ = try await database.enum("study_kind")
            .deleteCase("desirabilityTest")
            .update()
    }
}
