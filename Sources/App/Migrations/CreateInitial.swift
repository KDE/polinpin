import Fluent

struct InitialMigration: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("users")
            .id()
            .field("name", .string, .required)
            .field("username", .string, .required)
            .unique(on: "username")
            .field("password", .string, .required)
            .create()

        try await database.schema("sessions")
            .id()
            .field("user_id", .uuid, .required, .references("users", "id", onDelete: .cascade))
            .create()

        let studyKind = try await database.enum("study_kind")
            .case("treeTest")
            .create()

        try await database.schema("studies")
            .id()
            .field("user_id", .uuid, .required, .references("users", "id", onDelete: .cascade))
            .field("title", .string, .required)
            .field("slug", .string, .required)
            .unique(on: "slug", "user_id")
            .field("kind", studyKind, .required)
            .field("published", .bool, .required)
            .field("password", .string)
            .create()

        try await database.schema("tree_test_studies")
            .id()
            .field("study_id", .uuid, .required, .references("studies", "id", onDelete: .cascade))
            .field("tree", .dictionary, .required)
            .field("tasks", .array(of: .dictionary), .required)
            .create()

        try await database.schema("tree_test_study_observations")
            .id()
            .field("tree_test_study_id", .uuid, .required, .references("tree_test_studies", "id", onDelete: .cascade))
            .field("response", .array(of: .dictionary), .required)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("tree_test_study_observations").delete()
        try await database.schema("tree_test_studies").delete()
        try await database.schema("studies").delete()
        try await database.enum("study_kind").delete()
        try await database.schema("sessions").delete()
        try await database.schema("users").delete()
    }
}
