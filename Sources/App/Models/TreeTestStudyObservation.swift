import Fluent
import Vapor

struct TreeTestPastSelection: Codable {
    var selectedID: String
    var selectedAt: Int
}

struct TreeTestAnsweredQuestion: Codable {
    var taskID: String
    var answer: String
    var pastSelections: [TreeTestPastSelection]

    var startedAt: Int
    var endedAt: Int
}

struct TreeTestObservationData: Codable {
    var responses: [TreeTestAnsweredQuestion]
}

final class TreeTestStudyObservation: Model {
    static let schema = "tree_test_study_observations"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "tree_test_study_id")
    var study: TreeTestStudy

    @Field(key: "response")
    var response: [TreeTestAnsweredQuestion]

    init() { }
}
