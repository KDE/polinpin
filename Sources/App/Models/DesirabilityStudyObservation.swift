import Fluent
import Vapor

// each one of these corresponds to an item in the DesirabilityStudy; which words a user chose for it
struct DesirabilityStudyWordResponse: Codable {
    var words: [String]
    var rating: Int // seven-point semantic differential scale
}

struct DesirabilityStudyObservationData: Codable {
    var responses: [DesirabilityStudyWordResponse]
}

// each individual observation corresponds to one submitted completion from a user
final class DesirabilityStudyObservation: Model {
    static let schema = "desirability_test_study_observations"

    @ID(key: .id)
    var id: UUID?

    @Field(key: "response")
    var responses: [DesirabilityStudyWordResponse]

    @Parent(key: "desirability_study_id")
    var study: DesirabilityStudy

    init() { }
}
