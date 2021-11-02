import Fluent
import Vapor

final class ObservationModel: Model {
    static let schema = "observations"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "study_id")
    var study: StudyModel

    @Field(key: "observation")
    var observation: Observation

    init() { }

    init(id: UUID? = nil, belongsTo study: StudyModel, observation: Observation) {
        self.id = id
        self.$study.id = study.id!
        self.observation = observation
    }
}
