import Fluent
import Vapor

struct DesirabilityStudyWord: Codable {
    var word: String
    var tags: [String]
}

struct DesirabilityStudyWordTag: Codable {
    var tag: String
    var description: String
}

struct DesirabilityStudyItem: Codable {
    var imageFileID: UUID
    var description: String
}

struct DesirabilityStudyData: Codable, Content {
    var studyData: StudyData
    var wordBank: [DesirabilityStudyWord]
    var wordTags: [DesirabilityStudyWordTag]
    var items: [DesirabilityStudyItem]
    var numberOfWordsToSelect: Int
}

final class DesirabilityStudy: Model {
    static let schema = "desirability_test_studies"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "study_id")
    var study: Study

    @Field(key: "word_bank")
    var wordBank: [DesirabilityStudyWord]

    @Field(key: "word_tags")
    var wordTags: [DesirabilityStudyWordTag]

    @Field(key: "items")
    var items: [DesirabilityStudyItem]

    @Field(key: "number_of_words_to_select")
    var numberOfWordsToSelect: Int

    @Children(for: \.$study)
    var observations: [DesirabilityStudyObservation]

    init() { }

    func toData(study: Study) -> DesirabilityStudyData {
        DesirabilityStudyData(
            studyData: study.toData(),
            wordBank: wordBank,
            wordTags: wordTags,
            items: items,
            numberOfWordsToSelect: numberOfWordsToSelect
        )
    }
}
