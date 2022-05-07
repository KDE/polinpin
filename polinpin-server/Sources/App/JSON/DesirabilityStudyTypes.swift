import Vapor

struct DesirabilityStudy: ReqEncodable {
    var name: String
    var words: [String]
    var images: [DesirabilityStudyImage]
}

struct DesirabilityStudyImage: ReqEncodable {
    var imageURL: String
}
