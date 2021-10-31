import Vapor

protocol ReqEncodable: Content {
}
extension ReqEncodable {
    public func encodeResponse(for request: Request) async throws -> Response {
        let jsonData = try! JSONEncoder().encode(self)

        var headers = HTTPHeaders()
        headers.add(name: .contentType, value: "application/json")

        return Response(
            status: .ok,
            headers: headers,
            body: .init(data: jsonData)
        )
    }
    static func decodeRequest(_ request: Request) async throws -> Self {
        guard let data = request.body.data else {
            throw Abort(.badRequest)
        }
        guard let decoded = try? JSONDecoder().decode(Self.self, from: data) else {
            throw Abort(.badRequest)
        }
        return decoded
    }
}

struct Node<T: Codable>: ReqEncodable {
    var id: String
    var content: T
    var children: [Node<T>]
}

struct Item: ReqEncodable {
    var text: String
}

struct Study: ReqEncodable {
    var tree: Node<Item>
    var name: String
    var tasks: [Task]
}

struct Task: ReqEncodable {
    var text: String
    var correctAnswer: String
}

struct AnsweredTask: ReqEncodable {
    var task: Task
    var answer: String
}

struct PastSelection: ReqEncodable {
    var node: String
    var at: Int
}

struct TreeTestHeader: ReqEncodable {
    var name: String
    var id: String
}

struct Observation: ReqEncodable {
    var question: AnsweredTask
    var pastSelections: [PastSelection]
    var startedAt: Int
    var endedAt: Int
}

struct UserSession: ReqEncodable {
    var name: String
    var token: String
}

struct LoginRequest: ReqEncodable {
    var username: String
    var password: String
}

struct RegisterRequest: ReqEncodable {
    var name: String
    var username: String
    var password: String
}

struct CreateTreeTestRequest: ReqEncodable {
    var name: String
}

struct MyTreeTestsResponse: ReqEncodable {
    var tests: [TreeTestHeader]
}