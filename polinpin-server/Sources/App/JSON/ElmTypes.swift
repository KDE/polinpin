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

extension Node {
    func contains(id: String) -> Bool {
        return self.id == id || children.contains { $0.contains(id: id) }
    }
    func find(id: String) -> Node? {
        func nodeStepper<T>(akku: Node<T>?, item: Node<T>) -> Node<T>? {
            if akku != nil {
                return akku
            }

            if item.id == id {
                return item
            }

            return item.children.foldl(base: akku, transform: nodeStepper)
        }

        return [self].foldl(base: nil, transform: nodeStepper)
    }
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

struct ObservationPoint: ReqEncodable {
    var question: AnsweredTask
    var pastSelections: [PastSelection]
    var startedAt: Int
    var endedAt: Int
}

extension ObservationPoint {
    var timeTaken : TimeInterval {
        let firstDate = Date(timeIntervalSince1970: Double(startedAt))
        let lastDate = Date(timeIntervalSince1970: Double(endedAt))

        return lastDate.timeIntervalSince(firstDate)
    }

    func direct(tree: Node<Item>) -> Bool {
        var node = tree

        for item in pastSelections {
            if node.contains(id: item.node) {
                node = node.find(id: item.node)!
            } else {
                return false
            }
        }

        return true
    }
}

struct Observation: ReqEncodable {
    var observations: [ObservationPoint]
}

extension Observation {
    var timeTaken : TimeInterval? {
        guard let first = observations.first else { return nil }
        let firstDate = Date(timeIntervalSince1970: Double(first.startedAt))
        guard let last = observations.last else {return nil }
        let lastDate = Date(timeIntervalSince1970: Double(last.endedAt))

        return lastDate.timeIntervalSince(firstDate)
    }
}

extension Array {
    func percent(_ predicate: (Element) -> Bool) -> Float {
        return Float(self.filter(predicate).count) / Float(self.count)
    }
    func foldl<E>(base: E, transform: (E, Element) -> E) -> E {
        var result = base
        for item in self {
            result = transform(result, item)
        }
        return result
    }
}

extension Array where Element == Double {
    var median: Double {
        let sortedArray = sorted()
        if count % 2 != 0 {
            return Double(sortedArray[safe: count / 2] ?? 0.0)
        } else {
            return Double((sortedArray[safe: count / 2] ?? 0.0) + (sortedArray[safe: count / 2 - 1] ?? 0.0)) / 2.0
        }
    }
}

extension Array where Element: OptionalType {
    func mapOptional() -> [Element.WrappedType]? {
        var new = Array<Element.WrappedType>()

        for item in self {
            if item.wrapped == nil {
                return nil
            } else {
                new.append(item.wrapped!)
            }
        }

        return new
    }
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

struct TreeTestStatistics: ReqEncodable {
    var medianTime: Double
    var minimumTime: Double
    var maximumTime: Double

    // amount of answers chosen correctly
    var percentCorrect: Float

    // amount of answers chosen without backtracking
    var percentDirect: Float

    init?(for observations: [Observation], with tree: Node<Item>) {
        percentCorrect = observations.flatMap { $0.observations }.percent { $0.question.answer == $0.question.task.correctAnswer }
        guard let times = observations.map({ $0.timeTaken }).mapOptional() else {
            return nil
        }

        medianTime = times.median
        guard let min = times.min() else { return nil }
        minimumTime = min 
        guard let max = times.max() else { return nil }
        maximumTime = max

        percentDirect = observations.flatMap { $0.observations }.percent { $0.direct(tree: tree) }
    }
}
