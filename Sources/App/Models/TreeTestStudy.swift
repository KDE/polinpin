import Fluent
import Vapor

struct TreeNode<T: Codable>: Codable {
    var id: String
    var content: T
    var children: [TreeNode<T>]
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
    @inlinable func filterCount(_ predicate: (Element) throws -> Bool) rethrows -> Int {
        return try filter(predicate).count
    }
}

extension TreeNode {
    func contains(id: String) -> Bool {
        return self.id == id || children.contains { $0.contains(id: id) }
    }
    func find(id: String) -> TreeNode? {
        func nodeStepper<T>(akku: TreeNode<T>?, item: TreeNode<T>) -> TreeNode<T>? {
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

struct TreeStudyItem: Codable {
    var text: String
}

struct TreeStudyTask: Codable {
    var id: String
    var text: String
    var answer: String
}

final class TreeTestStudy: Model {
    static let schema = "tree_test_studies"

    @ID(key: .id)
    var id: UUID?

    @Parent(key: "study_id")
    var study: Study

    @Field(key: "tree")
    var tree: TreeNode<TreeStudyItem>

    @Field(key: "tasks")
    var tasks: [TreeStudyTask]

    @Children(for: \.$study)
    var observations: [TreeTestStudyObservation]

    init() { }
}
