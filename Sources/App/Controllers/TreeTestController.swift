import Vapor
import Fluent

func clean(string: String) -> String {
    string.replacingOccurrences(of: " ", with: "-")
     .filter { char in
        char.isLetter || char.isNumber
     }
}

fileprivate extension Request {
    func getStudy() async throws -> Study {
        let user = self.parameters.get("user")!
        guard let user = try await User.query(on: self.db).filter(\.$username == user).first() else {
            throw Abort(.notFound)
        }
        let uid = user.id!
        let slug = self.parameters.get("slug")!
        guard let study = try await Study.query(on: self.db)
            .filter(\.$user.$id == uid)
            .filter(\.$slug == slug)
            .filter(\.$kind == .treeTest)
            .with(\.$user)
            .with(\.$treeTestStudy)
            .first() else {
                throw Abort(.notFound)
            }

        return study
    }
}

final class TreeTestController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tests = routes.grouped("tree-tests")

        // CRUD
        tests.post(":user", use: create)
        tests.get(":user", ":slug", use: getByID)
        tests.patch(":user", ":slug", use: updateByID)

        // get study results
        tests.get(":user", ":slug", "results", use: results)

        // submit an observation (must be published)
        tests.post(":user", ":slug", "completed", use: submitObservation)
    }
    struct TreeTestStudyData: Content {
        var studyData: StudyData
        var tree: TreeNode<TreeStudyItem>
        var tasks: [TreeStudyTask]
    }
    func getByID(req: Request) async throws -> TreeTestStudyData {
        let study = try await req.getStudy()
        let user: User? = req.auth.get()

        if user?.id != study.user.id {
            guard study.published else {
                throw Abort(.forbidden)
            }
            guard study.password == req.headers["StudyPassword"][safe: 0] else {
                throw Abort(.unauthorized)
            }
        }

        return TreeTestStudyData(
            studyData: study.toData(),
            tree: study.treeTestStudy!.tree,
            tasks: study.treeTestStudy!.tasks
        )
    }
    struct UpdateRequest: Content {
        var tree: TreeNode<TreeStudyItem>
        var tasks: [TreeStudyTask]
    }
    func updateByID(req: Request) async throws -> HTTPStatus {
        let user: User = try req.auth.require()

        let request = try req.content.decode(UpdateRequest.self)
        let study = try await req.getStudy()
        guard user.id == study.user.id else {
            throw Abort(.forbidden)
        }

        for task in request.tasks {
            guard request.tree.contains(id: task.answer) else {
                throw Abort(.badRequest)
            }
        }

        study.treeTestStudy!.tree = request.tree
        study.treeTestStudy!.tasks = request.tasks

        try await study.treeTestStudy!.save(on: req.db)

        return .ok
    }
    struct ResultsResponse: Content {
        var observations: [TreeTestObservationData]
    }
    func results(req: Request) async throws -> ResultsResponse {
        let user: User = try req.auth.require()
        let study = try await req.getStudy()
        guard user.id == study.user.id else {
            throw Abort(.forbidden)
        }

        try await study.treeTestStudy!.$observations.load(on: req.db)
        return ResultsResponse(
            observations: study.treeTestStudy!.observations.map
                { TreeTestObservationData(responses: $0.response) }
        )
    }
    struct NewStudyRequest: Content {
        var title: String
    }
    func create(req: Request) async throws -> String {
        let user: User = try req.auth.require()
        let request = try req.content.decode(NewStudyRequest.self)

        return try await req.db.transaction { db in
            let study = Study()
            study.$user.id = user.id!
            study.kind = .treeTest
            study.title = request.title
            study.published = false
            study.slug = clean(string: request.title)
            try await study.create(on: db)

            let treeTest = TreeTestStudy()
            treeTest.tree = TreeNode(
                id: "root",
                content: TreeStudyItem(text: "Root"),
                children: []
            )
            treeTest.tasks = []

            try await study.$treeTestStudy.create(treeTest, on: db)
            return study.slug
        }
    }
    struct SubmitRequest: Content {
        var result: TreeTestObservationData
    }
    func submitObservation(req: Request) async throws -> HTTPStatus {
        let study = try await req.getStudy()
        let treeTest = study.treeTestStudy!
        let request = try req.content.decode(SubmitRequest.self)

        guard study.published else {
            throw Abort(.forbidden)
        }
        guard study.password == req.headers["StudyPassword"][safe: 0] else {
            throw Abort(.unauthorized)
        }
        guard treeTest.tasks.count == request.result.responses.count else {
            throw Abort(.badRequest)
        }
        for (index, element) in request.result.responses.enumerated() {
            guard treeTest.tasks[index].id == element.taskID else {
                throw Abort(.badRequest)
            }
            guard treeTest.tree.contains(id: element.answer) else {
                throw Abort(.badRequest)
            }
        }

        let observation = TreeTestStudyObservation()
        observation.response = request.result.responses
        try await treeTest.$observations.create(observation, on: req.db)

        return .ok
    }
}
