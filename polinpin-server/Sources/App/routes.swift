import Fluent
import Vapor

func randomString(length: Int) -> String {
    let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    return String((0..<length).map{ _ in letters.randomElement()! })
}

func clean(string: String) -> String {
    string.replacingOccurrences(of: " ", with: "-")
     .filter { char in
        char.isLetter || char.isNumber
     }
}

func routes(_ app: Application) throws {
    app.group("viewer") { viewer in
        viewer.get("tree-test", ":id") { req -> Study in
            guard let slug = req.parameters.get("id") else {
                throw Abort(.badRequest)
            }
            guard let treeTest = try await StudyModel.query(on: req.db)
                .filter(\.$slug == slug)
                .first() else {
                    throw Abort(.notFound)
                }

            return treeTest.study
        }
    }
    app.group("completed") { completed in
        completed.post("tree-test", ":id") { req -> String in
            let observation = try req.content.decode(Observation.self)

            guard let slug = req.parameters.get("id") else {
                throw Abort(.badRequest)
            }
            guard let treeTest = try await StudyModel.query(on: req.db)
                .filter(\.$slug == slug)
                .first() else {
                    throw Abort(.notFound)
                }

            try await ObservationModel(id: nil, belongsTo: treeTest, observation: observation)
                .create(on: req.db)

            return "ok"
        }
    }
    app.post("login") { req -> UserSession in
        let request = try req.content.decode(LoginRequest.self)

        guard let user = try await User.query(on: req.db)
            .filter(\.$username == request.username)
            .first() else {
                throw Abort(.notFound)
            }

        let password = try await req.password.async.verify(request.password, created: user.password)
        if !password {
            throw Abort(.unauthorized)
        }

        let session = Session(id: nil, belongsTo: user)
        try await session.create(on: req.db)

        return UserSession(name: user.name, token: session.id!.uuidString)
    }
    app.post("register") { req async throws -> UserSession in
        let request = try req.content.decode(RegisterRequest.self)
        let user = User(
            id: nil,
            name: request.name,
            username: request.username,
            password: try await req.password.async.hash(request.password)
        )

        return try await req.db.transaction { db in
            try await user.create(on: db)

            let session = Session(id: nil, belongsTo: user)
            try await session.create(on: db)

            return UserSession(name: user.name, token: session.id!.uuidString)
        }
    }
    app.grouped(UserAuthenticator()).group("my") { my in
        my.group("tree-tests") { treeTests in
            treeTests.get { req -> MyTreeTestsResponse in
                let user = try req.auth.require(User.self)
                try await user.$studies.load(on: req.db)
                return MyTreeTestsResponse(tests: user.studies.map { study in
                    TreeTestHeader(name: study.name, id: study.slug)
                })
            }
            treeTests.get(":id", "statistics") { req -> TreeTestStatistics in
                let user = try req.auth.require(User.self)
                guard let slug = req.parameters.get("id") else {
                    throw Abort(.badRequest)
                }
                guard let treeTest = try await StudyModel.query(on: req.db)
                    .filter(\.$slug == slug)
                    .with(\.$observations)
                    .first() else {
                        throw Abort(.notFound)
                    }
                if treeTest.$user.id != user.id {
                    throw Abort(.forbidden)
                }

                if treeTest.observations.count <= 2 {
                    throw Abort(.imATeapot)
                }

                guard let ret = TreeTestStatistics(for: treeTest.observations.map { $0.observation }, with: treeTest.study.tree) else {
                    throw Abort(.internalServerError)
                }

                return ret
            }
            treeTests.delete(":id") { req -> String in
                let user = try req.auth.require(User.self)
                guard let slug = req.parameters.get("id") else {
                    throw Abort(.badRequest)
                }
                guard let treeTest = try await StudyModel.query(on: req.db)
                    .filter(\.$slug == slug)
                    .first() else {
                        throw Abort(.notFound)
                    }
                if treeTest.$user.id != user.id {
                    throw Abort(.forbidden)
                }
                try await treeTest.delete(on: req.db)
                return ""
            }
            treeTests.patch(":id") { req -> String in
                let user = try req.auth.require(User.self)
                let newStudy = try req.content.decode(Study.self)

                guard let slug = req.parameters.get("id") else {
                    throw Abort(.badRequest)
                }
                guard let treeTest = try await StudyModel.query(on: req.db)
                    .filter(\.$slug == slug)
                    .first() else {
                        throw Abort(.notFound)
                    }

                if treeTest.$user.id != user.id {
                    throw Abort(.forbidden)
                }

                treeTest.study = newStudy
                try await treeTest.update(on: req.db)

                return "ok!"
            }
            treeTests.post { req -> String in
                let request = try req.content.decode(CreateTreeTestRequest.self)
                let user = try req.auth.require(User.self)

                let study = Study(
                    tree: Node(
                        id: "root",
                        content:
                        Item(text: "Root"),
                        children: []
                    ),
                    name: request.name,
                    tasks: []
                )

                let slug = clean(string: request.name) + "-" + randomString(length: 6)

                let studyModel = StudyModel(
                    id: nil,
                    study: study,
                    name: request.name,
                    slug: slug,
                    belongsTo: user
                )

                try await studyModel.create(on: req.db)
                return "\"\(slug)\""
            }
        }
    }
}
