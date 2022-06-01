import Vapor
import Fluent

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
            .filter(\.$kind == .desirabilityTest)
            .with(\.$user)
            .with(\.$desirabilityStudy)
            .first() else {
                throw Abort(.notFound)
            }

        return study
    }
}

final class DesirabilityTestController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tests = routes.grouped("desirability-studies")

        // CRUD
        tests.post(":user", use: create)
        tests.get(":user", ":slug", use: getByID)
        tests.patch(":user", ":slug", use: updateByID)

        // get study results
        tests.get(":user", ":slug", "results", use: results)

        // submit an observation (must be published)
        tests.post(":user", ":slug", "completed", use: submitObservation)
    }
    func getByID(req: Request) async throws -> DesirabilityStudyData {
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

        return study.desirabilityStudy!.toData(study: study)
    }
    struct UpdateRequest: Content {
        var wordBank: [DesirabilityStudyWord]
        var wordTags: [DesirabilityStudyWordTag]
        var items: [DesirabilityStudyItem]
        var numberOfWordsToSelect: Int
    }
    func updateByID(req: Request) async throws -> HTTPStatus {
        let user: User = try req.auth.require()
        let study = try await req.getStudy()
        let desirability = study.desirabilityStudy!
        let update = try req.content.decode(UpdateRequest.self)

        guard user.id == study.user.id else {
            throw Abort(.forbidden)
        }

        guard update.numberOfWordsToSelect >= 1 else {
            throw Abort(.badRequest)
        }

        let tags = Set(update.wordTags.map { $0.tag })

        for item in update.wordBank {
            for tag in item.tags {
                guard tags.contains(tag) else {
                    throw Abort(.badRequest)
                }
            }
        }

        desirability.wordBank = update.wordBank
        desirability.wordTags = update.wordTags
        desirability.items = update.items
        desirability.numberOfWordsToSelect = update.numberOfWordsToSelect

        try await desirability.save(on: req.db)
        return .ok
    }
    struct ResultsResponse: Content {
        var observations: [DesirabilityStudyObservationData]
    }
    func results(req: Request) async throws -> ResultsResponse {
        let user: User = try req.auth.require()
        let study = try await req.getStudy()

        guard user.id == study.user.id else {
            throw Abort(.forbidden)
        }

        try await study.desirabilityStudy!.$observations.load(on: req.db)
        return ResultsResponse(
            observations: study.desirabilityStudy!.observations.map { DesirabilityStudyObservationData(responses: $0.responses) }
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
            study.kind = .desirabilityTest
            study.title = request.title
            study.published = false
            study.slug = clean(string: request.title)
            try await study.create(on: db)

            let desirabilityStudy = DesirabilityStudy()
            desirabilityStudy.wordTags = [
                DesirabilityStudyWordTag(tag: "Positive", description: "A word that describes positive feelings"),
                DesirabilityStudyWordTag(tag: "Negative", description: "A word that describes negative feelings"),
            ]
            desirabilityStudy.wordBank = [
                DesirabilityStudyWord(word: "Advanced", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Boring", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Busy", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Calm", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Cheap", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Creative", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Cutting-edge", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Dated", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Distracting", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Dull", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Exciting", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Expensive", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Familiar", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Fresh", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Impressive", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Innovative", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Inspiring", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Intimidating", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Old", tags: ["Negative"]),
                DesirabilityStudyWord(word: "Professional", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Simplistic", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Trustworthy", tags: ["Positive"]),
                DesirabilityStudyWord(word: "Unprofessional", tags: ["Negative"]),
            ]
            desirabilityStudy.items = []
            desirabilityStudy.numberOfWordsToSelect = 5

            try await study.$desirabilityStudy.create(desirabilityStudy, on: db)
            return study.slug
        }
    }
    struct SubmitRequest: Content {
        var responses: [DesirabilityStudyWordResponse]
    }
    func submitObservation(req: Request) async throws -> HTTPStatus {
        let study = try await req.getStudy()
        let submit = try req.content.decode(SubmitRequest.self)

        let observation = DesirabilityStudyObservation()
        observation.responses = submit.responses

        let words = Set(study.desirabilityStudy!.wordBank.map { $0.word })

        for response in submit.responses {
            guard response.words.count == study.desirabilityStudy!.numberOfWordsToSelect else {
                throw Abort(.badRequest)
            }
            guard 1 <= response.rating && response.rating <= 7 else {
                throw Abort(.badRequest)
            }
            for word in response.words {
                guard words.contains(word) else {
                    throw Abort(.badRequest)
                }
            }
        }

        try await study.desirabilityStudy!.$observations.create(observation, on: req.db)

        return .ok
    }
}
