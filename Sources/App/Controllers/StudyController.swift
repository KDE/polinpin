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
            .with(\.$user)
            .first() else {
                throw Abort(.notFound)
            }

        return study
    }
}

final class StudyController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tests = routes.grouped("studies")

        // CRUD
        tests.get("my", use: my)
        tests.delete(":user", ":slug", use: deleteByID)
        tests.post(":user", ":slug", "publish", use: publish)
    }
    struct MyResponse: Content {
        var studies: [StudyData]
    }
    func my(req: Request) async throws -> MyResponse {
        let user: User = try req.auth.require()

        let study = try await Study.query(on: req.db)
            .filter(\.$user.$id == user.id!)
            .all()

        return MyResponse(studies: study.map { $0.toData() })
    }
    func deleteByID(req: Request) async throws -> HTTPStatus {
        let user: User = try req.auth.require()
        let study = try await req.getStudy()
        guard user.id == study.user.id else {
            throw Abort(.forbidden)
        }
        try await study.delete(on: req.db)
        return .ok
    }
    func publish(req: Request) async throws -> HTTPStatus {
        let user: User = try req.auth.require()
        let study = try await req.getStudy()
        guard user.id == study.user.id else {
            throw Abort(.forbidden)
        }

        study.published = true
        try await study.update(on: req.db)

        return .ok
    }
}
