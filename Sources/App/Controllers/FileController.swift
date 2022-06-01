import Vapor
import Fluent

final class FileController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let files = routes.grouped("files")
        files.on(.POST, "upload", body: .stream, use: upload)
    }
    func upload(req: Request) async throws -> String {
        let user: User = try req.auth.require()

        guard let data = try await req.body.collect(max: 1 << 24).get() else {
            throw Abort(.badRequest)
        }
        guard let mimetype = req.headers.contentType else {
            throw Abort(.badRequest)
        }

        return try await req.db.transaction { db in
            let file = DBFile()
            file.mimetype = mimetype.serialize()
            try await user.$files.create(file, on: db)

            try await req.fileio.writeFile(data, at: "./Uploads/" + file.id!.uuidString)

            return file.id!.uuidString
        }
    }
}
