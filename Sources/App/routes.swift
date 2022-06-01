import Fluent
import Vapor

func routes(_ app: Application) throws {
    app.middleware.use(UserAuthenticator())
    try app.register(collection: UserController())
    try app.register(collection: TreeTestController())
    try app.register(collection: DesirabilityTestController())
    try app.register(collection: StudyController())
    try app.register(collection: FileController())

    app.get("uploads", "**") { req -> EventLoopFuture<Response> in
        guard var path = req.parameters.getCatchall().joined(separator: "/").removingPercentEncoding else {
            return req.eventLoop.makeFailedFuture(Abort(.badRequest))
        }

        // path must be relative.
        path = path.removeLeadingSlashes()

        // protect against relative paths
        guard !path.contains("../") else {
            return req.eventLoop.makeFailedFuture(Abort(.forbidden))
        }

        // create absolute path
        let absPath = "./Uploads/" + path

        // check if path exists and whether it is a directory
        var isDir: ObjCBool = false
        guard FileManager.default.fileExists(atPath: absPath, isDirectory: &isDir) else {
            return req.eventLoop.makeFailedFuture(Abort(.notFound))
        }
        
        if isDir.boolValue {
            return req.eventLoop.makeFailedFuture(Abort(.forbidden))
        }
        
        // stream the file
        let res = req.fileio.streamFile(at: absPath)
        return req.eventLoop.makeSucceededFuture(res)
    }
}

fileprivate extension String {
    /// Determines if input path is absolute based on a leading slash
    func isAbsolute() -> Bool {
        return self.hasPrefix("/")
    }

    /// Makes a path relative by removing all leading slashes
    func removeLeadingSlashes() -> String {
        var newPath = self
        while newPath.hasPrefix("/") {
            newPath.removeFirst()
        }
        return newPath
    }

    /// Adds a trailing slash to the path if one is not already present
    func addTrailingSlash() -> String {
        var newPath = self
        if !newPath.hasSuffix("/") {
            newPath += "/"
        }
        return newPath
    }
}
