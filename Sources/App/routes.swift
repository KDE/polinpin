import Fluent
import Vapor

func routes(_ app: Application) throws {
    app.middleware.use(UserAuthenticator())
    try app.register(collection: UserController())
    try app.register(collection: TreeTestController())
}
