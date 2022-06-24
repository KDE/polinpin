import Fluent
import FluentSQLiteDriver
import FluentMySQLDriver
import FluentPostgresDriver
import Leaf
import Vapor

struct DatabaseConfiguration: Codable {
    enum Kind: Codable {
        case sqlite(file: String)
        case mysql(hostname: String, port: Int, username: String, password: String, database: String)
        case postgres(hostname: String, port: Int, username: String, password: String, database: String)
        case mysqlURL(url: String)
    }
    var settings: Kind
}

let dbConfig = try! JSONDecoder().decode(DatabaseConfiguration.self, from: try! String(contentsOfFile: "Polinpin Database Configuration.json").data(using: .utf8)!)

// configures your application
public func configure(_ app: Application) throws {
    let corsConfiguration = CORSMiddleware.Configuration(
        allowedOrigin: .all,
        allowedMethods: [.GET, .POST, .PUT, .OPTIONS, .DELETE, .PATCH],
        allowedHeaders: [.accept, .authorization, .contentType, .origin, .xRequestedWith, .userAgent, .accessControlAllowOrigin]
    )
    let cors = CORSMiddleware(configuration: corsConfiguration)
    // cors middleware should come before default error middleware using `at: .beginning`
    app.middleware.use(cors, at: .beginning)

    switch dbConfig.settings {
    case .sqlite(let file):
        app.databases.use(.sqlite(.file(file)), as: .sqlite)
    case .mysql(let hostname, let port, let username, let password, let database):
        app.databases.use(.mysql(
            hostname: hostname,
            port: port,
            username: username,
            password: password,
            database: database
        ), as: .mysql)
    case .postgres(let hostname, let port, let username, let password, let database):
        app.databases.use(.postgres(
            hostname: hostname,
            port: port,
            username: username,
            password: password,
            database: database
        ), as: .psql)
    case .mysqlURL(let url):
        app.databases.use(try .mysql(url: url), as: .sqlite)
        break
    }

    app.migrations.add(InitialMigration())
    app.migrations.add(DesirabilityFilesMigration())
    app.migrations.add(OAuthMigration())

    app.views.use(.leaf)

    // register routes
    try routes(app)
}
