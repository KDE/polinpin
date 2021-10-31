import App
import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)

let app = Application(env)
app.http.server.configuration.port = 25727
switch app.environment {
case .testing:
    app.passwords.use(.plaintext)
default: break
}

let corsConfiguration = CORSMiddleware.Configuration(
    allowedOrigin: .all,
    allowedMethods: [.GET, .POST, .PUT, .OPTIONS, .DELETE, .PATCH],
    allowedHeaders: [.accept, .authorization, .contentType, .origin, .xRequestedWith, .userAgent, .accessControlAllowOrigin]
)

let cors = CORSMiddleware(configuration: corsConfiguration)
app.middleware.use(cors, at: .beginning)

defer { app.shutdown() }

try configure(app)
try app.run()
