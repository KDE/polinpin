import App
import Vapor

struct AppConfiguration: Codable {
    var port: Int = 25727
}

let appConfig = try! JSONDecoder().decode(AppConfiguration.self, from: try! String(contentsOfFile: "Polinpin Configuration.json").data(using: .utf8)!)
var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)
let app = Application(env)
app.http.server.configuration.port = appConfig.port
defer { app.shutdown() }
try configure(app)
try app.run()
