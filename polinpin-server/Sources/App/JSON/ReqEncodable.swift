import Vapor

protocol ReqEncodable: Content {
}
extension ReqEncodable {
    public func encodeResponse(for request: Request) async throws -> Response {
        let jsonData = try! JSONEncoder().encode(self)

        var headers = HTTPHeaders()
        headers.add(name: .contentType, value: "application/json")

        return Response(
            status: .ok,
            headers: headers,
            body: .init(data: jsonData)
        )
    }
    static func decodeRequest(_ request: Request) async throws -> Self {
        guard let data = request.body.data else {
            throw Abort(.badRequest)
        }
        guard let decoded = try? JSONDecoder().decode(Self.self, from: data) else {
            throw Abort(.badRequest)
        }
        return decoded
    }
}
