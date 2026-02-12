// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// HTTP header name with validation.
public struct HeaderName: Equatable, Hashable {
    public let value: String

    public init?(_ value: String) {
        // Header names must be valid tokens (no special characters)
        let trimmed = value.trimmingCharacters(in: .whitespaces)
        guard !trimmed.isEmpty else { return nil }

        // RFC 7230: token = 1*tchar
        // tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
        //         "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
        let allowedChars = CharacterSet.alphanumerics.union(CharacterSet(charactersIn: "!#$%&'*+-.^_`|~"))
        guard trimmed.unicodeScalars.allSatisfy({ allowedChars.contains($0) }) else { return nil }

        self.value = trimmed
    }

    /// Canonical form (lowercase).
    public var canonical: String {
        value.lowercased()
    }
}

/// HTTP header value with validation.
public struct HeaderValue: Equatable, Hashable {
    public let value: String

    public init?(_ value: String) {
        // Header values must not contain control characters (except tab)
        let trimmed = value.trimmingCharacters(in: .whitespaces)

        for scalar in trimmed.unicodeScalars {
            if scalar.value < 32 && scalar.value != 9 { // Allow tab (9)
                return nil
            }
            if scalar.value == 127 { // DEL
                return nil
            }
        }

        self.value = trimmed
    }
}

/// HTTP header.
public struct Header: Equatable {
    public let name: HeaderName
    public let value: HeaderValue

    public init?(name: String, value: String) {
        guard let headerName = HeaderName(name),
              let headerValue = HeaderValue(value) else { return nil }
        self.name = headerName
        self.value = headerValue
    }
}

/// Header utilities.
public enum SafeHeader {
    // MARK: - Common Header Names

    public static let contentType = HeaderName("Content-Type")!
    public static let contentLength = HeaderName("Content-Length")!
    public static let contentEncoding = HeaderName("Content-Encoding")!
    public static let contentDisposition = HeaderName("Content-Disposition")!
    public static let cacheControl = HeaderName("Cache-Control")!
    public static let authorization = HeaderName("Authorization")!
    public static let accept = HeaderName("Accept")!
    public static let acceptLanguage = HeaderName("Accept-Language")!
    public static let acceptEncoding = HeaderName("Accept-Encoding")!
    public static let userAgent = HeaderName("User-Agent")!
    public static let host = HeaderName("Host")!
    public static let cookie = HeaderName("Cookie")!
    public static let setCookie = HeaderName("Set-Cookie")!
    public static let location = HeaderName("Location")!
    public static let referer = HeaderName("Referer")!
    public static let origin = HeaderName("Origin")!
    public static let xRequestedWith = HeaderName("X-Requested-With")!
    public static let xForwardedFor = HeaderName("X-Forwarded-For")!
    public static let xForwardedProto = HeaderName("X-Forwarded-Proto")!

    // MARK: - Security Headers

    public static let strictTransportSecurity = HeaderName("Strict-Transport-Security")!
    public static let contentSecurityPolicy = HeaderName("Content-Security-Policy")!
    public static let xContentTypeOptions = HeaderName("X-Content-Type-Options")!
    public static let xFrameOptions = HeaderName("X-Frame-Options")!
    public static let xXSSProtection = HeaderName("X-XSS-Protection")!
    public static let referrerPolicy = HeaderName("Referrer-Policy")!
    public static let permissionsPolicy = HeaderName("Permissions-Policy")!

    // MARK: - CORS Headers

    public static let accessControlAllowOrigin = HeaderName("Access-Control-Allow-Origin")!
    public static let accessControlAllowMethods = HeaderName("Access-Control-Allow-Methods")!
    public static let accessControlAllowHeaders = HeaderName("Access-Control-Allow-Headers")!
    public static let accessControlMaxAge = HeaderName("Access-Control-Max-Age")!
    public static let accessControlAllowCredentials = HeaderName("Access-Control-Allow-Credentials")!

    // MARK: - Header Builders

    /// Create Content-Type header.
    public static func createContentType(_ mimeType: String, charset: String? = nil) -> Header? {
        var value = mimeType
        if let charset = charset {
            value += "; charset=\(charset)"
        }
        return Header(name: "Content-Type", value: value)
    }

    /// Create Authorization header with Bearer token.
    public static func createBearerAuth(_ token: String) -> Header? {
        Header(name: "Authorization", value: "Bearer \(token)")
    }

    /// Create Authorization header with Basic auth.
    public static func createBasicAuth(username: String, password: String) -> Header? {
        let credentials = "\(username):\(password)"
        guard let data = credentials.data(using: .utf8) else { return nil }
        let base64 = data.base64EncodedString()
        return Header(name: "Authorization", value: "Basic \(base64)")
    }

    /// Create Cache-Control header.
    public static func createCacheControl(
        public: Bool = false,
        private: Bool = false,
        noCache: Bool = false,
        noStore: Bool = false,
        maxAge: Int? = nil,
        sMaxAge: Int? = nil,
        mustRevalidate: Bool = false
    ) -> Header? {
        var directives: [String] = []

        if `public` { directives.append("public") }
        if `private` { directives.append("private") }
        if noCache { directives.append("no-cache") }
        if noStore { directives.append("no-store") }
        if let age = maxAge { directives.append("max-age=\(age)") }
        if let age = sMaxAge { directives.append("s-maxage=\(age)") }
        if mustRevalidate { directives.append("must-revalidate") }

        guard !directives.isEmpty else { return nil }
        return Header(name: "Cache-Control", value: directives.joined(separator: ", "))
    }

    /// Create HSTS header.
    public static func createHSTS(maxAge: Int, includeSubdomains: Bool = false, preload: Bool = false) -> Header? {
        var value = "max-age=\(maxAge)"
        if includeSubdomains { value += "; includeSubDomains" }
        if preload { value += "; preload" }
        return Header(name: "Strict-Transport-Security", value: value)
    }

    /// Create CSP header.
    public static func createCSP(directives: [String: String]) -> Header? {
        let value = directives.map { "\($0.key) \($0.value)" }.joined(separator: "; ")
        return Header(name: "Content-Security-Policy", value: value)
    }

    /// Create CORS Allow-Origin header.
    public static func createCORSOrigin(_ origin: String) -> Header? {
        Header(name: "Access-Control-Allow-Origin", value: origin)
    }

    /// Create X-Frame-Options header.
    public static func createXFrameOptions(_ option: XFrameOption) -> Header? {
        Header(name: "X-Frame-Options", value: option.rawValue)
    }

    /// Parse header string "Name: Value".
    public static func parse(_ headerString: String) -> Header? {
        guard let colonIndex = headerString.firstIndex(of: ":") else { return nil }

        let name = String(headerString[..<colonIndex])
        let value = String(headerString[headerString.index(after: colonIndex)...])
            .trimmingCharacters(in: .whitespaces)

        return Header(name: name, value: value)
    }

    /// Format header as string.
    public static func format(_ header: Header) -> String {
        "\(header.name.value): \(header.value.value)"
    }
}

/// X-Frame-Options values.
public enum XFrameOption: String {
    case deny = "DENY"
    case sameOrigin = "SAMEORIGIN"
}
