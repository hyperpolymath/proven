// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// SameSite cookie attribute values.
public enum SameSite: String {
    case strict = "Strict"
    case lax = "Lax"
    case none = "None"
}

/// HTTP Cookie with validation.
public struct Cookie: Equatable {
    public let name: String
    public let value: String
    public let domain: String?
    public let path: String?
    public let expires: Date?
    public let maxAge: Int?
    public let secure: Bool
    public let httpOnly: Bool
    public let sameSite: SameSite?

    public init?(
        name: String,
        value: String,
        domain: String? = nil,
        path: String? = nil,
        expires: Date? = nil,
        maxAge: Int? = nil,
        secure: Bool = false,
        httpOnly: Bool = false,
        sameSite: SameSite? = nil
    ) {
        // Validate name (no control chars, spaces, or separators)
        guard !name.isEmpty else { return nil }
        let invalidNameChars = CharacterSet(charactersIn: "()<>@,;:\\\"/[]?={} \t")
        guard name.unicodeScalars.allSatisfy({ !invalidNameChars.contains($0) && $0.value >= 33 && $0.value <= 126 }) else {
            return nil
        }

        // Validate value (no control chars, semicolons, or quotes at boundaries)
        for scalar in value.unicodeScalars {
            if scalar.value < 33 || scalar.value == 127 || scalar == ";" || scalar == "\\" {
                return nil
            }
        }

        self.name = name
        self.value = value
        self.domain = domain
        self.path = path
        self.expires = expires
        self.maxAge = maxAge
        self.secure = secure
        self.httpOnly = httpOnly
        self.sameSite = sameSite
    }

    /// Check if cookie is expired.
    public var isExpired: Bool {
        if let expires = expires {
            return expires < Date()
        }
        return false
    }

    /// Check if cookie is session cookie.
    public var isSession: Bool {
        expires == nil && maxAge == nil
    }
}

/// Cookie utilities.
public enum SafeCookie {
    /// Create a secure cookie with sensible defaults.
    public static func createSecure(
        name: String,
        value: String,
        domain: String? = nil,
        path: String? = "/",
        maxAge: Int? = nil,
        sameSite: SameSite = .strict
    ) -> Cookie? {
        Cookie(
            name: name,
            value: value,
            domain: domain,
            path: path,
            maxAge: maxAge,
            secure: true,
            httpOnly: true,
            sameSite: sameSite
        )
    }

    /// Create a session cookie.
    public static func createSession(
        name: String,
        value: String,
        domain: String? = nil,
        path: String? = "/"
    ) -> Cookie? {
        Cookie(
            name: name,
            value: value,
            domain: domain,
            path: path,
            secure: true,
            httpOnly: true,
            sameSite: .strict
        )
    }

    /// Create an expired cookie (for deletion).
    public static func createExpired(name: String, domain: String? = nil, path: String? = "/") -> Cookie? {
        Cookie(
            name: name,
            value: "",
            domain: domain,
            path: path,
            expires: Date(timeIntervalSince1970: 0),
            maxAge: 0
        )
    }

    /// Parse Set-Cookie header.
    public static func parse(_ setCookieHeader: String) -> Cookie? {
        let parts = setCookieHeader.components(separatedBy: ";").map { $0.trimmingCharacters(in: .whitespaces) }

        guard let firstPart = parts.first,
              let equalsIndex = firstPart.firstIndex(of: "=") else { return nil }

        let name = String(firstPart[..<equalsIndex])
        let value = String(firstPart[firstPart.index(after: equalsIndex)...])

        var domain: String?
        var path: String?
        var expires: Date?
        var maxAge: Int?
        var secure = false
        var httpOnly = false
        var sameSite: SameSite?

        for part in parts.dropFirst() {
            let lowercased = part.lowercased()

            if lowercased == "secure" {
                secure = true
            } else if lowercased == "httponly" {
                httpOnly = true
            } else if lowercased.hasPrefix("domain=") {
                domain = String(part.dropFirst("domain=".count))
            } else if lowercased.hasPrefix("path=") {
                path = String(part.dropFirst("path=".count))
            } else if lowercased.hasPrefix("max-age=") {
                maxAge = Int(part.dropFirst("max-age=".count))
            } else if lowercased.hasPrefix("expires=") {
                let dateString = String(part.dropFirst("expires=".count))
                expires = parseHTTPDate(dateString)
            } else if lowercased.hasPrefix("samesite=") {
                let value = String(part.dropFirst("samesite=".count)).lowercased()
                switch value {
                case "strict": sameSite = .strict
                case "lax": sameSite = .lax
                case "none": sameSite = .none
                default: break
                }
            }
        }

        return Cookie(
            name: name,
            value: value,
            domain: domain,
            path: path,
            expires: expires,
            maxAge: maxAge,
            secure: secure,
            httpOnly: httpOnly,
            sameSite: sameSite
        )
    }

    /// Format cookie for Set-Cookie header.
    public static func format(_ cookie: Cookie) -> String {
        var parts: [String] = ["\(cookie.name)=\(cookie.value)"]

        if let domain = cookie.domain {
            parts.append("Domain=\(domain)")
        }

        if let path = cookie.path {
            parts.append("Path=\(path)")
        }

        if let maxAge = cookie.maxAge {
            parts.append("Max-Age=\(maxAge)")
        }

        if let expires = cookie.expires {
            let formatter = DateFormatter()
            formatter.dateFormat = "EEE, dd MMM yyyy HH:mm:ss 'GMT'"
            formatter.timeZone = TimeZone(identifier: "GMT")
            formatter.locale = Locale(identifier: "en_US_POSIX")
            parts.append("Expires=\(formatter.string(from: expires))")
        }

        if cookie.secure {
            parts.append("Secure")
        }

        if cookie.httpOnly {
            parts.append("HttpOnly")
        }

        if let sameSite = cookie.sameSite {
            parts.append("SameSite=\(sameSite.rawValue)")
        }

        return parts.joined(separator: "; ")
    }

    /// Parse Cookie header (name=value pairs).
    public static func parseRequestCookies(_ cookieHeader: String) -> [String: String] {
        var cookies: [String: String] = [:]

        for pair in cookieHeader.components(separatedBy: ";") {
            let trimmed = pair.trimmingCharacters(in: .whitespaces)
            if let equalsIndex = trimmed.firstIndex(of: "=") {
                let name = String(trimmed[..<equalsIndex])
                let value = String(trimmed[trimmed.index(after: equalsIndex)...])
                cookies[name] = value
            }
        }

        return cookies
    }

    /// Format cookies for Cookie header.
    public static func formatRequestCookies(_ cookies: [String: String]) -> String {
        cookies.map { "\($0.key)=\($0.value)" }.joined(separator: "; ")
    }

    // MARK: - Private Helpers

    private static func parseHTTPDate(_ string: String) -> Date? {
        let formatters = [
            "EEE, dd MMM yyyy HH:mm:ss zzz",
            "EEEE, dd-MMM-yy HH:mm:ss zzz",
            "EEE MMM d HH:mm:ss yyyy"
        ]

        for format in formatters {
            let formatter = DateFormatter()
            formatter.dateFormat = format
            formatter.locale = Locale(identifier: "en_US_POSIX")
            if let date = formatter.date(from: string) {
                return date
            }
        }

        return nil
    }
}
