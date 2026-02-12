// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe URL parsing and validation operations.
public enum SafeUrl {
    /// Represents the parsed components of a URL.
    public struct ParsedUrl: Equatable {
        public let scheme: String
        public let host: String
        public let port: Int?
        public let path: String
        public let query: String?
        public let fragment: String?
    }

    /// Parse a URL into its components.
    public static func parse(_ urlString: String) -> ParsedUrl? {
        guard let url = URL(string: urlString),
              let scheme = url.scheme,
              let host = url.host else {
            return nil
        }

        return ParsedUrl(
            scheme: scheme.lowercased(),
            host: host,
            port: url.port,
            path: url.path.isEmpty ? "/" : url.path,
            query: url.query,
            fragment: url.fragment
        )
    }

    /// Check if a string is a valid URL.
    public static func isValid(_ urlString: String) -> Bool {
        parse(urlString) != nil
    }

    /// Extract the host from a URL.
    public static func getHost(_ urlString: String) -> String? {
        parse(urlString)?.host
    }

    /// Extract the path from a URL.
    public static func getPath(_ urlString: String) -> String? {
        parse(urlString)?.path
    }

    /// Check if a URL uses HTTPS.
    public static func isHttps(_ urlString: String) -> Bool {
        guard let parsed = parse(urlString) else { return false }
        return parsed.scheme == "https"
    }

    /// Check if a URL uses a secure scheme (https, wss).
    public static func isSecure(_ urlString: String) -> Bool {
        guard let parsed = parse(urlString) else { return false }
        return parsed.scheme == "https" || parsed.scheme == "wss"
    }
}
