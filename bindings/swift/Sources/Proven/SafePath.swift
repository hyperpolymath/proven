// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe filesystem path operations with traversal attack prevention.
public enum SafePath {
    /// Check if a path contains directory traversal sequences.
    public static func hasTraversal(_ path: String) -> Bool {
        path.contains("..") || path.contains("~")
    }

    /// Check if a path is safe (no traversal attacks).
    public static func isSafe(_ path: String) -> Bool {
        !hasTraversal(path)
    }

    /// Sanitize a filename by removing dangerous characters.
    public static func sanitizeFilename(_ filename: String) -> String {
        filename
            .replacingOccurrences(of: "..", with: "_")
            .replacingOccurrences(of: "/", with: "_")
            .replacingOccurrences(of: "\\", with: "_")
            .replacingOccurrences(of: "<", with: "_")
            .replacingOccurrences(of: ">", with: "_")
            .replacingOccurrences(of: ":", with: "_")
            .replacingOccurrences(of: "\"", with: "_")
            .replacingOccurrences(of: "|", with: "_")
            .replacingOccurrences(of: "?", with: "_")
            .replacingOccurrences(of: "*", with: "_")
            .replacingOccurrences(of: "\0", with: "_")
    }

    /// Safely join path components, rejecting traversal attempts.
    /// Returns nil if any part contains traversal sequences.
    public static func safeJoin(base: String, parts: [String]) -> String? {
        guard !parts.contains(where: hasTraversal) else { return nil }

        let sanitized = parts.map(sanitizeFilename)
        var path = base

        for part in sanitized {
            if path.hasSuffix("/") {
                path += part
            } else {
                path += "/" + part
            }
        }

        return path
    }

    /// Safely join path components (variadic version).
    public static func safeJoin(base: String, _ parts: String...) -> String? {
        safeJoin(base: base, parts: parts)
    }
}
