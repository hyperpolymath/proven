// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe string operations for escaping and sanitization.
public enum SafeString {
    /// Escape a string for safe HTML insertion.
    public static func escapeHtml(_ value: String) -> String {
        value
            .replacingOccurrences(of: "&", with: "&amp;")
            .replacingOccurrences(of: "<", with: "&lt;")
            .replacingOccurrences(of: ">", with: "&gt;")
            .replacingOccurrences(of: "\"", with: "&quot;")
            .replacingOccurrences(of: "'", with: "&#x27;")
    }

    /// Escape a string for safe SQL interpolation.
    /// Note: Prefer parameterized queries over string interpolation.
    public static func escapeSql(_ value: String) -> String {
        value.replacingOccurrences(of: "'", with: "''")
    }

    /// Escape a string for safe JavaScript string literal insertion.
    public static func escapeJs(_ value: String) -> String {
        value
            .replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")
            .replacingOccurrences(of: "'", with: "\\'")
            .replacingOccurrences(of: "\n", with: "\\n")
            .replacingOccurrences(of: "\r", with: "\\r")
            .replacingOccurrences(of: "\t", with: "\\t")
    }

    /// Percent-encode a string for safe URL inclusion.
    public static func escapeUrl(_ value: String) -> String {
        value.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed) ?? value
    }

    /// Safely truncate a string to a maximum length, respecting UTF-8 boundaries.
    public static func truncateSafe(_ value: String, maxLength: Int, suffix: String = "...") -> String {
        guard maxLength >= 0 else { return "" }
        guard value.count > maxLength else { return value }

        let suffixCount = suffix.count
        guard maxLength > suffixCount else {
            return String(value.prefix(maxLength))
        }

        return String(value.prefix(maxLength - suffixCount)) + suffix
    }
}
