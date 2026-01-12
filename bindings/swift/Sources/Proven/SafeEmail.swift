// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe email validation and parsing operations.
public enum SafeEmail {
    /// Represents the parts of an email address.
    public struct EmailParts: Equatable {
        public let localPart: String
        public let domain: String
    }

    /// Check if an email address is valid (basic check).
    public static func isValid(_ email: String) -> Bool {
        let parts = email.split(separator: "@", omittingEmptySubsequences: false)
        guard parts.count == 2 else { return false }

        let localPart = parts[0]
        let domain = parts[1]

        guard !localPart.isEmpty else { return false }
        guard domain.count >= 3 else { return false }
        guard domain.contains(".") else { return false }
        guard !domain.hasPrefix(".") else { return false }
        guard !domain.hasSuffix(".") else { return false }

        return true
    }

    /// Split an email into local part and domain.
    public static func split(_ email: String) -> EmailParts? {
        guard isValid(email) else { return nil }

        let parts = email.split(separator: "@", omittingEmptySubsequences: false)
        return EmailParts(
            localPart: String(parts[0]),
            domain: String(parts[1])
        )
    }

    /// Extract the domain from an email address.
    public static func getDomain(_ email: String) -> String? {
        split(email)?.domain
    }

    /// Extract the local part from an email address.
    public static func getLocalPart(_ email: String) -> String? {
        split(email)?.localPart
    }

    /// Normalize an email address (lowercase domain).
    public static func normalize(_ email: String) -> String? {
        guard let parts = split(email) else { return nil }
        return parts.localPart + "@" + parts.domain.lowercased()
    }
}
