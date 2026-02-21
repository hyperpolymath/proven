// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Semantic versioning support.
public struct SemVer: Equatable, Comparable, Hashable, CustomStringConvertible {
    public let major: Int
    public let minor: Int
    public let patch: Int
    public let prerelease: String?
    public let build: String?

    public init(major: Int, minor: Int, patch: Int, prerelease: String? = nil, build: String? = nil) {
        self.major = max(0, major)
        self.minor = max(0, minor)
        self.patch = max(0, patch)
        self.prerelease = prerelease
        self.build = build
    }

    /// Parse a semantic version string.
    public static func parse(_ version: String) -> SemVer? {
        let pattern = #"^(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9.-]+))?(?:\+([a-zA-Z0-9.-]+))?$"#
        guard let regex = try? NSRegularExpression(pattern: pattern),
              let match = regex.firstMatch(in: version, range: NSRange(version.startIndex..., in: version)) else {
            return nil
        }

        func group(_ n: Int) -> String? {
            guard let range = Range(match.range(at: n), in: version) else { return nil }
            return String(version[range])
        }

        guard let majorStr = group(1), let major = Int(majorStr),
              let minorStr = group(2), let minor = Int(minorStr),
              let patchStr = group(3), let patch = Int(patchStr) else {
            return nil
        }

        return SemVer(major: major, minor: minor, patch: patch, prerelease: group(4), build: group(5))
    }

    public var description: String {
        var result = "\(major).\(minor).\(patch)"
        if let pre = prerelease {
            result += "-\(pre)"
        }
        if let bld = build {
            result += "+\(bld)"
        }
        return result
    }

    public func bumpMajor() -> SemVer {
        SemVer(major: major + 1, minor: 0, patch: 0)
    }

    public func bumpMinor() -> SemVer {
        SemVer(major: major, minor: minor + 1, patch: 0)
    }

    public func bumpPatch() -> SemVer {
        SemVer(major: major, minor: minor, patch: patch + 1)
    }

    public var isPrerelease: Bool {
        prerelease != nil
    }

    public static func < (lhs: SemVer, rhs: SemVer) -> Bool {
        if lhs.major != rhs.major { return lhs.major < rhs.major }
        if lhs.minor != rhs.minor { return lhs.minor < rhs.minor }
        if lhs.patch != rhs.patch { return lhs.patch < rhs.patch }

        // Pre-release versions have lower precedence
        if lhs.prerelease != nil && rhs.prerelease == nil { return true }
        if lhs.prerelease == nil && rhs.prerelease != nil { return false }

        // Compare pre-release identifiers
        if let lhsPre = lhs.prerelease, let rhsPre = rhs.prerelease {
            return lhsPre < rhsPre
        }

        return false
    }

    /// Check if this version satisfies a requirement (simple comparison).
    public func satisfies(_ requirement: String) -> Bool {
        let trimmed = requirement.trimmingCharacters(in: .whitespaces)

        if trimmed.hasPrefix(">=") {
            guard let req = SemVer.parse(String(trimmed.dropFirst(2))) else { return false }
            return self >= req
        }
        if trimmed.hasPrefix("<=") {
            guard let req = SemVer.parse(String(trimmed.dropFirst(2))) else { return false }
            return self <= req
        }
        if trimmed.hasPrefix(">") {
            guard let req = SemVer.parse(String(trimmed.dropFirst(1))) else { return false }
            return self > req
        }
        if trimmed.hasPrefix("<") {
            guard let req = SemVer.parse(String(trimmed.dropFirst(1))) else { return false }
            return self < req
        }
        if trimmed.hasPrefix("=") {
            guard let req = SemVer.parse(String(trimmed.dropFirst(1))) else { return false }
            return self == req
        }
        if trimmed.hasPrefix("^") {
            guard let req = SemVer.parse(String(trimmed.dropFirst(1))) else { return false }
            // Compatible with major version
            return self.major == req.major && self >= req
        }
        if trimmed.hasPrefix("~") {
            guard let req = SemVer.parse(String(trimmed.dropFirst(1))) else { return false }
            // Compatible with minor version
            return self.major == req.major && self.minor == req.minor && self >= req
        }

        // Exact match
        guard let req = SemVer.parse(trimmed) else { return false }
        return self == req
    }
}

/// Version utilities namespace.
public enum SafeVersion {
    public static func parse(_ version: String) -> SemVer? {
        SemVer.parse(version)
    }

    public static func compare(_ a: SemVer, _ b: SemVer) -> Int {
        if a < b { return -1 }
        if a > b { return 1 }
        return 0
    }
}
