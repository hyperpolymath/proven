// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Semantic versioning support delegated to libproven FFI.
///
/// Parsing and comparison via the formally verified Idris 2 core.

import CProven

/// Parsed semantic version returned by SafeVersion.parse.
public struct SemVer: Equatable, Sendable {
    public let major: UInt32
    public let minor: UInt32
    public let patch: UInt32
    public let prerelease: String?
}

public enum SafeVersion {
    /// Parse a semantic version string (e.g., "1.2.3-alpha").
    public static func parse(_ version: String) -> Result<SemVer, ProvenError> {
        withStringBytes(version) { ptr, len in
            var result = proven_version_parse(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            let pre: String?
            if let prePtr = result.version.prerelease, result.version.prerelease_len > 0 {
                pre = String(cString: prePtr)
            } else {
                pre = nil
            }
            proven_version_free(&result.version)
            return .success(SemVer(
                major: result.version.major,
                minor: result.version.minor,
                patch: result.version.patch,
                prerelease: pre
            ))
        }
    }

    /// Compare two semantic versions.
    /// Returns negative if a < b, 0 if equal, positive if a > b.
    public static func compare(_ a: SemVer, _ b: SemVer) -> Int32 {
        var cA = ProvenSemanticVersion()
        cA.major = a.major
        cA.minor = a.minor
        cA.patch = a.patch
        cA.prerelease = nil
        cA.prerelease_len = 0

        var cB = ProvenSemanticVersion()
        cB.major = b.major
        cB.minor = b.minor
        cB.patch = b.patch
        cB.prerelease = nil
        cB.prerelease_len = 0

        return proven_version_compare(cA, cB)
    }
}
