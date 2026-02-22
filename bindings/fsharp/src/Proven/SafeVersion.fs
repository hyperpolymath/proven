// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe semantic version parsing and comparison via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeVersion =

    /// Parse semantic version string (e.g., "1.2.3-alpha").
    /// Returns the VersionResult struct on success, or None on failure.
    let parse (s: string) : VersionResult option =
        let bytes = toUtf8 s
        let result = FFI.proven_version_parse(bytes, unativeint bytes.Length)
        if result.Status = 0 then Some result
        else None

    /// Compare two semantic versions.
    /// Returns negative if a < b, 0 if a = b, positive if a > b.
    let compare (a: SemanticVersion) (b: SemanticVersion) : int =
        FFI.proven_version_compare(a, b)

    /// Free any prerelease string memory allocated within a version.
    /// Must be called when done with a VersionResult that has a non-null Prerelease pointer.
    let free (prerelease: nativeint) : unit =
        if prerelease <> nativeint 0 then
            FFI.proven_version_free(prerelease)
