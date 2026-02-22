// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe path operations for directory traversal prevention.
 *
 * Thin FFI wrapper around libproven's SafePath module. All traversal
 * detection and filename sanitization is performed in formally verified
 * Idris 2 code. This module only marshals data to/from the C ABI.
 */
module proven.safe_path;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Check if path contains directory traversal sequences ("..").
/// Returns true if traversal is detected.
bool hasTraversal(string path) @trusted nothrow @nogc
{
    if (path.length == 0)
        return false;
    auto result = proven_path_has_traversal(
        cast(const(ubyte)*) path.ptr, path.length
    );
    if (provenFailed(result.status))
        return true; // Fail safe: assume traversal on error
    return result.value;
}

/// Sanitize a filename by removing dangerous characters.
/// Returns "unnamed" if the result would be empty.
string sanitizeFilename(string filename) @trusted nothrow
{
    if (filename.length == 0)
        return "unnamed";
    auto result = proven_path_sanitize_filename(
        cast(const(ubyte)*) filename.ptr, filename.length
    );
    auto sanitized = provenStringToD(result);
    if (sanitized is null || sanitized.length == 0)
        return "unnamed";
    return sanitized;
}
