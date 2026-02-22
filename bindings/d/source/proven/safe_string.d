// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe string operations for XSS prevention and sanitization.
 *
 * Thin FFI wrapper around libproven's SafeString module. All escaping
 * and validation logic is performed in formally verified Idris 2 code.
 * This module only marshals data to/from the C ABI.
 */
module proven.safe_string;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Escape HTML special characters (prevents XSS).
/// Returns null on FFI failure.
string escapeHtml(string input) @trusted nothrow
{
    if (input.length == 0)
        return "";
    auto result = proven_string_escape_html(
        cast(const(ubyte)*) input.ptr, input.length
    );
    return provenStringToD(result);
}

/// Escape for SQL (single quotes).
/// Returns null on FFI failure.
string escapeSql(string input) @trusted nothrow
{
    if (input.length == 0)
        return "";
    auto result = proven_string_escape_sql(
        cast(const(ubyte)*) input.ptr, input.length
    );
    return provenStringToD(result);
}

/// Escape for JavaScript string literals.
/// Returns null on FFI failure.
string escapeJs(string input) @trusted nothrow
{
    if (input.length == 0)
        return "";
    auto result = proven_string_escape_js(
        cast(const(ubyte)*) input.ptr, input.length
    );
    return provenStringToD(result);
}

/// Check if bytes are valid UTF-8.
bool isValidUtf8(const(ubyte)[] data) @trusted nothrow @nogc
{
    if (data.length == 0)
        return true;
    auto result = proven_string_is_valid_utf8(data.ptr, data.length);
    if (provenFailed(result.status))
        return false;
    return result.value;
}

/// Check if a string contains valid UTF-8.
bool isValidUtf8String(string input) @trusted nothrow
{
    return isValidUtf8(cast(const(ubyte)[]) input);
}

/// URL encode a string (RFC 3986 percent encoding).
/// Returns null on FFI failure.
string urlEncode(string input) @trusted nothrow
{
    if (input.length == 0)
        return "";
    auto result = proven_http_url_encode(
        cast(const(ubyte)*) input.ptr, input.length
    );
    return provenStringToD(result);
}

/// URL decode a percent-encoded string.
/// Returns null on FFI failure.
Nullable!string urlDecode(string input) @trusted nothrow
{
    if (input.length == 0)
        return nullable("");
    auto result = proven_http_url_decode(
        cast(const(ubyte)*) input.ptr, input.length
    );
    auto decoded = provenStringToD(result);
    if (decoded is null)
        return Nullable!string.init;
    return nullable(decoded);
}
