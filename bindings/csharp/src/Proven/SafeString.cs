// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeString.cs - Safe text operations including encoding validation and XSS prevention.
//
// Thin P/Invoke wrapper over libproven. ALL computation is performed in
// verified Idris 2 code via the Zig FFI bridge. No logic is reimplemented here.

namespace Proven
{
    /// <summary>
    /// Safe string operations backed by formally verified Idris 2 code.
    /// Provides UTF-8 validation, SQL escaping, HTML escaping, and JavaScript
    /// escaping. All methods delegate to the libproven FFI. Returns null on error.
    /// </summary>
    public static class SafeString
    {
        /// <summary>
        /// Check if a byte sequence is valid UTF-8.
        /// Delegates to proven_string_is_valid_utf8 via FFI.
        /// </summary>
        /// <param name="data">Byte data to validate.</param>
        /// <returns>true if valid UTF-8, false if not, null on FFI error.</returns>
        public static bool? IsValidUtf8(byte[] data)
        {
            return MarshalHelpers.BoolResultToNullable(
                LibProven.proven_string_is_valid_utf8(data, (nuint)data.Length));
        }

        /// <summary>
        /// Escape a string for safe SQL interpolation (single quotes).
        /// Prefer parameterized queries over string escaping.
        /// Delegates to proven_string_escape_sql via FFI.
        /// </summary>
        /// <param name="value">Input string to escape.</param>
        /// <returns>The escaped string, or null on error.</returns>
        public static string? EscapeSql(string value)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(value);
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_string_escape_sql(bytes, (nuint)bytes.Length));
        }

        /// <summary>
        /// Escape a string for safe HTML insertion (prevents XSS).
        /// Delegates to proven_string_escape_html via FFI.
        /// </summary>
        /// <param name="value">Input string to escape.</param>
        /// <returns>The HTML-escaped string, or null on error.</returns>
        public static string? EscapeHtml(string value)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(value);
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_string_escape_html(bytes, (nuint)bytes.Length));
        }

        /// <summary>
        /// Escape a string for safe JavaScript string literal insertion.
        /// Delegates to proven_string_escape_js via FFI.
        /// </summary>
        /// <param name="value">Input string to escape.</param>
        /// <returns>The JS-escaped string, or null on error.</returns>
        public static string? EscapeJs(string value)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(value);
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_string_escape_js(bytes, (nuint)bytes.Length));
        }

        /// <summary>
        /// Encode bytes to a hexadecimal string.
        /// Delegates to proven_hex_encode via FFI.
        /// </summary>
        /// <param name="data">Byte data to encode.</param>
        /// <param name="uppercase">If true, use uppercase hex digits (A-F).</param>
        /// <returns>The hex-encoded string, or null on error.</returns>
        public static string? HexEncode(byte[] data, bool uppercase = false)
        {
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_hex_encode(data, (nuint)data.Length, uppercase));
        }

        /// <summary>
        /// URL-encode a string using RFC 3986 percent encoding.
        /// Unreserved characters (A-Za-z0-9-._~) pass through; others become %XX.
        /// Delegates to proven_http_url_encode via FFI.
        /// </summary>
        /// <param name="value">Input string to encode.</param>
        /// <returns>The percent-encoded string, or null on error.</returns>
        public static string? UrlEncode(string value)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(value);
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_http_url_encode(bytes, (nuint)bytes.Length));
        }

        /// <summary>
        /// URL-decode a percent-encoded string.
        /// Delegates to proven_http_url_decode via FFI.
        /// </summary>
        /// <param name="value">Percent-encoded string to decode.</param>
        /// <returns>The decoded string, or null on error.</returns>
        public static string? UrlDecode(string value)
        {
            byte[] bytes = MarshalHelpers.ToUtf8(value);
            return MarshalHelpers.StringResultToManaged(
                LibProven.proven_http_url_decode(bytes, (nuint)bytes.Length));
        }
    }
}
