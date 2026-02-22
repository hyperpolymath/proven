// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUrl - URL parsing and encoding for Squirrel.
//
// All operations delegate to libproven via the native "proven" table.
// Returns null on error.

/**
 * SafeUrl - URL parsing, validation, and percent-encoding.
 *
 * @example
 *   local url = SafeUrl();
 *   local parts = url.parse("https://example.com:8080/path?q=1#frag");
 *   if (parts != null) {
 *       print("Host: " + parts.host);   // "example.com"
 *       print("Port: " + parts.port);   // 8080
 *   }
 *
 *   local encoded = url.encode("hello world&foo=bar");
 *   // "hello%20world%26foo%3Dbar"
 */
class SafeUrl {
    /**
     * Parse a URL into its components.
     * @param {string} url_string - URL to parse.
     * @return {table|null} Table with keys: scheme, host, port, path, query,
     *         fragment. Port is null if not specified. Returns null on error.
     */
    function parse(url_string) {
        return proven.parse_url(url_string);
    }

    /**
     * URL-encode a string (RFC 3986 percent-encoding).
     * @param {string} str - String to encode.
     * @return {string|null} Encoded string, or null on error.
     */
    function encode(str) {
        return proven.url_encode(str);
    }

    /**
     * URL-decode a percent-encoded string.
     * @param {string} str - Encoded string.
     * @return {string|null} Decoded string, or null on error.
     */
    function decode(str) {
        return proven.url_decode(str);
    }
}
