// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString - Encoding-safe string operations for Squirrel.
//
// All operations delegate to libproven via the native "proven" table.
// Returns null on error.

/**
 * SafeString - Safe text encoding and escaping.
 *
 * Provides XSS-safe HTML escaping, SQL injection prevention, and
 * JavaScript string literal escaping. All backed by verified Idris 2 code.
 *
 * @example
 *   local ss = SafeString();
 *   local safe = ss.escape_html("<script>alert('xss')</script>");
 *   // "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"
 */
class SafeString {
    /**
     * Check if a string contains valid UTF-8 bytes.
     * @param {string} str - Input string.
     * @return {bool|null} true if valid UTF-8, null on error.
     */
    function is_valid_utf8(str) {
        return proven.is_valid_utf8(str);
    }

    /**
     * Escape a string for safe inclusion in SQL queries.
     * Prefer parameterized queries over string escaping.
     * @param {string} str - Input string.
     * @return {string|null} Escaped string, or null on error.
     */
    function escape_sql(str) {
        return proven.escape_sql(str);
    }

    /**
     * Escape a string for safe inclusion in HTML content.
     * Prevents XSS attacks by escaping <, >, &, ", '.
     * @param {string} str - Input string.
     * @return {string|null} HTML-safe string, or null on error.
     */
    function escape_html(str) {
        return proven.escape_html(str);
    }

    /**
     * Escape a string for safe inclusion in JavaScript string literals.
     * @param {string} str - Input string.
     * @return {string|null} JS-safe string, or null on error.
     */
    function escape_js(str) {
        return proven.escape_js(str);
    }

    /**
     * Check if a filesystem path contains traversal sequences ("..").
     * @param {string} path - Path to check.
     * @return {bool|null} true if traversal detected, null on error.
     */
    function path_has_traversal(path) {
        return proven.path_has_traversal(path);
    }

    /**
     * Sanitize a filename by removing dangerous characters.
     * @param {string} name - Filename to sanitize.
     * @return {string|null} Sanitized filename, or null on error.
     */
    function sanitize_filename(name) {
        return proven.sanitize_filename(name);
    }
}
