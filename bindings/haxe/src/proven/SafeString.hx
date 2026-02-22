// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString - String escaping and validation via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * Safe string operations for XSS prevention, SQL injection prevention,
 * and UTF-8 validation.
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `null` on error.
 */
class SafeString {
    /**
     * Check if bytes are valid UTF-8.
     * @param s Input string
     * @return true if valid UTF-8, false if invalid, null on error
     */
    public static function isValidUtf8(s:String):Null<Bool> {
        return LibProven.stringIsValidUtf8(s);
    }

    /**
     * Escape string for SQL (single quotes).
     * Prefer parameterized queries over string escaping.
     * @param s Input string
     * @return Escaped string, or null on error
     */
    public static function escapeSql(s:String):Null<String> {
        return LibProven.stringEscapeSql(s);
    }

    /**
     * Escape string for HTML (prevents XSS).
     * @param s Input string
     * @return Escaped string, or null on error
     */
    public static function escapeHtml(s:String):Null<String> {
        return LibProven.stringEscapeHtml(s);
    }

    /**
     * Escape string for JavaScript string literals.
     * @param s Input string
     * @return Escaped string, or null on error
     */
    public static function escapeJs(s:String):Null<String> {
        return LibProven.stringEscapeJs(s);
    }
}
