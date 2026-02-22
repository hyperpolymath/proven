// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString - String escaping, validation, and path safety via libproven FFI.
// All computation is performed in verified Idris 2 code via the Zig FFI
// layer. This Wren class is a thin wrapper; it does NOT reimplement any logic.

// Safe string operations for XSS prevention, SQL injection prevention,
// UTF-8 validation, and path traversal detection.
// Returns null on error instead of crashing.
class SafeString {
    // Check if bytes are valid UTF-8.
    // Returns true/false, or null on error.
    foreign static isValidUtf8(s)

    // Escape string for HTML (prevents XSS).
    // Returns the escaped string, or null on error.
    foreign static escapeHtml(s)

    // Escape string for SQL (single quotes).
    // Prefer parameterized queries over string escaping.
    // Returns the escaped string, or null on error.
    foreign static escapeSql(s)

    // Escape string for JavaScript string literals.
    // Returns the escaped string, or null on error.
    foreign static escapeJs(s)

    // Check if path contains directory traversal sequences ("..").
    // Returns true if traversal detected, false if safe, null on error.
    foreign static hasTraversal(s)

    // Sanitize a filename by removing dangerous characters.
    // Returns the sanitized filename, or null on error.
    foreign static sanitizeFilename(s)
}
