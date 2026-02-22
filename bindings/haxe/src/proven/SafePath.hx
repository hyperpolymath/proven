// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePath - Filesystem traversal prevention via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * Safe filesystem path operations that prevent directory traversal attacks
 * and sanitize filenames.
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `null` on error.
 */
class SafePath {
    /**
     * Check if path contains directory traversal sequences ("..").
     * @param s Path to check
     * @return true if traversal detected, false if safe, null on error
     */
    public static function hasTraversal(s:String):Null<Bool> {
        return LibProven.pathHasTraversal(s);
    }

    /**
     * Sanitize a filename by removing dangerous characters.
     * @param s Filename to sanitize
     * @return Sanitized filename, or null on error
     */
    public static function sanitizeFilename(s:String):Null<String> {
        return LibProven.pathSanitizeFilename(s);
    }
}
