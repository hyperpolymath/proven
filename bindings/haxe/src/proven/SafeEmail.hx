// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail - Email validation via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * Email address validation (RFC 5321 simplified).
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `null` on error.
 */
class SafeEmail {
    /**
     * Validate an email address.
     * @param s Email address string
     * @return true if valid, false if invalid, null on error
     */
    public static function isValid(s:String):Null<Bool> {
        return LibProven.emailIsValid(s);
    }
}
