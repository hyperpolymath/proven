// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto - Cryptographic primitives via libproven FFI.
// All computation is performed in verified Idris 2 code. This module
// provides an idiomatic Haxe wrapper around the raw C FFI calls.

package proven;

import proven.LibProven;

/**
 * Safe cryptographic operations including constant-time comparison
 * (timing-attack resistant).
 *
 * All operations delegate to the formally verified Idris 2 core via
 * libproven. Returns `null` on error.
 */
class SafeCrypto {
    /**
     * Constant-time byte comparison (timing-attack safe).
     * @param a First buffer
     * @param b Second buffer
     * @return true if equal, false if different or lengths differ, null on error
     */
    public static function constantTimeEq(a:String, b:String):Null<Bool> {
        return LibProven.cryptoConstantTimeEq(a, b);
    }
}
