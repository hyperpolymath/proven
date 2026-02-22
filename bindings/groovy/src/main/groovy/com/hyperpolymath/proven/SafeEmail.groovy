// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail - JNA wrapper for proven_email_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe email validation via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No email validation logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeEmail {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /**
     * Check if an email address is valid (RFC 5321).
     */
    static Optional<Boolean> isValid(String email) {
        Memory mem = NativeUtil.toNativeString(email)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = email.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_email_is_valid(mem, bytes.length))
    }
}
