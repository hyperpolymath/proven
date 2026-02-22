// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe email validation via libproven FFI.
 *
 * Provides RFC 5321 email address validation.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No email validation logic is reimplemented in Java.
 */
public final class SafeEmail {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeEmail() {}

    /**
     * Check if an email address is valid (RFC 5321).
     *
     * @param email the email address to validate
     * @return true if valid, empty on null pointer error
     */
    public static Optional<Boolean> isValid(String email) {
        Memory mem = NativeUtil.toNativeString(email);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = email.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_email_is_valid(mem, bytes.length));
    }
}
