// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe JSON validation via libproven FFI.
 *
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No JSON parsing logic is reimplemented in Java.
 */
public final class SafeJson {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeJson() {}

    /**
     * Check if a string is valid JSON.
     *
     * @param json the string to validate
     * @return true if valid JSON, empty on null pointer error
     */
    public static Optional<Boolean> isValid(String json) {
        Memory mem = NativeUtil.toNativeString(json);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = json.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractBool(
            LIB.proven_json_is_valid(mem, bytes.length));
    }
}
