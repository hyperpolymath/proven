// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// NativeUtil - Data marshalling utilities for JNA calls to libproven.
// No domain logic; only pointer/string conversion.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import com.sun.jna.Pointer
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Internal utilities for marshalling data between Groovy and the libproven C ABI.
 */
@CompileStatic
final class NativeUtil {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    private NativeUtil() {}

    /**
     * Copy a byte array into a JNA Memory block.
     */
    static Memory toNativeBytes(byte[] data) {
        if (data == null || data.length == 0) {
            return null
        }
        Memory mem = new Memory(data.length)
        mem.write(0, data, 0, data.length)
        return mem
    }

    /**
     * Convert a String to a JNA Memory pointer (UTF-8 encoded).
     */
    static Memory toNativeString(String str) {
        if (str == null || str.isEmpty()) {
            return null
        }
        return toNativeBytes(str.getBytes(StandardCharsets.UTF_8))
    }

    /**
     * Extract a String from a StringResult and free the native string.
     */
    static Optional<String> extractString(ProvenLibrary.StringResult result) {
        if (result.status != 0) {
            freeStringResult(result)
            return Optional.empty()
        }
        if (result.value == null || result.length == 0) {
            return Optional.of('')
        }
        try {
            byte[] bytes = result.value.getByteArray(0, (int) result.length)
            return Optional.of(new String(bytes, StandardCharsets.UTF_8))
        } finally {
            freeStringResult(result)
        }
    }

    /**
     * Free the native string in a StringResult if non-null.
     */
    static void freeStringResult(ProvenLibrary.StringResult result) {
        if (result.value != null) {
            LIB.proven_free_string(result.value)
        }
    }

    /**
     * Interpret an IntResult as Optional<Long>.
     */
    static Optional<Long> extractLong(ProvenLibrary.IntResult result) {
        if (result.status != 0) {
            return Optional.empty()
        }
        return Optional.of(result.value)
    }

    /**
     * Interpret a BoolResult as Optional<Boolean>.
     */
    static Optional<Boolean> extractBool(ProvenLibrary.BoolResult result) {
        if (result.status != 0) {
            return Optional.empty()
        }
        return Optional.of(result.value)
    }

    /**
     * Interpret a FloatResult as Optional<Double>.
     */
    static Optional<Double> extractDouble(ProvenLibrary.FloatResult result) {
        if (result.status != 0) {
            return Optional.empty()
        }
        return Optional.of(result.value)
    }
}
