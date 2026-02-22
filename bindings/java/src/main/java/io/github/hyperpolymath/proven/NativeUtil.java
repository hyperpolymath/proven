// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;
import com.sun.jna.Pointer;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Internal utilities for marshalling data between Java and the libproven C ABI.
 *
 * This class handles:
 * - Converting Java strings/byte arrays to native pointers for FFI calls
 * - Extracting strings from StringResult and freeing the native allocation
 * - Interpreting IntResult, BoolResult, FloatResult into Optional values
 *
 * No domain logic is implemented here; only data marshalling.
 */
final class NativeUtil {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private NativeUtil() {}

    /**
     * Copy a Java byte array into a JNA Memory block suitable for passing
     * to C functions that take (ptr, len) pairs.
     *
     * @param data the byte array to copy
     * @return a Memory pointer containing the data, or null if data is null/empty
     */
    static Memory toNativeBytes(byte[] data) {
        if (data == null || data.length == 0) {
            return null;
        }
        Memory mem = new Memory(data.length);
        mem.write(0, data, 0, data.length);
        return mem;
    }

    /**
     * Convert a Java String to a native Memory pointer (UTF-8 encoded).
     *
     * @param str the string to convert
     * @return a Memory pointer containing the UTF-8 bytes, or null if str is null/empty
     */
    static Memory toNativeString(String str) {
        if (str == null || str.isEmpty()) {
            return null;
        }
        return toNativeBytes(str.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Extract a Java String from a StringResult and free the native string.
     *
     * @param result the StringResult from a libproven call
     * @return Optional containing the string on success, empty on error
     */
    static Optional<String> extractString(ProvenLibrary.StringResult result) {
        if (result.status != ProvenStatus.OK.getCode()) {
            freeStringResult(result);
            return Optional.empty();
        }
        if (result.value == null || result.length == 0) {
            return Optional.of("");
        }
        try {
            byte[] bytes = result.value.getByteArray(0, (int) result.length);
            return Optional.of(new String(bytes, StandardCharsets.UTF_8));
        } finally {
            freeStringResult(result);
        }
    }

    /**
     * Extract a status-qualified string from a StringResult and free the native string.
     *
     * @param result the StringResult from a libproven call
     * @return the string on success, or null if the status indicates an error
     */
    static String extractStringOrNull(ProvenLibrary.StringResult result) {
        return extractString(result).orElse(null);
    }

    /**
     * Free the native string in a StringResult if it is non-null.
     */
    static void freeStringResult(ProvenLibrary.StringResult result) {
        if (result.value != null) {
            LIB.proven_free_string(result.value);
        }
    }

    /**
     * Interpret an IntResult as Optional<Long>.
     *
     * @param result the IntResult from a libproven call
     * @return Optional containing the value on success, empty on error
     */
    static Optional<Long> extractLong(ProvenLibrary.IntResult result) {
        if (result.status != ProvenStatus.OK.getCode()) {
            return Optional.empty();
        }
        return Optional.of(result.value);
    }

    /**
     * Interpret a BoolResult as Optional<Boolean>.
     *
     * @param result the BoolResult from a libproven call
     * @return Optional containing the value on success, empty on error
     */
    static Optional<Boolean> extractBool(ProvenLibrary.BoolResult result) {
        if (result.status != ProvenStatus.OK.getCode()) {
            return Optional.empty();
        }
        return Optional.of(result.value);
    }

    /**
     * Interpret a FloatResult as Optional<Double>.
     *
     * @param result the FloatResult from a libproven call
     * @return Optional containing the value on success, empty on error
     */
    static Optional<Double> extractDouble(ProvenLibrary.FloatResult result) {
        if (result.status != ProvenStatus.OK.getCode()) {
            return Optional.empty();
        }
        return Optional.of(result.value);
    }

    /**
     * Get the ProvenStatus from an IntResult.
     */
    static ProvenStatus statusOf(ProvenLibrary.IntResult result) {
        return ProvenStatus.fromCode(result.status);
    }

    /**
     * Get the ProvenStatus from a BoolResult.
     */
    static ProvenStatus statusOf(ProvenLibrary.BoolResult result) {
        return ProvenStatus.fromCode(result.status);
    }

    /**
     * Get the ProvenStatus from a StringResult.
     */
    static ProvenStatus statusOf(ProvenLibrary.StringResult result) {
        return ProvenStatus.fromCode(result.status);
    }

    /**
     * Get the ProvenStatus from a FloatResult.
     */
    static ProvenStatus statusOf(ProvenLibrary.FloatResult result) {
        return ProvenStatus.fromCode(result.status);
    }
}
