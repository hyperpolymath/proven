// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.util.Optional;

/**
 * Safe hexadecimal encoding via libproven FFI.
 *
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No hex encoding logic is reimplemented in Java.
 */
public final class SafeHex {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeHex() {}

    /**
     * Encode bytes to lowercase hex string.
     *
     * @param data the bytes to encode
     * @return the hex string, or empty on error
     */
    public static Optional<String> encode(byte[] data) {
        return encodeInternal(data, false);
    }

    /**
     * Encode bytes to uppercase hex string.
     *
     * @param data the bytes to encode
     * @return the hex string, or empty on error
     */
    public static Optional<String> encodeUpper(byte[] data) {
        return encodeInternal(data, true);
    }

    private static Optional<String> encodeInternal(byte[] data, boolean uppercase) {
        Memory mem = NativeUtil.toNativeBytes(data);
        if (mem == null) {
            return Optional.empty();
        }
        return NativeUtil.extractString(
            LIB.proven_hex_encode(mem, data.length, uppercase));
    }
}
