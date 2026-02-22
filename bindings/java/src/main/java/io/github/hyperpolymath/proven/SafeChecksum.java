// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.util.Optional;

/**
 * Safe checksum operations via libproven FFI.
 *
 * Provides CRC32 computation and verification.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No checksum logic is reimplemented in Java.
 */
public final class SafeChecksum {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeChecksum() {}

    /**
     * Compute CRC32 checksum of a byte array.
     *
     * @param data the data to checksum
     * @return the CRC32 value, or empty on error
     */
    public static Optional<Long> crc32(byte[] data) {
        Memory mem = NativeUtil.toNativeBytes(data);
        if (mem == null) {
            return Optional.empty();
        }
        return NativeUtil.extractLong(
            LIB.proven_checksum_crc32(mem, data.length));
    }

    /**
     * Verify a CRC32 checksum against expected value.
     *
     * @param data     the data to verify
     * @param expected the expected CRC32 value
     * @return true if the checksum matches, empty on error
     */
    public static Optional<Boolean> verifyCrc32(byte[] data, int expected) {
        Memory mem = NativeUtil.toNativeBytes(data);
        if (mem == null) {
            return Optional.empty();
        }
        return NativeUtil.extractBool(
            LIB.proven_checksum_verify_crc32(mem, data.length, expected));
    }
}
