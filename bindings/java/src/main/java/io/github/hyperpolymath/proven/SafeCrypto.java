// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.util.Optional;

/**
 * Safe cryptographic operations via libproven FFI.
 *
 * Provides constant-time comparison and secure random byte generation.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No cryptographic logic is reimplemented in Java.
 */
public final class SafeCrypto {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    /** Maximum random byte request size (1 MiB). */
    private static final int MAX_RANDOM_BYTES = 1024 * 1024;

    private SafeCrypto() {}

    /**
     * Constant-time byte array comparison to prevent timing attacks.
     *
     * @param a first byte array
     * @param b second byte array
     * @return true if equal, empty if null pointer error
     */
    public static Optional<Boolean> constantTimeEquals(byte[] a, byte[] b) {
        Memory memA = NativeUtil.toNativeBytes(a);
        Memory memB = NativeUtil.toNativeBytes(b);
        if (memA == null || memB == null) {
            return Optional.empty();
        }
        return NativeUtil.extractBool(
            LIB.proven_crypto_constant_time_eq(memA, a.length, memB, b.length));
    }

    /**
     * Generate cryptographically secure random bytes.
     *
     * @param length number of bytes to generate (1 to 1 MiB)
     * @return the random bytes, or empty on invalid length or allocation error
     */
    public static Optional<byte[]> randomBytes(int length) {
        if (length <= 0 || length > MAX_RANDOM_BYTES) {
            return Optional.empty();
        }
        Memory mem = new Memory(length);
        int statusCode = LIB.proven_crypto_random_bytes(mem, length);
        ProvenStatus status = ProvenStatus.fromCode(statusCode);
        if (status.isError()) {
            return Optional.empty();
        }
        return Optional.of(mem.getByteArray(0, length));
    }
}
