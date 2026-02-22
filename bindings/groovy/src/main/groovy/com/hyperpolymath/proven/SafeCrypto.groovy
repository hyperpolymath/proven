// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto - JNA wrapper for proven_crypto_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.util.Optional

/**
 * Safe cryptographic operations via libproven JNA FFI.
 *
 * Provides constant-time comparison and secure random byte generation.
 * Every method delegates to the Idris 2 verified implementation.
 * No cryptographic logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeCrypto {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /**
     * Constant-time byte comparison (timing attack prevention).
     */
    static Optional<Boolean> constantTimeEq(byte[] a, byte[] b) {
        Memory memA = NativeUtil.toNativeBytes(a)
        Memory memB = NativeUtil.toNativeBytes(b)
        if (memA == null || memB == null) {
            return Optional.empty()
        }
        NativeUtil.extractBool(
            LIB.proven_crypto_constant_time_eq(memA, a.length, memB, b.length))
    }

    /**
     * Fill a buffer with cryptographically secure random bytes.
     *
     * @param length Number of bytes to generate.
     * @return Random bytes, or empty on error.
     */
    static Optional<byte[]> randomBytes(int length) {
        if (length <= 0) {
            return Optional.empty()
        }
        Memory mem = new Memory(length)
        int status = LIB.proven_crypto_random_bytes(mem, length)
        if (status != 0) {
            return Optional.empty()
        }
        byte[] result = mem.getByteArray(0, length)
        return Optional.of(result)
    }
}
