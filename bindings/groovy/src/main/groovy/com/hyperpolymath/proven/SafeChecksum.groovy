// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeChecksum - JNA wrapper for proven_checksum_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.util.Optional

/**
 * Safe checksum operations via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No checksum logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeChecksum {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Compute CRC32 checksum of data. */
    static Optional<Long> crc32(byte[] data) {
        Memory mem = NativeUtil.toNativeBytes(data)
        if (mem == null) {
            return Optional.empty()
        }
        NativeUtil.extractLong(
            LIB.proven_checksum_crc32(mem, data.length))
    }

    /** Verify that data matches an expected CRC32 checksum. */
    static Optional<Boolean> verifyCrc32(byte[] data, int expected) {
        Memory mem = NativeUtil.toNativeBytes(data)
        if (mem == null) {
            return Optional.empty()
        }
        NativeUtil.extractBool(
            LIB.proven_checksum_verify_crc32(mem, data.length, expected))
    }
}
