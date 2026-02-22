// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeHex - JNA wrapper for proven_hex_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.util.Optional

/**
 * Safe hexadecimal encoding via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No hex logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeHex {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Encode raw bytes to hexadecimal string. */
    static Optional<String> encode(byte[] data, boolean uppercase = false) {
        Memory mem = NativeUtil.toNativeBytes(data)
        if (mem == null) {
            return Optional.empty()
        }
        NativeUtil.extractString(
            LIB.proven_hex_encode(mem, data.length, uppercase))
    }
}
