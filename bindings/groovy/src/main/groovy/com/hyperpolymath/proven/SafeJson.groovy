// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeJson - JNA wrapper for proven_json_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe JSON validation via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No JSON logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeJson {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Check if a string contains valid JSON. */
    static Optional<Boolean> isValid(String json) {
        Memory mem = NativeUtil.toNativeString(json)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = json.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractBool(
            LIB.proven_json_is_valid(mem, bytes.length))
    }
}
