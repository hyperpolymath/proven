// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCalculator - JNA wrapper for proven_calculator_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import com.sun.jna.Memory
import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets
import java.util.Optional

/**
 * Safe expression evaluator via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No calculator logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeCalculator {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Evaluate a mathematical expression string. */
    static Optional<Double> eval(String expression) {
        Memory mem = NativeUtil.toNativeString(expression)
        if (mem == null) {
            return Optional.empty()
        }
        byte[] bytes = expression.getBytes(StandardCharsets.UTF_8)
        NativeUtil.extractDouble(
            LIB.proven_calculator_eval(mem, bytes.length))
    }
}
