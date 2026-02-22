// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeFloat - JNA wrapper for proven_float_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

import java.util.Optional

/**
 * Safe floating-point operations via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No float logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeFloat {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Safe division. Returns empty on division by zero or non-finite result. */
    static Optional<Double> div(double a, double b) {
        NativeUtil.extractDouble(LIB.proven_float_div(a, b))
    }

    /** Check if a value is finite (not NaN, not Inf). */
    static boolean isFinite(double x) {
        LIB.proven_float_is_finite(x)
    }

    /** Check if a value is NaN. */
    static boolean isNaN(double x) {
        LIB.proven_float_is_nan(x)
    }

    /** Safe square root. Returns empty for negative inputs. */
    static Optional<Double> sqrt(double x) {
        NativeUtil.extractDouble(LIB.proven_float_sqrt(x))
    }

    /** Safe natural logarithm. Returns empty for non-positive inputs. */
    static Optional<Double> ln(double x) {
        NativeUtil.extractDouble(LIB.proven_float_ln(x))
    }
}
