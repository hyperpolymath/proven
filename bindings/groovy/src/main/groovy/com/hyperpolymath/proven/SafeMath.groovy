// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath - JNA wrapper for proven_math_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

import java.util.Optional

/**
 * Safe arithmetic operations via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No arithmetic logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeMath {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /**
     * Checked addition. Returns empty on overflow.
     */
    static Optional<Long> add(long a, long b) {
        NativeUtil.extractLong(LIB.proven_math_add_checked(a, b))
    }

    /**
     * Checked subtraction. Returns empty on underflow.
     */
    static Optional<Long> sub(long a, long b) {
        NativeUtil.extractLong(LIB.proven_math_sub_checked(a, b))
    }

    /**
     * Checked multiplication. Returns empty on overflow.
     */
    static Optional<Long> mul(long a, long b) {
        NativeUtil.extractLong(LIB.proven_math_mul_checked(a, b))
    }

    /**
     * Safe division. Returns empty on division by zero.
     */
    static Optional<Long> div(long a, long b) {
        NativeUtil.extractLong(LIB.proven_math_div(a, b))
    }

    /**
     * Safe modulo. Returns empty on division by zero.
     */
    static Optional<Long> mod(long a, long b) {
        NativeUtil.extractLong(LIB.proven_math_mod(a, b))
    }

    /**
     * Safe absolute value. Returns empty for Long.MIN_VALUE.
     */
    static Optional<Long> abs(long n) {
        NativeUtil.extractLong(LIB.proven_math_abs_safe(n))
    }

    /**
     * Clamp value to range [lo, hi]. Cannot fail.
     */
    static long clamp(long lo, long hi, long value) {
        LIB.proven_math_clamp(lo, hi, value)
    }

    /**
     * Checked exponentiation. Returns empty on overflow.
     */
    static Optional<Long> pow(long base, int exponent) {
        NativeUtil.extractLong(LIB.proven_math_pow_checked(base, exponent))
    }
}
