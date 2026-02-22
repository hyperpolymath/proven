// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import java.util.Optional;

/**
 * Safe mathematical operations via libproven FFI.
 *
 * Every method delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No arithmetic logic is reimplemented in Java.
 *
 * All fallible operations return {@link Optional} -- never throw for
 * domain errors (overflow, division by zero, etc.).
 */
public final class SafeMath {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeMath() {}

    /**
     * Checked addition. Returns empty on overflow.
     *
     * @param a first operand
     * @param b second operand
     * @return the sum, or empty if overflow would occur
     */
    public static Optional<Long> add(long a, long b) {
        return NativeUtil.extractLong(LIB.proven_math_add_checked(a, b));
    }

    /**
     * Checked subtraction. Returns empty on underflow.
     *
     * @param a first operand
     * @param b second operand
     * @return the difference, or empty if underflow would occur
     */
    public static Optional<Long> sub(long a, long b) {
        return NativeUtil.extractLong(LIB.proven_math_sub_checked(a, b));
    }

    /**
     * Checked multiplication. Returns empty on overflow.
     *
     * @param a first operand
     * @param b second operand
     * @return the product, or empty if overflow would occur
     */
    public static Optional<Long> mul(long a, long b) {
        return NativeUtil.extractLong(LIB.proven_math_mul_checked(a, b));
    }

    /**
     * Safe division. Returns empty on division by zero or overflow
     * (Long.MIN_VALUE / -1).
     *
     * @param numerator   the dividend
     * @param denominator the divisor
     * @return the quotient, or empty on error
     */
    public static Optional<Long> div(long numerator, long denominator) {
        return NativeUtil.extractLong(LIB.proven_math_div(numerator, denominator));
    }

    /**
     * Safe modulo. Returns empty on division by zero.
     *
     * @param numerator   the dividend
     * @param denominator the divisor
     * @return the remainder, or empty on error
     */
    public static Optional<Long> mod(long numerator, long denominator) {
        return NativeUtil.extractLong(LIB.proven_math_mod(numerator, denominator));
    }

    /**
     * Safe absolute value. Returns empty if the value is Long.MIN_VALUE
     * (which has no positive representation in two's complement).
     *
     * @param n the value
     * @return the absolute value, or empty on overflow
     */
    public static Optional<Long> abs(long n) {
        return NativeUtil.extractLong(LIB.proven_math_abs_safe(n));
    }

    /**
     * Clamp a value to the range [lo, hi].
     * This operation cannot fail.
     *
     * @param lo    the lower bound
     * @param hi    the upper bound
     * @param value the value to clamp
     * @return the clamped value
     */
    public static long clamp(long lo, long hi, long value) {
        return LIB.proven_math_clamp(lo, hi, value);
    }

    /**
     * Checked exponentiation. Returns empty on overflow.
     *
     * @param base     the base
     * @param exponent the non-negative exponent
     * @return base raised to the exponent, or empty on overflow
     */
    public static Optional<Long> pow(long base, int exponent) {
        return NativeUtil.extractLong(LIB.proven_math_pow_checked(base, exponent));
    }
}
