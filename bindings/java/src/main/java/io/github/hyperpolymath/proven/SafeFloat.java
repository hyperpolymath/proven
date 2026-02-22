// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import java.util.Optional;

/**
 * Safe floating-point operations via libproven FFI.
 *
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No float logic is reimplemented in Java.
 */
public final class SafeFloat {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeFloat() {}

    /**
     * Safe division. Returns empty on division by zero or non-finite result.
     *
     * @param a the dividend
     * @param b the divisor
     * @return the quotient, or empty on error
     */
    public static Optional<Double> div(double a, double b) {
        return NativeUtil.extractDouble(LIB.proven_float_div(a, b));
    }

    /**
     * Check if a double value is finite (not NaN, not infinite).
     *
     * @param x the value to check
     * @return true if finite
     */
    public static boolean isFinite(double x) {
        return LIB.proven_float_is_finite(x);
    }

    /**
     * Check if a double value is NaN.
     *
     * @param x the value to check
     * @return true if NaN
     */
    public static boolean isNaN(double x) {
        return LIB.proven_float_is_nan(x);
    }

    /**
     * Safe square root. Returns empty for negative inputs.
     *
     * @param x the value
     * @return the square root, or empty on error
     */
    public static Optional<Double> sqrt(double x) {
        return NativeUtil.extractDouble(LIB.proven_float_sqrt(x));
    }

    /**
     * Safe natural logarithm. Returns empty for non-positive inputs.
     *
     * @param x the value
     * @return the natural log, or empty on error
     */
    public static Optional<Double> ln(double x) {
        return NativeUtil.extractDouble(LIB.proven_float_ln(x));
    }
}
