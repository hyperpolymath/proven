// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven;

import java.math.BigInteger;

/**
 * Safe mathematical operations that detect overflow without throwing.
 * All operations return Result types for safe error handling.
 */
public final class SafeMath {
    private SafeMath() {}

    /**
     * Safe addition with overflow detection.
     */
    public static Result<Long, String> addSafe(long a, long b) {
        try {
            return Result.ok(Math.addExact(a, b));
        } catch (ArithmeticException e) {
            return Result.err("Integer overflow in addition");
        }
    }

    /**
     * Safe subtraction with overflow detection.
     */
    public static Result<Long, String> subSafe(long a, long b) {
        try {
            return Result.ok(Math.subtractExact(a, b));
        } catch (ArithmeticException e) {
            return Result.err("Integer overflow in subtraction");
        }
    }

    /**
     * Safe multiplication with overflow detection.
     */
    public static Result<Long, String> mulSafe(long a, long b) {
        try {
            return Result.ok(Math.multiplyExact(a, b));
        } catch (ArithmeticException e) {
            return Result.err("Integer overflow in multiplication");
        }
    }

    /**
     * Safe division with zero check.
     */
    public static Result<Long, String> divSafe(long a, long b) {
        if (b == 0) {
            return Result.err("Division by zero");
        }
        if (a == Long.MIN_VALUE && b == -1) {
            return Result.err("Integer overflow in division");
        }
        return Result.ok(a / b);
    }

    /**
     * Safe modulo with zero check.
     */
    public static Result<Long, String> modSafe(long a, long b) {
        if (b == 0) {
            return Result.err("Modulo by zero");
        }
        return Result.ok(a % b);
    }

    /**
     * Safe negation with overflow detection.
     */
    public static Result<Long, String> negateSafe(long a) {
        try {
            return Result.ok(Math.negateExact(a));
        } catch (ArithmeticException e) {
            return Result.err("Integer overflow in negation");
        }
    }

    /**
     * Safe absolute value with overflow detection.
     */
    public static Result<Long, String> absSafe(long a) {
        if (a == Long.MIN_VALUE) {
            return Result.err("Integer overflow in absolute value");
        }
        return Result.ok(Math.abs(a));
    }

    /**
     * Checked integer addition for int type.
     */
    public static Result<Integer, String> addSafeInt(int a, int b) {
        try {
            return Result.ok(Math.addExact(a, b));
        } catch (ArithmeticException e) {
            return Result.err("Integer overflow in addition");
        }
    }

    /**
     * Checked integer multiplication for int type.
     */
    public static Result<Integer, String> mulSafeInt(int a, int b) {
        try {
            return Result.ok(Math.multiplyExact(a, b));
        } catch (ArithmeticException e) {
            return Result.err("Integer overflow in multiplication");
        }
    }

    /**
     * Safe power operation using BigInteger to detect overflow.
     */
    public static Result<Long, String> powSafe(long base, int exponent) {
        if (exponent < 0) {
            return Result.err("Negative exponent not supported for integer power");
        }
        if (exponent == 0) {
            return Result.ok(1L);
        }
        try {
            BigInteger result = BigInteger.valueOf(base).pow(exponent);
            return Result.ok(result.longValueExact());
        } catch (ArithmeticException e) {
            return Result.err("Integer overflow in power operation");
        }
    }

    /**
     * Clamp a value to a range.
     */
    public static long clamp(long value, long min, long max) {
        return Math.max(min, Math.min(max, value));
    }

    /**
     * Check if addition would overflow.
     */
    public static boolean wouldOverflowAdd(long a, long b) {
        return addSafe(a, b).isErr();
    }

    /**
     * Check if multiplication would overflow.
     */
    public static boolean wouldOverflowMul(long a, long b) {
        return mulSafe(a, b).isErr();
    }
}
