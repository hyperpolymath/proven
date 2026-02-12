// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.math.BigInteger;

/**
 * Safe mathematical operations that detect overflow without throwing.
 * All operations return ProvenResult types for safe error handling.
 * Calls native verified code via JNI when available.
 */
public final class SafeMath {
    private SafeMath() {}

    /**
     * Safe addition with overflow detection.
     */
    public static ProvenResult<Long> addSafe(long a, long b) {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMathAdd(a, b, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return addSafePure(a, b);
    }

    /**
     * Safe subtraction with overflow detection.
     */
    public static ProvenResult<Long> subSafe(long a, long b) {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMathSub(a, b, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return subSafePure(a, b);
    }

    /**
     * Safe multiplication with overflow detection.
     */
    public static ProvenResult<Long> mulSafe(long a, long b) {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMathMul(a, b, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return mulSafePure(a, b);
    }

    /**
     * Safe division with zero check.
     */
    public static ProvenResult<Long> divSafe(long a, long b) {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMathDiv(a, b, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return divSafePure(a, b);
    }

    /**
     * Safe modulo with zero check.
     */
    public static ProvenResult<Long> modSafe(long a, long b) {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMathMod(a, b, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        return modSafePure(a, b);
    }

    /**
     * Safe negation with overflow detection.
     */
    public static ProvenResult<Long> negateSafe(long a) {
        if (a == Long.MIN_VALUE) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Integer overflow in negation");
        }
        return ProvenResult.ok(-a);
    }

    /**
     * Safe absolute value with overflow detection.
     */
    public static ProvenResult<Long> absSafe(long a) {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMathAbs(a, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        if (a == Long.MIN_VALUE) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Integer overflow in absolute value");
        }
        return ProvenResult.ok(Math.abs(a));
    }

    /**
     * Clamp a value to a range.
     */
    public static long clamp(long value, long min, long max) {
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeMathClamp(min, max, value);
        }
        return Math.max(min, Math.min(max, value));
    }

    /**
     * Safe power operation using BigInteger to detect overflow.
     */
    public static ProvenResult<Long> powSafe(long base, int exponent) {
        if (exponent < 0) {
            return ProvenResult.err(ProvenStatus.ERR_INVALID_ARGUMENT,
                "Negative exponent not supported for integer power");
        }
        if (exponent == 0) {
            return ProvenResult.ok(1L);
        }
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMathPow(base, exponent, status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(result) : ProvenResult.err(s);
        }
        try {
            BigInteger result = BigInteger.valueOf(base).pow(exponent);
            return ProvenResult.ok(result.longValueExact());
        } catch (ArithmeticException e) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Integer overflow in power operation");
        }
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

    /**
     * Checked integer addition for int type.
     */
    public static ProvenResult<Integer> addSafeInt(int a, int b) {
        try {
            return ProvenResult.ok(Math.addExact(a, b));
        } catch (ArithmeticException e) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Integer overflow in addition");
        }
    }

    /**
     * Checked integer subtraction for int type.
     */
    public static ProvenResult<Integer> subSafeInt(int a, int b) {
        try {
            return ProvenResult.ok(Math.subtractExact(a, b));
        } catch (ArithmeticException e) {
            return ProvenResult.err(ProvenStatus.ERR_UNDERFLOW, "Integer underflow in subtraction");
        }
    }

    /**
     * Checked integer multiplication for int type.
     */
    public static ProvenResult<Integer> mulSafeInt(int a, int b) {
        try {
            return ProvenResult.ok(Math.multiplyExact(a, b));
        } catch (ArithmeticException e) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Integer overflow in multiplication");
        }
    }

    // Pure Java fallback implementations

    private static ProvenResult<Long> addSafePure(long a, long b) {
        try {
            return ProvenResult.ok(Math.addExact(a, b));
        } catch (ArithmeticException e) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Integer overflow in addition");
        }
    }

    private static ProvenResult<Long> subSafePure(long a, long b) {
        try {
            return ProvenResult.ok(Math.subtractExact(a, b));
        } catch (ArithmeticException e) {
            return ProvenResult.err(ProvenStatus.ERR_UNDERFLOW, "Integer underflow in subtraction");
        }
    }

    private static ProvenResult<Long> mulSafePure(long a, long b) {
        try {
            return ProvenResult.ok(Math.multiplyExact(a, b));
        } catch (ArithmeticException e) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Integer overflow in multiplication");
        }
    }

    private static ProvenResult<Long> divSafePure(long a, long b) {
        if (b == 0) {
            return ProvenResult.err(ProvenStatus.ERR_DIVISION_BY_ZERO, "Division by zero");
        }
        if (a == Long.MIN_VALUE && b == -1) {
            return ProvenResult.err(ProvenStatus.ERR_OVERFLOW, "Integer overflow in division");
        }
        return ProvenResult.ok(a / b);
    }

    private static ProvenResult<Long> modSafePure(long a, long b) {
        if (b == 0) {
            return ProvenResult.err(ProvenStatus.ERR_DIVISION_BY_ZERO, "Modulo by zero");
        }
        return ProvenResult.ok(a % b);
    }
}
