// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

/**
 * Safe mathematical operations with Groovy-idiomatic API.
 * Provides checked arithmetic that never overflows silently.
 * Calls Idris 2 verified code via Zig ABI when available.
 */
@CompileStatic
class SafeMath {

    /**
     * Checked addition that returns Result instead of throwing.
     * Groovy operator overloading friendly.
     */
    static Result<Long, String> add(long a, long b) {
        if (ProvenNative.nativeLoaded) {
            long result = ProvenNative.nativeAdd(a, b)
            // Native returns Long.MIN_VALUE on overflow
            return result == Long.MIN_VALUE && (a >= 0 || b >= 0) ?
                Result.err("Integer overflow in addition") :
                Result.ok(result)
        }
        return addPure(a, b)
    }

    /**
     * Checked subtraction.
     */
    static Result<Long, String> sub(long a, long b) {
        if (ProvenNative.nativeLoaded) {
            long result = ProvenNative.nativeSub(a, b)
            return result == Long.MIN_VALUE && a >= 0 && b <= 0 ?
                Result.err("Integer overflow in subtraction") :
                Result.ok(result)
        }
        return subPure(a, b)
    }

    /**
     * Checked multiplication.
     */
    static Result<Long, String> mul(long a, long b) {
        if (ProvenNative.nativeLoaded) {
            long result = ProvenNative.nativeMul(a, b)
            return result == Long.MIN_VALUE && a != 0 && b != Long.MIN_VALUE ?
                Result.err("Integer overflow in multiplication") :
                Result.ok(result)
        }
        return mulPure(a, b)
    }

    /**
     * Checked division (never divides by zero).
     */
    static Result<Long, String> div(long a, long b) {
        if (b == 0) {
            return Result.err("Division by zero")
        }
        if (a == Long.MIN_VALUE && b == -1) {
            return Result.err("Integer overflow in division")
        }
        return Result.ok(a / b)
    }

    /**
     * Safe modulo (never divides by zero).
     */
    static Result<Long, String> mod(long a, long b) {
        if (b == 0) {
            return Result.err("Division by zero")
        }
        return Result.ok(a % b)
    }

    /**
     * Absolute value that handles Long.MIN_VALUE.
     */
    static Result<Long, String> abs(long a) {
        if (a == Long.MIN_VALUE) {
            return Result.err("Cannot compute abs of Long.MIN_VALUE")
        }
        return Result.ok(Math.abs(a))
    }

    /**
     * Safe power function with overflow checking.
     */
    static Result<Long, String> pow(long base, int exponent) {
        if (exponent < 0) {
            return Result.err("Negative exponent not supported for integers")
        }
        if (exponent == 0) {
            return Result.ok(1L)
        }

        long result = 1L
        long b = base
        int e = exponent

        while (e > 0) {
            if ((e & 1) == 1) {
                def mulResult = mul(result, b)
                if (mulResult.err) return mulResult
                result = mulResult.value
            }
            e >>= 1
            if (e > 0) {
                def squareResult = mul(b, b)
                if (squareResult.err) return squareResult
                b = squareResult.value
            }
        }
        return Result.ok(result)
    }

    /**
     * Clamp value to range [min, max].
     */
    static long clamp(long value, long min, long max) {
        Math.max(min, Math.min(max, value))
    }

    /**
     * Safe integer parsing.
     */
    static Result<Long, String> parseLong(String s) {
        if (s == null || s.trim().empty) {
            return Result.err("Cannot parse empty string")
        }
        try {
            return Result.ok(Long.parseLong(s.trim()))
        } catch (NumberFormatException e) {
            return Result.err("Invalid number format: ${s}")
        }
    }

    // Pure Groovy implementations (fallback)

    private static Result<Long, String> addPure(long a, long b) {
        long result = a + b
        // Overflow if signs of a and b match but result differs
        if (((a ^ result) & (b ^ result)) < 0) {
            return Result.err("Integer overflow in addition")
        }
        return Result.ok(result)
    }

    private static Result<Long, String> subPure(long a, long b) {
        long result = a - b
        // Overflow if sign of a and b differ and result sign differs from a
        if (((a ^ b) & (a ^ result)) < 0) {
            return Result.err("Integer overflow in subtraction")
        }
        return Result.ok(result)
    }

    private static Result<Long, String> mulPure(long a, long b) {
        long result = a * b
        long absA = Math.abs(a)
        long absB = Math.abs(b)
        if ((absA | absB) >>> 31 != 0) {
            // Some bits are set in the upper 32 bits, check for overflow
            if (b != 0 && result / b != a) {
                return Result.err("Integer overflow in multiplication")
            }
            if (a == Long.MIN_VALUE && b == -1) {
                return Result.err("Integer overflow in multiplication")
            }
        }
        return Result.ok(result)
    }
}
