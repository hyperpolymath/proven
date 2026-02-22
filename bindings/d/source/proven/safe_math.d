// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe arithmetic operations with overflow checking.
 *
 * Thin FFI wrapper around libproven's SafeMath module. All computation
 * is performed in formally verified Idris 2 code. This module only
 * marshals data to/from the C ABI.
 */
module proven.safe_math;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Add two long values with overflow checking.
/// Returns null on overflow.
Nullable!long safeAdd(long a, long b) nothrow @trusted @nogc
{
    auto result = proven_math_add_checked(a, b);
    if (provenFailed(result.status))
        return Nullable!long.init;
    return nullable(result.value);
}

/// Subtract two long values with underflow checking.
/// Returns null on underflow.
Nullable!long safeSub(long a, long b) nothrow @trusted @nogc
{
    auto result = proven_math_sub_checked(a, b);
    if (provenFailed(result.status))
        return Nullable!long.init;
    return nullable(result.value);
}

/// Multiply two long values with overflow checking.
/// Returns null on overflow.
Nullable!long safeMul(long a, long b) nothrow @trusted @nogc
{
    auto result = proven_math_mul_checked(a, b);
    if (provenFailed(result.status))
        return Nullable!long.init;
    return nullable(result.value);
}

/// Divide two long values safely.
/// Returns null on division by zero or overflow (MIN / -1).
Nullable!long safeDiv(long a, long b) nothrow @trusted @nogc
{
    auto result = proven_math_div(a, b);
    if (provenFailed(result.status))
        return Nullable!long.init;
    return nullable(result.value);
}

/// Modulo operation with safety checks.
/// Returns null on division by zero.
Nullable!long safeMod(long a, long b) nothrow @trusted @nogc
{
    auto result = proven_math_mod(a, b);
    if (provenFailed(result.status))
        return Nullable!long.init;
    return nullable(result.value);
}

/// Absolute value with overflow checking.
/// Returns null for long.min (cannot be represented as positive).
Nullable!long safeAbs(long a) nothrow @trusted @nogc
{
    auto result = proven_math_abs_safe(a);
    if (provenFailed(result.status))
        return Nullable!long.init;
    return nullable(result.value);
}

/// Power operation with overflow checking.
/// Returns null on overflow or negative exponent.
Nullable!long safePow(long base, uint exp) nothrow @trusted @nogc
{
    auto result = proven_math_pow_checked(base, exp);
    if (provenFailed(result.status))
        return Nullable!long.init;
    return nullable(result.value);
}

/// Clamp value to [lo, hi] range.
long clamp(long value, long lo, long hi) nothrow @trusted @nogc
{
    return proven_math_clamp(lo, hi, value);
}

/// Check if value is in range [minVal, maxVal].
bool inRange(long value, long minVal, long maxVal) pure nothrow @safe @nogc
{
    return value >= minVal && value <= maxVal;
}

/// Safe sum of an array. Returns null on overflow.
Nullable!long safeSum(const long[] values) nothrow @trusted
{
    long result = 0;
    foreach (v; values)
    {
        auto r = proven_math_add_checked(result, v);
        if (provenFailed(r.status))
            return Nullable!long.init;
        result = r.value;
    }
    return nullable(result);
}

/// Safe product of an array. Returns null on overflow.
Nullable!long safeProduct(const long[] values) nothrow @trusted
{
    long result = 1;
    foreach (v; values)
    {
        auto r = proven_math_mul_checked(result, v);
        if (provenFailed(r.status))
            return Nullable!long.init;
        result = r.value;
    }
    return nullable(result);
}
