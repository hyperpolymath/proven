// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe arithmetic operations with overflow checking.
 */
module proven.safe_math;

import std.typecons : Nullable, nullable;
import core.checkedint;

/// Add two long values with overflow checking.
Nullable!long safeAdd(long a, long b) nothrow @safe
{
    bool overflow;
    immutable result = adds(a, b, overflow);
    return overflow ? Nullable!long.init : nullable(result);
}

/// Subtract two long values with overflow checking.
Nullable!long safeSub(long a, long b) nothrow @safe
{
    bool overflow;
    immutable result = subs(a, b, overflow);
    return overflow ? Nullable!long.init : nullable(result);
}

/// Multiply two long values with overflow checking.
Nullable!long safeMul(long a, long b) nothrow @safe
{
    bool overflow;
    immutable result = muls(a, b, overflow);
    return overflow ? Nullable!long.init : nullable(result);
}

/// Divide two long values safely.
Nullable!long safeDiv(long a, long b) nothrow @safe
{
    if (b == 0)
        return Nullable!long.init;
    // Check for overflow (MIN_VALUE / -1)
    if (a == long.min && b == -1)
        return Nullable!long.init;
    return nullable(a / b);
}

/// Modulo operation with safety checks.
Nullable!long safeMod(long a, long b) nothrow @safe
{
    if (b == 0)
        return Nullable!long.init;
    return nullable(a % b);
}

/// Absolute value with overflow checking.
Nullable!long safeAbs(long a) nothrow @safe
{
    if (a == long.min)
        return Nullable!long.init;
    return nullable(a < 0 ? -a : a);
}

/// Negate with overflow checking.
Nullable!long safeNeg(long a) nothrow @safe
{
    if (a == long.min)
        return Nullable!long.init;
    bool overflow;
    immutable result = negs(a, overflow);
    return overflow ? Nullable!long.init : nullable(result);
}

/// Power operation with overflow checking.
Nullable!long safePow(long base, long exp) nothrow @safe
{
    if (exp < 0)
        return Nullable!long.init;
    if (exp == 0)
        return nullable(1L);
    if (exp == 1)
        return nullable(base);

    long result = 1;
    long b = base;
    long e = exp;

    while (e > 0)
    {
        if (e & 1)
        {
            auto newResult = safeMul(result, b);
            if (newResult.isNull)
                return Nullable!long.init;
            result = newResult.get;
        }
        e >>= 1;
        if (e > 0)
        {
            auto newB = safeMul(b, b);
            if (newB.isNull)
                return Nullable!long.init;
            b = newB.get;
        }
    }

    return nullable(result);
}

/// Safe sum of an array.
Nullable!long safeSum(const long[] values) nothrow @safe
{
    long result = 0;
    foreach (v; values)
    {
        auto newResult = safeAdd(result, v);
        if (newResult.isNull)
            return Nullable!long.init;
        result = newResult.get;
    }
    return nullable(result);
}

/// Safe product of an array.
Nullable!long safeProduct(const long[] values) nothrow @safe
{
    long result = 1;
    foreach (v; values)
    {
        auto newResult = safeMul(result, v);
        if (newResult.isNull)
            return Nullable!long.init;
        result = newResult.get;
    }
    return nullable(result);
}

/// Clamp value to range.
long clamp(long value, long minVal, long maxVal) pure nothrow @safe @nogc
{
    if (value < minVal)
        return minVal;
    if (value > maxVal)
        return maxVal;
    return value;
}

/// Check if value is in range.
bool inRange(long value, long minVal, long maxVal) pure nothrow @safe @nogc
{
    return value >= minVal && value <= maxVal;
}

// Unit tests
unittest
{
    // Test basic operations
    assert(safeAdd(1, 2).get == 3);
    assert(safeSub(5, 3).get == 2);
    assert(safeMul(4, 5).get == 20);
    assert(safeDiv(10, 2).get == 5);

    // Test overflow detection
    assert(safeAdd(long.max, 1).isNull);
    assert(safeMul(long.max, 2).isNull);
    assert(safeDiv(5, 0).isNull);

    // Test array operations
    assert(safeSum([1L, 2L, 3L]).get == 6);
    assert(safeProduct([2L, 3L, 4L]).get == 24);
}
