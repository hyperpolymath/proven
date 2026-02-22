// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe floating-point operations with NaN/Infinity prevention.
 *
 * Thin FFI wrapper around libproven's SafeFloat module. Division,
 * square root, and logarithm safety checks are performed in formally
 * verified Idris 2 code. This module only marshals data to/from the
 * C ABI.
 */
module proven.safe_float;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Safe floating-point division.
/// Returns null on division by zero or NaN input.
Nullable!double safeDiv(double numerator, double denominator) @trusted nothrow @nogc
{
    auto result = proven_float_div(numerator, denominator);
    if (provenFailed(result.status))
        return Nullable!double.init;
    return nullable(result.value);
}

/// Check if a float is finite (not NaN or Infinity).
bool isFiniteValue(double x) @trusted nothrow @nogc
{
    return proven_float_is_finite(x);
}

/// Check if a float is NaN.
bool isNaNValue(double x) @trusted nothrow @nogc
{
    return proven_float_is_nan(x);
}

/// Safe square root.
/// Returns null if x is negative or NaN.
Nullable!double safeSqrt(double x) @trusted nothrow @nogc
{
    auto result = proven_float_sqrt(x);
    if (provenFailed(result.status))
        return Nullable!double.init;
    return nullable(result.value);
}

/// Safe natural logarithm.
/// Returns null if x <= 0 or NaN.
Nullable!double safeLn(double x) @trusted nothrow @nogc
{
    auto result = proven_float_ln(x);
    if (provenFailed(result.status))
        return Nullable!double.init;
    return nullable(result.value);
}
