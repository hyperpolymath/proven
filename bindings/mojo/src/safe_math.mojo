# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeMath -- Overflow-safe integer and floating-point arithmetic.

All computation is delegated to the formally verified Idris 2 core via C FFI.
This module provides Mojo-idiomatic Optional[T] wrappers around the raw
IntResult/FloatResult types from lib_proven.

Functions:
    safe_add       -- Checked addition (overflow detection)
    safe_sub       -- Checked subtraction (underflow detection)
    safe_mul       -- Checked multiplication (overflow detection)
    safe_div       -- Safe integer division (division-by-zero protection)
    safe_mod       -- Safe modulo (division-by-zero protection)
    safe_abs       -- Safe absolute value (INT64_MIN protection)
    safe_pow       -- Integer exponentiation with overflow checking
    clamp          -- Clamp value to [lo, hi] range
    safe_float_div -- Safe floating-point division
    safe_sqrt      -- Safe square root (negative input protection)
    safe_ln        -- Safe natural logarithm (non-positive input protection)
    float_is_finite -- Check if float is finite
    float_is_nan    -- Check if float is NaN
"""

from .lib_proven import (
    IntResult,
    FloatResult,
    proven_math_add_checked,
    proven_math_sub_checked,
    proven_math_mul_checked,
    proven_math_div,
    proven_math_mod,
    proven_math_abs_safe,
    proven_math_clamp,
    proven_math_pow_checked,
    proven_float_div,
    proven_float_is_finite,
    proven_float_is_nan,
    proven_float_sqrt,
    proven_float_ln,
    PROVEN_OK,
    status_to_string,
)


fn safe_add(a: Int64, b: Int64) -> Optional[Int64]:
    """Add two 64-bit integers with overflow detection.

    Returns None if the result would overflow Int64.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_math_add_checked(a, b)
    if result.succeeded():
        return result.value
    return None


fn safe_sub(a: Int64, b: Int64) -> Optional[Int64]:
    """Subtract b from a with underflow detection.

    Returns None if the result would underflow Int64.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_math_sub_checked(a, b)
    if result.succeeded():
        return result.value
    return None


fn safe_mul(a: Int64, b: Int64) -> Optional[Int64]:
    """Multiply two 64-bit integers with overflow detection.

    Returns None if the result would overflow Int64.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_math_mul_checked(a, b)
    if result.succeeded():
        return result.value
    return None


fn safe_div(numerator: Int64, denominator: Int64) -> Optional[Int64]:
    """Divide numerator by denominator safely.

    Returns None if denominator is 0 or if the result would overflow
    (INT64_MIN / -1).
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_math_div(numerator, denominator)
    if result.succeeded():
        return result.value
    return None


fn safe_mod(numerator: Int64, denominator: Int64) -> Optional[Int64]:
    """Compute numerator modulo denominator safely.

    Returns None if denominator is 0.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_math_mod(numerator, denominator)
    if result.succeeded():
        return result.value
    return None


fn safe_abs(n: Int64) -> Optional[Int64]:
    """Compute absolute value safely.

    Returns None for INT64_MIN (whose positive counterpart cannot be
    represented as Int64).
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_math_abs_safe(n)
    if result.succeeded():
        return result.value
    return None


fn safe_pow(base: Int64, exp: UInt32) -> Optional[Int64]:
    """Compute base raised to exp with overflow checking.

    Returns None if the result would overflow Int64 or if exp is negative.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_math_pow_checked(base, exp)
    if result.succeeded():
        return result.value
    return None


fn clamp(lo: Int64, hi: Int64, value: Int64) -> Int64:
    """Clamp value to [lo, hi] range.

    All computation performed in formally verified Idris 2 code.
    """
    return proven_math_clamp(lo, hi, value)


fn safe_float_div(a: Float64, b: Float64) -> Optional[Float64]:
    """Divide a by b safely (floating-point).

    Returns None if b is 0 or if the inputs are NaN.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_float_div(a, b)
    if result.succeeded():
        return result.value
    return None


fn safe_sqrt(x: Float64) -> Optional[Float64]:
    """Compute square root safely.

    Returns None if x is negative or NaN.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_float_sqrt(x)
    if result.succeeded():
        return result.value
    return None


fn safe_ln(x: Float64) -> Optional[Float64]:
    """Compute natural logarithm safely.

    Returns None if x <= 0 or x is NaN.
    All computation performed in formally verified Idris 2 code.
    """
    var result = proven_float_ln(x)
    if result.succeeded():
        return result.value
    return None


fn float_is_finite(x: Float64) -> Bool:
    """Check if float is finite (not NaN, not Inf).

    Delegates to libproven.
    """
    return proven_float_is_finite(x)


fn float_is_nan(x: Float64) -> Bool:
    """Check if float is NaN.

    Delegates to libproven.
    """
    return proven_float_is_nan(x)


# =============================================================================
# Convenience: raw result access (for callers who want the status code)
# =============================================================================


fn safe_add_result(a: Int64, b: Int64) -> IntResult:
    """Checked addition returning the raw IntResult with status code.

    Use this when you need to inspect the specific error (overflow, etc.).
    """
    return proven_math_add_checked(a, b)


fn safe_sub_result(a: Int64, b: Int64) -> IntResult:
    """Checked subtraction returning the raw IntResult with status code."""
    return proven_math_sub_checked(a, b)


fn safe_mul_result(a: Int64, b: Int64) -> IntResult:
    """Checked multiplication returning the raw IntResult with status code."""
    return proven_math_mul_checked(a, b)


fn safe_div_result(numerator: Int64, denominator: Int64) -> IntResult:
    """Safe division returning the raw IntResult with status code."""
    return proven_math_div(numerator, denominator)


fn safe_float_div_result(a: Float64, b: Float64) -> FloatResult:
    """Safe float division returning the raw FloatResult with status code."""
    return proven_float_div(a, b)
