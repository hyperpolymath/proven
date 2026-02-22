# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# cython: language_level=3

"""
SafeMath - Arithmetic operations that cannot crash.

All functions delegate to libproven via direct C FFI calls.
Returns None on error (overflow, underflow, division by zero).
NEVER reimplements logic.
"""

cimport proven.lib_proven as c


def safe_add(long long a, long long b):
    """Checked addition. Returns None on overflow.

    >>> safe_add(5, 3)
    8
    >>> safe_add(2**62, 2**62) is None
    True
    """
    cdef c.ProvenIntResult result
    with nogil:
        result = c.proven_math_add_checked(a, b)
    if result.status == c.PROVEN_OK:
        return result.value
    return None


def safe_sub(long long a, long long b):
    """Checked subtraction. Returns None on underflow.

    >>> safe_sub(10, 3)
    7
    """
    cdef c.ProvenIntResult result
    with nogil:
        result = c.proven_math_sub_checked(a, b)
    if result.status == c.PROVEN_OK:
        return result.value
    return None


def safe_mul(long long a, long long b):
    """Checked multiplication. Returns None on overflow.

    >>> safe_mul(6, 7)
    42
    """
    cdef c.ProvenIntResult result
    with nogil:
        result = c.proven_math_mul_checked(a, b)
    if result.status == c.PROVEN_OK:
        return result.value
    return None


def safe_div(long long numerator, long long denominator):
    """Safe integer division. Returns None on division by zero or overflow.

    >>> safe_div(10, 2)
    5
    >>> safe_div(10, 0) is None
    True
    """
    cdef c.ProvenIntResult result
    with nogil:
        result = c.proven_math_div(numerator, denominator)
    if result.status == c.PROVEN_OK:
        return result.value
    return None


def safe_mod(long long numerator, long long denominator):
    """Safe modulo. Returns None on division by zero.

    >>> safe_mod(10, 3)
    1
    >>> safe_mod(10, 0) is None
    True
    """
    cdef c.ProvenIntResult result
    with nogil:
        result = c.proven_math_mod(numerator, denominator)
    if result.status == c.PROVEN_OK:
        return result.value
    return None


def safe_abs(long long n):
    """Safe absolute value. Returns None for INT64_MIN.

    >>> safe_abs(-42)
    42
    """
    cdef c.ProvenIntResult result
    with nogil:
        result = c.proven_math_abs_safe(n)
    if result.status == c.PROVEN_OK:
        return result.value
    return None


def safe_negate(long long n):
    """Safe negation via checked subtraction from zero. Returns None on overflow.

    >>> safe_negate(5)
    -5
    """
    return safe_sub(0, n)


def clamp(long long lo, long long hi, long long value):
    """Clamp value to [lo, hi] range.

    >>> clamp(0, 100, 50)
    50
    >>> clamp(0, 100, 150)
    100
    >>> clamp(0, 100, -10)
    0
    """
    cdef long long result
    with nogil:
        result = c.proven_math_clamp(lo, hi, value)
    return result


def safe_pow(long long base, unsigned int exp):
    """Checked integer exponentiation. Returns None on overflow.

    >>> safe_pow(2, 10)
    1024
    """
    cdef c.ProvenIntResult result
    with nogil:
        result = c.proven_math_pow_checked(base, exp)
    if result.status == c.PROVEN_OK:
        return result.value
    return None
