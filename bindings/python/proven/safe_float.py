# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeFloat - Floating point operations without NaN/Infinity surprises.

Provides safe floating-point operations with NaN and Infinity prevention.
All operations are delegated to the Idris core via FFI.
"""

from typing import Optional

from .core import ProvenStatus, get_lib


class SafeFloat:
    """Safe floating-point operations via FFI."""

    @staticmethod
    def is_finite(value: float) -> bool:
        """
        Check if value is finite (not NaN or Infinity) via FFI.

        Args:
            value: Float to check

        Returns:
            True if finite
        """
        lib = get_lib()
        result = lib.proven_float_is_finite(value)
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def is_nan(value: float) -> bool:
        """Check if value is NaN."""
        return not SafeFloat.is_finite(value) and value != value

    @staticmethod
    def is_inf(value: float) -> bool:
        """Check if value is positive or negative infinity."""
        return not SafeFloat.is_finite(value) and value == value

    @staticmethod
    def div(numerator: float, denominator: float) -> Optional[float]:
        """
        Safe division that returns None instead of Infinity or NaN via FFI.

        Args:
            numerator: The dividend
            denominator: The divisor

        Returns:
            Quotient, or None if result would be NaN or Infinity

        Example:
            >>> SafeFloat.div(10.0, 2.0)
            5.0
            >>> SafeFloat.div(1.0, 0.0)
            None
        """
        lib = get_lib()
        result = lib.proven_float_div(numerator, denominator)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def add(a: float, b: float) -> Optional[float]:
        """
        Safe addition that returns None on overflow via FFI.

        Args:
            a: First operand
            b: Second operand

        Returns:
            Sum, or None if result is infinite
        """
        lib = get_lib()
        result = lib.proven_float_add(a, b)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def sub(a: float, b: float) -> Optional[float]:
        """Safe subtraction that returns None on overflow via FFI."""
        lib = get_lib()
        result = lib.proven_float_sub(a, b)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def mul(a: float, b: float) -> Optional[float]:
        """Safe multiplication that returns None on overflow via FFI."""
        lib = get_lib()
        result = lib.proven_float_mul(a, b)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def sqrt(value: float) -> Optional[float]:
        """
        Safe square root that returns None for negative numbers via FFI.

        Args:
            value: Value to take square root of

        Returns:
            Square root, or None if negative
        """
        lib = get_lib()
        result = lib.proven_float_sqrt(value)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def log(value: float) -> Optional[float]:
        """
        Safe natural logarithm that returns None for non-positive numbers via FFI.

        Args:
            value: Value to take log of

        Returns:
            Natural log, or None if value <= 0
        """
        lib = get_lib()
        result = lib.proven_float_log(value)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def log10(value: float) -> Optional[float]:
        """Safe base-10 logarithm via FFI."""
        lib = get_lib()
        result = lib.proven_float_log10(value)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def pow(base: float, exp: float) -> Optional[float]:
        """
        Safe exponentiation that returns None on invalid results via FFI.

        Args:
            base: Base
            exp: Exponent

        Returns:
            Result, or None if NaN or Infinity
        """
        lib = get_lib()
        result = lib.proven_float_pow(base, exp)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def clamp(value: float, lo: float, hi: float) -> float:
        """
        Clamp value to range [lo, hi] via FFI.

        Handles NaN by returning lo.

        Args:
            value: Value to clamp
            lo: Lower bound
            hi: Upper bound

        Returns:
            Clamped value
        """
        lib = get_lib()
        result = lib.proven_float_clamp(value, lo, hi)
        if result.status != ProvenStatus.OK:
            return lo
        return result.value

    @staticmethod
    def finite_or(value: float, default: float) -> float:
        """
        Return value if finite, otherwise return default.

        Args:
            value: Value to check
            default: Default to use if not finite

        Returns:
            value if finite, else default
        """
        if SafeFloat.is_finite(value):
            return value
        return default

    @staticmethod
    def approximately_equal(a: float, b: float, epsilon: float = 1e-9) -> bool:
        """
        Check if two floats are approximately equal via FFI.

        Args:
            a: First value
            b: Second value
            epsilon: Maximum allowed difference

        Returns:
            True if |a - b| <= epsilon
        """
        lib = get_lib()
        result = lib.proven_float_approx_eq(a, b, epsilon)
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    @staticmethod
    def lerp(a: float, b: float, t: float) -> Optional[float]:
        """
        Linear interpolation between a and b via FFI.

        Args:
            a: Start value
            b: End value
            t: Interpolation factor (0-1)

        Returns:
            Interpolated value, or None if result is not finite
        """
        lib = get_lib()
        result = lib.proven_float_lerp(a, b, t)
        if result.status != ProvenStatus.OK:
            return None
        return result.value
