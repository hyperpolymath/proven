# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeFloat - Floating point operations without NaN/Infinity surprises.

Provides safe floating-point operations with NaN and Infinity prevention.
"""

from typing import Optional
import math


class SafeFloat:
    """Safe floating-point operations."""

    @staticmethod
    def is_finite(value: float) -> bool:
        """
        Check if value is finite (not NaN or Infinity).

        Args:
            value: Float to check

        Returns:
            True if finite
        """
        return math.isfinite(value)

    @staticmethod
    def is_nan(value: float) -> bool:
        """Check if value is NaN."""
        return math.isnan(value)

    @staticmethod
    def is_inf(value: float) -> bool:
        """Check if value is positive or negative infinity."""
        return math.isinf(value)

    @staticmethod
    def div(numerator: float, denominator: float) -> Optional[float]:
        """
        Safe division that returns None instead of Infinity or NaN.

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
        if denominator == 0.0:
            return None
        result = numerator / denominator
        if not math.isfinite(result):
            return None
        return result

    @staticmethod
    def add(a: float, b: float) -> Optional[float]:
        """
        Safe addition that returns None on overflow.

        Args:
            a: First operand
            b: Second operand

        Returns:
            Sum, or None if result is infinite
        """
        result = a + b
        if not math.isfinite(result):
            return None
        return result

    @staticmethod
    def sub(a: float, b: float) -> Optional[float]:
        """Safe subtraction that returns None on overflow."""
        result = a - b
        if not math.isfinite(result):
            return None
        return result

    @staticmethod
    def mul(a: float, b: float) -> Optional[float]:
        """Safe multiplication that returns None on overflow."""
        result = a * b
        if not math.isfinite(result):
            return None
        return result

    @staticmethod
    def sqrt(value: float) -> Optional[float]:
        """
        Safe square root that returns None for negative numbers.

        Args:
            value: Value to take square root of

        Returns:
            Square root, or None if negative
        """
        if value < 0:
            return None
        return math.sqrt(value)

    @staticmethod
    def log(value: float) -> Optional[float]:
        """
        Safe natural logarithm that returns None for non-positive numbers.

        Args:
            value: Value to take log of

        Returns:
            Natural log, or None if value <= 0
        """
        if value <= 0:
            return None
        return math.log(value)

    @staticmethod
    def log10(value: float) -> Optional[float]:
        """Safe base-10 logarithm."""
        if value <= 0:
            return None
        return math.log10(value)

    @staticmethod
    def pow(base: float, exp: float) -> Optional[float]:
        """
        Safe exponentiation that returns None on invalid results.

        Args:
            base: Base
            exp: Exponent

        Returns:
            Result, or None if NaN or Infinity
        """
        try:
            result = math.pow(base, exp)
            if not math.isfinite(result):
                return None
            return result
        except (ValueError, OverflowError):
            return None

    @staticmethod
    def clamp(value: float, lo: float, hi: float) -> float:
        """
        Clamp value to range [lo, hi].

        Handles NaN by returning lo.

        Args:
            value: Value to clamp
            lo: Lower bound
            hi: Upper bound

        Returns:
            Clamped value
        """
        if math.isnan(value):
            return lo
        return max(lo, min(hi, value))

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
        return value if math.isfinite(value) else default

    @staticmethod
    def approximately_equal(a: float, b: float, epsilon: float = 1e-9) -> bool:
        """
        Check if two floats are approximately equal.

        Args:
            a: First value
            b: Second value
            epsilon: Maximum allowed difference

        Returns:
            True if |a - b| <= epsilon
        """
        if not math.isfinite(a) or not math.isfinite(b):
            return False
        return abs(a - b) <= epsilon

    @staticmethod
    def lerp(a: float, b: float, t: float) -> Optional[float]:
        """
        Linear interpolation between a and b.

        Args:
            a: Start value
            b: End value
            t: Interpolation factor (0-1)

        Returns:
            Interpolated value, or None if result is not finite
        """
        result = a + (b - a) * t
        if not math.isfinite(result):
            return None
        return result
