# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeMath - Arithmetic operations that cannot crash.

All operations handle edge cases like division by zero, overflow, and underflow
without throwing exceptions. Operations return None on failure or use Result types.
"""

from typing import Optional

from .core import ProvenStatus, ProvenError, get_lib, check_status


class SafeMath:
    """Safe arithmetic operations with proven correctness guarantees."""

    @staticmethod
    def div(numerator: int, denominator: int) -> Optional[int]:
        """
        Safe division that returns None on division by zero.

        Args:
            numerator: The dividend
            denominator: The divisor

        Returns:
            The quotient, or None if denominator is 0

        Example:
            >>> SafeMath.div(10, 2)
            5
            >>> SafeMath.div(10, 0)
            None
        """
        lib = get_lib()
        result = lib.proven_math_div(numerator, denominator)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def div_or(default: int, numerator: int, denominator: int) -> int:
        """
        Safe division with a default value for division by zero.

        Args:
            default: Value to return if denominator is 0
            numerator: The dividend
            denominator: The divisor

        Returns:
            The quotient, or default if denominator is 0
        """
        result = SafeMath.div(numerator, denominator)
        return result if result is not None else default

    @staticmethod
    def mod(numerator: int, denominator: int) -> Optional[int]:
        """
        Safe modulo that returns None on division by zero.

        Args:
            numerator: The dividend
            denominator: The divisor

        Returns:
            The remainder, or None if denominator is 0
        """
        lib = get_lib()
        result = lib.proven_math_mod(numerator, denominator)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def add_checked(a: int, b: int) -> Optional[int]:
        """
        Addition with overflow detection.

        Returns None if the result would overflow a 64-bit signed integer.

        Args:
            a: First operand
            b: Second operand

        Returns:
            The sum, or None if overflow would occur

        Example:
            >>> SafeMath.add_checked(5, 3)
            8
            >>> SafeMath.add_checked(2**63 - 1, 1)
            None
        """
        lib = get_lib()
        result = lib.proven_math_add_checked(a, b)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def sub_checked(a: int, b: int) -> Optional[int]:
        """
        Subtraction with underflow detection.

        Returns None if the result would underflow a 64-bit signed integer.

        Args:
            a: First operand
            b: Second operand

        Returns:
            The difference, or None if underflow would occur
        """
        lib = get_lib()
        result = lib.proven_math_sub_checked(a, b)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def mul_checked(a: int, b: int) -> Optional[int]:
        """
        Multiplication with overflow detection.

        Returns None if the result would overflow a 64-bit signed integer.

        Args:
            a: First operand
            b: Second operand

        Returns:
            The product, or None if overflow would occur
        """
        lib = get_lib()
        result = lib.proven_math_mul_checked(a, b)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def abs_safe(n: int) -> Optional[int]:
        """
        Safe absolute value that handles MIN_INT correctly.

        Regular abs(MIN_INT) overflows because -MIN_INT > MAX_INT.
        This version returns None in that case.

        Args:
            n: The integer

        Returns:
            |n|, or None if n is MIN_INT (cannot be represented)
        """
        lib = get_lib()
        result = lib.proven_math_abs_safe(n)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def abs_clamped(n: int) -> int:
        """
        Absolute value that clamps to MAX_INT instead of overflowing.

        Args:
            n: The integer

        Returns:
            |n|, or MAX_INT if n is MIN_INT
        """
        result = SafeMath.abs_safe(n)
        if result is None:
            return (2**63) - 1  # MAX_INT64
        return result

    @staticmethod
    def clamp(lo: int, hi: int, value: int) -> int:
        """
        Clamp a value to range [lo, hi].

        Args:
            lo: Lower bound (inclusive)
            hi: Upper bound (inclusive)
            value: Value to clamp

        Returns:
            value if lo <= value <= hi, else lo or hi

        Example:
            >>> SafeMath.clamp(0, 100, 50)
            50
            >>> SafeMath.clamp(0, 100, 150)
            100
            >>> SafeMath.clamp(0, 100, -10)
            0
        """
        lib = get_lib()
        return lib.proven_math_clamp(lo, hi, value)

    @staticmethod
    def pow_checked(base: int, exp: int) -> Optional[int]:
        """
        Integer exponentiation with overflow detection.

        Args:
            base: The base
            exp: The exponent (must be non-negative)

        Returns:
            base^exp, or None if overflow would occur

        Raises:
            ValueError: If exp is negative
        """
        if exp < 0:
            raise ValueError("Exponent must be non-negative")

        lib = get_lib()
        result = lib.proven_math_pow_checked(base, exp)
        if result.status != ProvenStatus.OK:
            return None
        return result.value

    @staticmethod
    def percent_of(percent: int, total: int) -> Optional[int]:
        """
        Calculate percentage safely.

        Args:
            percent: The percentage (e.g., 50 for 50%)
            total: The total value

        Returns:
            percent% of total, or None on overflow/division-by-zero

        Example:
            >>> SafeMath.percent_of(50, 200)
            100
        """
        product = SafeMath.mul_checked(percent, total)
        if product is None:
            return None
        return SafeMath.div(product, 100)

    @staticmethod
    def as_percent(part: int, whole: int) -> Optional[int]:
        """
        Calculate what percentage part is of whole.

        Args:
            part: The part
            whole: The whole

        Returns:
            The percentage (0-100+), or None on division by zero

        Example:
            >>> SafeMath.as_percent(50, 200)
            25
        """
        scaled = SafeMath.mul_checked(part, 100)
        if scaled is None:
            return None
        return SafeMath.div(scaled, whole)
