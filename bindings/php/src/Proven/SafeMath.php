<?php
// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Safe arithmetic operations that cannot crash or overflow unexpectedly.
 *
 * All operations handle edge cases like division by zero and overflow
 * without throwing exceptions. Operations return null on failure.
 */
class SafeMath
{
    /** @var int Maximum 64-bit signed integer */
    public const MAX_INT = PHP_INT_MAX;

    /** @var int Minimum 64-bit signed integer */
    public const MIN_INT = PHP_INT_MIN;

    /**
     * Safe division that returns null on division by zero.
     *
     * @param int $numerator The dividend
     * @param int $denominator The divisor
     * @return int|null The quotient, or null if denominator is 0
     */
    public static function div(int $numerator, int $denominator): ?int
    {
        if ($denominator === 0) {
            return null;
        }
        return intdiv($numerator, $denominator);
    }

    /**
     * Safe division with a default value for division by zero.
     *
     * @param int $default Value to return if denominator is 0
     * @param int $numerator The dividend
     * @param int $denominator The divisor
     * @return int The quotient, or default if denominator is 0
     */
    public static function divOr(int $default, int $numerator, int $denominator): int
    {
        return self::div($numerator, $denominator) ?? $default;
    }

    /**
     * Safe modulo that returns null on division by zero.
     *
     * @param int $numerator The dividend
     * @param int $denominator The divisor
     * @return int|null The remainder, or null if denominator is 0
     */
    public static function mod(int $numerator, int $denominator): ?int
    {
        if ($denominator === 0) {
            return null;
        }
        return $numerator % $denominator;
    }

    /**
     * Addition with overflow detection.
     *
     * @param int $a First operand
     * @param int $b Second operand
     * @return int|null The sum, or null if overflow would occur
     */
    public static function addChecked(int $a, int $b): ?int
    {
        // Check for overflow before performing operation
        if ($b > 0 && $a > self::MAX_INT - $b) {
            return null;
        }
        if ($b < 0 && $a < self::MIN_INT - $b) {
            return null;
        }
        return $a + $b;
    }

    /**
     * Subtraction with underflow detection.
     *
     * @param int $a First operand
     * @param int $b Second operand
     * @return int|null The difference, or null if underflow would occur
     */
    public static function subChecked(int $a, int $b): ?int
    {
        // Check for underflow before performing operation
        if ($b > 0 && $a < self::MIN_INT + $b) {
            return null;
        }
        if ($b < 0 && $a > self::MAX_INT + $b) {
            return null;
        }
        return $a - $b;
    }

    /**
     * Multiplication with overflow detection.
     *
     * @param int $a First operand
     * @param int $b Second operand
     * @return int|null The product, or null if overflow would occur
     */
    public static function mulChecked(int $a, int $b): ?int
    {
        if ($a === 0 || $b === 0) {
            return 0;
        }

        // Check for overflow
        if ($a > 0) {
            if ($b > 0) {
                if ($a > intdiv(self::MAX_INT, $b)) {
                    return null;
                }
            } else {
                if ($b < intdiv(self::MIN_INT, $a)) {
                    return null;
                }
            }
        } else {
            if ($b > 0) {
                if ($a < intdiv(self::MIN_INT, $b)) {
                    return null;
                }
            } else {
                if ($a !== 0 && $b < intdiv(self::MAX_INT, $a)) {
                    return null;
                }
            }
        }

        return $a * $b;
    }

    /**
     * Safe absolute value that handles MIN_INT correctly.
     *
     * @param int $n The integer
     * @return int|null |n|, or null if n is MIN_INT (cannot be represented)
     */
    public static function absSafe(int $n): ?int
    {
        if ($n === self::MIN_INT) {
            return null;
        }
        return abs($n);
    }

    /**
     * Absolute value that clamps to MAX_INT instead of overflowing.
     *
     * @param int $n The integer
     * @return int |n|, or MAX_INT if n is MIN_INT
     */
    public static function absClamped(int $n): int
    {
        return self::absSafe($n) ?? self::MAX_INT;
    }

    /**
     * Clamp a value to range [lo, hi].
     *
     * @param int $lo Lower bound (inclusive)
     * @param int $hi Upper bound (inclusive)
     * @param int $value Value to clamp
     * @return int value if lo <= value <= hi, else lo or hi
     */
    public static function clamp(int $lo, int $hi, int $value): int
    {
        if ($value < $lo) {
            return $lo;
        }
        if ($value > $hi) {
            return $hi;
        }
        return $value;
    }

    /**
     * Integer exponentiation with overflow detection.
     *
     * @param int $base The base
     * @param int $exp The exponent (must be non-negative)
     * @return int|null base^exp, or null if overflow would occur
     * @throws \InvalidArgumentException If exp is negative
     */
    public static function powChecked(int $base, int $exp): ?int
    {
        if ($exp < 0) {
            throw new \InvalidArgumentException('Exponent must be non-negative');
        }

        if ($exp === 0) {
            return 1;
        }

        $result = 1;
        $currentBase = $base;
        $currentExp = $exp;

        while ($currentExp > 0) {
            if ($currentExp & 1) {
                $newResult = self::mulChecked($result, $currentBase);
                if ($newResult === null) {
                    return null;
                }
                $result = $newResult;
            }
            $currentExp >>= 1;
            if ($currentExp > 0) {
                $newBase = self::mulChecked($currentBase, $currentBase);
                if ($newBase === null) {
                    return null;
                }
                $currentBase = $newBase;
            }
        }

        return $result;
    }

    /**
     * Calculate percentage safely.
     *
     * @param int $percent The percentage (e.g., 50 for 50%)
     * @param int $total The total value
     * @return int|null percent% of total, or null on overflow/division-by-zero
     */
    public static function percentOf(int $percent, int $total): ?int
    {
        $product = self::mulChecked($percent, $total);
        if ($product === null) {
            return null;
        }
        return self::div($product, 100);
    }

    /**
     * Calculate what percentage part is of whole.
     *
     * @param int $part The part
     * @param int $whole The whole
     * @return int|null The percentage (0-100+), or null on division by zero
     */
    public static function asPercent(int $part, int $whole): ?int
    {
        $scaled = self::mulChecked($part, 100);
        if ($scaled === null) {
            return null;
        }
        return self::div($scaled, $whole);
    }
}
