<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath - FFI wrapper for proven_math_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe arithmetic operations that cannot crash or overflow unexpectedly.
 *
 * Every method calls the corresponding libproven FFI function.
 * Returns null on error (division by zero, overflow, underflow).
 */
final class SafeMath
{
    /**
     * Safe division. Returns null on division by zero.
     */
    public static function div(int $a, int $b): ?int
    {
        $result = FFI::getLib()->proven_math_div($a, $b);
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }

    /**
     * Safe modulo. Returns null on division by zero.
     */
    public static function mod(int $a, int $b): ?int
    {
        $result = FFI::getLib()->proven_math_mod($a, $b);
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }

    /**
     * Checked addition. Returns null on overflow.
     */
    public static function addChecked(int $a, int $b): ?int
    {
        $result = FFI::getLib()->proven_math_add_checked($a, $b);
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }

    /**
     * Checked subtraction. Returns null on underflow.
     */
    public static function subChecked(int $a, int $b): ?int
    {
        $result = FFI::getLib()->proven_math_sub_checked($a, $b);
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }

    /**
     * Checked multiplication. Returns null on overflow.
     */
    public static function mulChecked(int $a, int $b): ?int
    {
        $result = FFI::getLib()->proven_math_mul_checked($a, $b);
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }

    /**
     * Safe absolute value. Returns null for MIN_INT.
     */
    public static function absSafe(int $n): ?int
    {
        $result = FFI::getLib()->proven_math_abs_safe($n);
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }

    /**
     * Clamp value to range [lo, hi].
     */
    public static function clamp(int $lo, int $hi, int $value): int
    {
        return (int) FFI::getLib()->proven_math_clamp($lo, $hi, $value);
    }

    /**
     * Integer power with overflow checking. Returns null on overflow.
     */
    public static function powChecked(int $base, int $exp): ?int
    {
        $result = FFI::getLib()->proven_math_pow_checked($base, $exp);
        if ($result->status !== 0) {
            return null;
        }
        return (int) $result->value;
    }
}
