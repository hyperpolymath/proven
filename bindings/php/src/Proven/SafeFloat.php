<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeFloat - FFI wrapper for proven_float_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe floating-point operations via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeFloat
{
    /**
     * Safe division. Returns null on division by zero or non-finite result.
     */
    public static function div(float $a, float $b): ?float
    {
        $result = FFI::getLib()->proven_float_div($a, $b);
        if ($result->status !== 0) {
            return null;
        }
        return (float) $result->value;
    }

    /**
     * Check if a value is finite (not NaN, not Inf).
     */
    public static function isFinite(float $x): bool
    {
        return FFI::getLib()->proven_float_is_finite($x);
    }

    /**
     * Check if a value is NaN.
     */
    public static function isNaN(float $x): bool
    {
        return FFI::getLib()->proven_float_is_nan($x);
    }

    /**
     * Safe square root. Returns null for negative inputs.
     */
    public static function sqrt(float $x): ?float
    {
        $result = FFI::getLib()->proven_float_sqrt($x);
        if ($result->status !== 0) {
            return null;
        }
        return (float) $result->value;
    }

    /**
     * Safe natural logarithm. Returns null for non-positive inputs.
     */
    public static function ln(float $x): ?float
    {
        $result = FFI::getLib()->proven_float_ln($x);
        if ($result->status !== 0) {
            return null;
        }
        return (float) $result->value;
    }
}
