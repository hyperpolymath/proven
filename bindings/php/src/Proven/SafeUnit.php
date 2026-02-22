<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUnit - FFI wrapper for proven_unit_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe unit conversion via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 * Unit enum values are integers defined by the libproven C ABI.
 */
final class SafeUnit
{
    /**
     * Convert a length value between units.
     *
     * @param float $value The value to convert.
     * @param int   $from  Source unit enum value.
     * @param int   $to    Target unit enum value.
     * @return float|null The converted value, or null on error.
     */
    public static function convertLength(float $value, int $from, int $to): ?float
    {
        $result = FFI::getLib()->proven_unit_convert_length($value, $from, $to);
        if ($result->status !== 0) {
            return null;
        }
        return (float) $result->value;
    }

    /**
     * Convert a temperature value between units.
     *
     * @param float $value The temperature to convert.
     * @param int   $from  Source unit enum value.
     * @param int   $to    Target unit enum value.
     * @return float|null The converted value, or null on error.
     */
    public static function convertTemp(float $value, int $from, int $to): ?float
    {
        $result = FFI::getLib()->proven_unit_convert_temp($value, $from, $to);
        if ($result->status !== 0) {
            return null;
        }
        return (float) $result->value;
    }
}
