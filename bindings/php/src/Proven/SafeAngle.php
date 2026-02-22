<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeAngle - FFI wrapper for proven_angle_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe angle conversion and normalization via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeAngle
{
    /**
     * Convert degrees to radians.
     */
    public static function degToRad(float $degrees): float
    {
        return FFI::getLib()->proven_angle_deg_to_rad($degrees);
    }

    /**
     * Convert radians to degrees.
     */
    public static function radToDeg(float $radians): float
    {
        return FFI::getLib()->proven_angle_rad_to_deg($radians);
    }

    /**
     * Normalize an angle in degrees to [0, 360).
     */
    public static function normalizeDegrees(float $degrees): float
    {
        return FFI::getLib()->proven_angle_normalize_degrees($degrees);
    }

    /**
     * Normalize an angle in radians to [0, 2*pi).
     */
    public static function normalizeRadians(float $radians): float
    {
        return FFI::getLib()->proven_angle_normalize_radians($radians);
    }
}
