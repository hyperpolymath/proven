// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe angle operations with unit conversions and normalization.
 *
 * Thin FFI wrapper around libproven's SafeAngle module. Angle
 * conversion and normalization are performed in formally verified
 * Idris 2 code. This module only marshals data to/from the C ABI.
 */
module proven.safe_angle;

import proven.ffi;

pragma(lib, "proven");

/// Convert degrees to radians.
double degToRad(double degrees) @trusted nothrow @nogc
{
    return proven_angle_deg_to_rad(degrees);
}

/// Convert radians to degrees.
double radToDeg(double radians) @trusted nothrow @nogc
{
    return proven_angle_rad_to_deg(radians);
}

/// Normalize angle to [0, 360) degrees. Returns 0 for NaN.
double normalizeDegrees(double degrees) @trusted nothrow @nogc
{
    return proven_angle_normalize_degrees(degrees);
}

/// Normalize angle to [0, 2*pi) radians. Returns 0 for NaN.
double normalizeRadians(double radians) @trusted nothrow @nogc
{
    return proven_angle_normalize_radians(radians);
}
