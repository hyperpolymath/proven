// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe angle conversions and normalization via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeAngle =

    /// Convert degrees to radians.
    let degToRad (degrees: float) : float =
        FFI.proven_angle_deg_to_rad(degrees)

    /// Convert radians to degrees.
    let radToDeg (radians: float) : float =
        FFI.proven_angle_rad_to_deg(radians)

    /// Normalize angle to [0, 360) degrees.
    let normalizeDegrees (degrees: float) : float =
        FFI.proven_angle_normalize_degrees(degrees)

    /// Normalize angle to [0, 2*pi) radians.
    let normalizeRadians (radians: float) : float =
        FFI.proven_angle_normalize_radians(radians)
