// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Angle conversions and normalization delegated to libproven FFI.
///
/// All computation performed by the formally verified Idris 2 core.

import CProven

public enum SafeAngle {
    /// Convert degrees to radians.
    public static func degToRad(_ degrees: Double) -> Double {
        proven_angle_deg_to_rad(degrees)
    }

    /// Convert radians to degrees.
    public static func radToDeg(_ radians: Double) -> Double {
        proven_angle_rad_to_deg(radians)
    }

    /// Normalize angle to [0, 360) degrees.
    public static func normalizeDegrees(_ degrees: Double) -> Double {
        proven_angle_normalize_degrees(degrees)
    }

    /// Normalize angle to [0, 2*pi) radians.
    public static func normalizeRadians(_ radians: Double) -> Double {
        proven_angle_normalize_radians(radians)
    }
}
