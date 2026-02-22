// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe angle operations via libproven FFI.
//!
//! Converts between degrees and radians, normalizes angles.
//! All operations delegate to Idris 2 verified code.

use crate::ffi;

/// Angle in degrees.
pub type Degrees = f64;

/// Angle in radians.
pub type Radians = f64;

/// Convert degrees to radians.
pub fn deg_to_rad(degrees: Degrees) -> Radians {
    // SAFETY: proven_angle_deg_to_rad takes a value-type f64;
    // always safe to call.
    unsafe { ffi::proven_angle_deg_to_rad(degrees) }
}

/// Convert radians to degrees.
pub fn rad_to_deg(radians: Radians) -> Degrees {
    // SAFETY: proven_angle_rad_to_deg takes a value-type f64;
    // always safe to call.
    unsafe { ffi::proven_angle_rad_to_deg(radians) }
}

/// Normalize angle to [0, 360) degrees.
pub fn normalize_degrees(degrees: Degrees) -> Degrees {
    // SAFETY: proven_angle_normalize_degrees takes a value-type f64;
    // always safe to call.
    unsafe { ffi::proven_angle_normalize_degrees(degrees) }
}

/// Normalize angle to [0, 2*pi) radians.
pub fn normalize_radians(radians: Radians) -> Radians {
    // SAFETY: proven_angle_normalize_radians takes a value-type f64;
    // always safe to call.
    unsafe { ffi::proven_angle_normalize_radians(radians) }
}

/// Linearly interpolate between two angles in degrees.
///
/// Takes the shortest path around the circle.
pub fn lerp_angle_degrees(a: Degrees, b: Degrees, t: f64) -> Degrees {
    let a_norm = normalize_degrees(a);
    let b_norm = normalize_degrees(b);
    let mut diff = b_norm - a_norm;
    if diff > 180.0 {
        diff -= 360.0;
    } else if diff < -180.0 {
        diff += 360.0;
    }
    normalize_degrees(a_norm + diff * t)
}
