// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeAngle - Angle operations that cannot crash.
////
//// Thin FFI wrapper over libproven proven_angle_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Convert degrees to radians.
@external(erlang, "proven_nif", "angle_deg_to_rad")
pub fn deg_to_rad(degrees: Float) -> Float

/// Convert radians to degrees.
@external(erlang, "proven_nif", "angle_rad_to_deg")
pub fn rad_to_deg(radians: Float) -> Float

/// Normalize an angle to [0, 360) degrees.
/// Returns 0.0 for NaN input.
@external(erlang, "proven_nif", "angle_normalize_degrees")
pub fn normalize_degrees(degrees: Float) -> Float

/// Normalize an angle to [0, 2*pi) radians.
/// Returns 0.0 for NaN input.
@external(erlang, "proven_nif", "angle_normalize_radians")
pub fn normalize_radians(radians: Float) -> Float
