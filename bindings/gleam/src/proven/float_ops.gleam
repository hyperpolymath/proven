// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeFloat - Floating point operations that cannot crash.
////
//// Thin FFI wrapper over libproven proven_float_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Safe floating-point division.
/// Returns Error on division by zero, NaN input, or overflow.
@external(erlang, "proven_nif", "float_div")
pub fn div(a: Float, b: Float) -> Result(Float, String)

/// Safe square root.
/// Returns Error if input is negative or NaN.
@external(erlang, "proven_nif", "float_sqrt")
pub fn sqrt(x: Float) -> Result(Float, String)

/// Safe natural logarithm.
/// Returns Error if input is non-positive or NaN.
@external(erlang, "proven_nif", "float_ln")
pub fn ln(x: Float) -> Result(Float, String)

/// Check if a float is finite (not NaN or Infinity).
@external(erlang, "proven_nif", "float_is_finite")
pub fn is_finite(x: Float) -> Bool

/// Check if a float is NaN.
@external(erlang, "proven_nif", "float_is_nan")
pub fn is_nan(x: Float) -> Bool
