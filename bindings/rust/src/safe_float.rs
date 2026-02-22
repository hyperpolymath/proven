// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe floating-point operations via libproven FFI.
//!
//! Prevents NaN/Infinity propagation, provides safe division, square root,
//! and logarithm. All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Safe floating-point operations.
pub struct SafeFloat;

impl SafeFloat {
    /// Safe floating-point division.
    ///
    /// Returns `Err(Error::DivisionByZero)` if the divisor is zero.
    /// Returns `Err` for operations producing NaN or Infinity.
    pub fn div(a: f64, b: f64) -> Result<f64> {
        // SAFETY: proven_float_div takes value-type f64 arguments;
        // always safe to call.
        let result = unsafe { ffi::proven_float_div(a, b) };
        core::float_result_to_result(result)
    }

    /// Check if a float is finite (not NaN or Infinity).
    pub fn is_finite(x: f64) -> bool {
        // SAFETY: proven_float_is_finite takes a value-type f64;
        // always safe to call.
        unsafe { ffi::proven_float_is_finite(x) }
    }

    /// Check if a float is NaN.
    pub fn is_nan(x: f64) -> bool {
        // SAFETY: proven_float_is_nan takes a value-type f64;
        // always safe to call.
        unsafe { ffi::proven_float_is_nan(x) }
    }

    /// Safe square root.
    ///
    /// Returns `Err` for negative inputs (which would produce NaN).
    pub fn sqrt(x: f64) -> Result<f64> {
        // SAFETY: proven_float_sqrt takes a value-type f64;
        // always safe to call.
        let result = unsafe { ffi::proven_float_sqrt(x) };
        core::float_result_to_result(result)
    }

    /// Safe natural logarithm.
    ///
    /// Returns `Err` for non-positive inputs.
    pub fn ln(x: f64) -> Result<f64> {
        // SAFETY: proven_float_ln takes a value-type f64;
        // always safe to call.
        let result = unsafe { ffi::proven_float_ln(x) };
        core::float_result_to_result(result)
    }
}
