// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe arithmetic operations via libproven FFI.
//!
//! All operations delegate to the Idris 2 verified implementation through
//! the Zig FFI layer. No arithmetic logic is implemented in Rust.

use crate::core::{self, Result};
use crate::ffi;

/// Safe math operations that cannot crash.
///
/// All methods call the formally verified Idris 2 implementation via FFI.
pub struct SafeMath;

impl SafeMath {
    /// Safe addition with overflow detection.
    ///
    /// Returns `Err(Error::Overflow)` if the result would exceed `i64::MAX`.
    pub fn add(a: i64, b: i64) -> Result<i64> {
        // SAFETY: proven_math_add_checked is a pure function with no
        // pointer arguments; always safe to call.
        let result = unsafe { ffi::proven_math_add_checked(a, b) };
        core::int_result_to_result(result)
    }

    /// Safe subtraction with underflow detection.
    ///
    /// Returns `Err(Error::Underflow)` if the result would go below `i64::MIN`.
    pub fn sub(a: i64, b: i64) -> Result<i64> {
        // SAFETY: proven_math_sub_checked is a pure function with no
        // pointer arguments; always safe to call.
        let result = unsafe { ffi::proven_math_sub_checked(a, b) };
        core::int_result_to_result(result)
    }

    /// Safe multiplication with overflow detection.
    ///
    /// Returns `Err(Error::Overflow)` if the result would overflow.
    pub fn mul(a: i64, b: i64) -> Result<i64> {
        // SAFETY: proven_math_mul_checked is a pure function with no
        // pointer arguments; always safe to call.
        let result = unsafe { ffi::proven_math_mul_checked(a, b) };
        core::int_result_to_result(result)
    }

    /// Safe division with zero-check.
    ///
    /// Returns `Err(Error::DivisionByZero)` if denominator is 0.
    pub fn div(numerator: i64, denominator: i64) -> Result<i64> {
        // SAFETY: proven_math_div is a pure function with no
        // pointer arguments; always safe to call.
        let result = unsafe { ffi::proven_math_div(numerator, denominator) };
        core::int_result_to_result(result)
    }

    /// Safe modulo with zero-check.
    ///
    /// Returns `Err(Error::DivisionByZero)` if denominator is 0.
    pub fn modulo(numerator: i64, denominator: i64) -> Result<i64> {
        // SAFETY: proven_math_mod is a pure function with no
        // pointer arguments; always safe to call.
        let result = unsafe { ffi::proven_math_mod(numerator, denominator) };
        core::int_result_to_result(result)
    }

    /// Safe absolute value that handles `i64::MIN` correctly.
    ///
    /// Returns `Err(Error::Overflow)` for `i64::MIN` (since its absolute
    /// value cannot be represented as i64).
    pub fn abs(n: i64) -> Result<i64> {
        // SAFETY: proven_math_abs_safe is a pure function with no
        // pointer arguments; always safe to call.
        let result = unsafe { ffi::proven_math_abs_safe(n) };
        core::int_result_to_result(result)
    }

    /// Clamp value to range [lo, hi].
    ///
    /// Always succeeds; returns lo if value < lo, hi if value > hi.
    pub fn clamp(lo: i64, hi: i64, value: i64) -> i64 {
        // SAFETY: proven_math_clamp is a pure function with no
        // pointer arguments; always safe to call.
        unsafe { ffi::proven_math_clamp(lo, hi, value) }
    }

    /// Integer power with overflow checking.
    ///
    /// Returns `Err(Error::Overflow)` if the result would overflow.
    pub fn pow(base: i64, exp: u32) -> Result<i64> {
        // SAFETY: proven_math_pow_checked is a pure function with no
        // pointer arguments; always safe to call.
        let result = unsafe { ffi::proven_math_pow_checked(base, exp) };
        core::int_result_to_result(result)
    }
}
