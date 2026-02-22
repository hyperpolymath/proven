// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe arithmetic operations with affine type semantics.
//!
//! All computation is delegated to libproven's formally verified Idris 2
//! core via the FFI bridge. No arithmetic logic is reimplemented here.
//!
//! ## Affine Type Semantics
//!
//! Each result value can be consumed at most once. The Rust ownership
//! system enforces this: once you match on a `ProvenResult<i64>`, the
//! inner value is moved and cannot be reused without explicit cloning.
//! This mirrors Ephapax's affine type system where resources are
//! consumed at most once.

use crate::ffi;
use crate::{int_result_to_proven, ProvenResult};

/// Affine wrapper for a checked integer value.
///
/// `#[affine]` -- this value can be consumed at most once.
/// Once unwrapped, the inner `i64` is moved out.
#[derive(Debug)]
pub struct AffineInt {
    value: i64,
}

impl AffineInt {
    /// Consume this affine value, returning the inner integer.
    ///
    /// After calling this, the `AffineInt` is consumed and cannot be reused.
    pub fn consume(self) -> i64 {
        self.value
    }
}

/// Checked addition with overflow detection.
///
/// Returns the sum as an affine value, or error on overflow.
/// Delegates to `proven_math_add_checked` via FFI.
pub fn safe_add(a: i64, b: i64) -> ProvenResult<AffineInt> {
    // SAFETY: proven_math_add_checked is a pure computation in Idris 2.
    let result = unsafe { ffi::proven_math_add_checked(a, b) };
    int_result_to_proven(result).map(|v| AffineInt { value: v })
}

/// Checked subtraction with underflow detection.
///
/// Returns the difference as an affine value, or error on underflow.
/// Delegates to `proven_math_sub_checked` via FFI.
pub fn safe_sub(a: i64, b: i64) -> ProvenResult<AffineInt> {
    // SAFETY: proven_math_sub_checked is a pure computation in Idris 2.
    let result = unsafe { ffi::proven_math_sub_checked(a, b) };
    int_result_to_proven(result).map(|v| AffineInt { value: v })
}

/// Checked multiplication with overflow detection.
///
/// Returns the product as an affine value, or error on overflow.
/// Delegates to `proven_math_mul_checked` via FFI.
pub fn safe_mul(a: i64, b: i64) -> ProvenResult<AffineInt> {
    // SAFETY: proven_math_mul_checked is a pure computation in Idris 2.
    let result = unsafe { ffi::proven_math_mul_checked(a, b) };
    int_result_to_proven(result).map(|v| AffineInt { value: v })
}

/// Safe division with division-by-zero protection.
///
/// Returns the quotient as an affine value, or error on division by zero.
/// Delegates to `proven_math_div` via FFI.
pub fn safe_div(numerator: i64, denominator: i64) -> ProvenResult<AffineInt> {
    // SAFETY: proven_math_div is a pure computation in Idris 2.
    let result = unsafe { ffi::proven_math_div(numerator, denominator) };
    int_result_to_proven(result).map(|v| AffineInt { value: v })
}

/// Safe modulo with division-by-zero protection.
///
/// Returns the remainder as an affine value, or error on zero divisor.
/// Delegates to `proven_math_mod` via FFI.
pub fn safe_mod(numerator: i64, denominator: i64) -> ProvenResult<AffineInt> {
    // SAFETY: proven_math_mod is a pure computation in Idris 2.
    let result = unsafe { ffi::proven_math_mod(numerator, denominator) };
    int_result_to_proven(result).map(|v| AffineInt { value: v })
}

/// Safe absolute value (handles MIN_INT correctly).
///
/// Returns the absolute value as an affine value, or error for i64::MIN.
/// Delegates to `proven_math_abs_safe` via FFI.
pub fn safe_abs(n: i64) -> ProvenResult<AffineInt> {
    // SAFETY: proven_math_abs_safe is a pure computation in Idris 2.
    let result = unsafe { ffi::proven_math_abs_safe(n) };
    int_result_to_proven(result).map(|v| AffineInt { value: v })
}

/// Safe negation (handles MIN_INT correctly).
///
/// Returns the negated value as an affine value, or error for i64::MIN.
/// Delegates to `proven_math_negate_safe` via FFI.
pub fn safe_negate(n: i64) -> ProvenResult<AffineInt> {
    // SAFETY: proven_math_negate_safe is a pure computation in Idris 2.
    let result = unsafe { ffi::proven_math_negate_safe(n) };
    int_result_to_proven(result).map(|v| AffineInt { value: v })
}

/// Clamp value to range [lo, hi].
///
/// Always succeeds (no error possible). The result is affine.
/// Delegates to `proven_math_clamp` via FFI.
pub fn clamp(lo: i64, hi: i64, value: i64) -> AffineInt {
    // SAFETY: proven_math_clamp always succeeds.
    let v = unsafe { ffi::proven_math_clamp(lo, hi, value) };
    AffineInt { value: v }
}

/// Checked exponentiation with overflow detection.
///
/// Returns the result as an affine value, or error on overflow.
/// Delegates to `proven_math_pow_checked` via FFI.
pub fn safe_pow(base: i64, exp: u32) -> ProvenResult<AffineInt> {
    // SAFETY: proven_math_pow_checked is a pure computation in Idris 2.
    let result = unsafe { ffi::proven_math_pow_checked(base, exp) };
    int_result_to_proven(result).map(|v| AffineInt { value: v })
}
