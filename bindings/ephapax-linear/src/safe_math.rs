// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Linear safe arithmetic operations via libproven FFI.
//!
//! All operations delegate to the Idris 2 verified implementation through
//! the Zig FFI layer. No arithmetic logic is implemented in Rust.
//!
//! ## Linear Type Semantics
//!
//! [`LinearInt`] and [`LinearFloat`] are linear wrappers around numeric values.
//! They have **no `Drop` implementation** and **no `Clone`/`Copy`**. The caller
//! must explicitly consume each value via:
//!
//! - `.into_inner()` to extract the raw value
//! - A consuming arithmetic method (e.g., `.add()`, `.sub()`) that produces
//!   a new linear value
//! - `.consume()` to discard the value explicitly
//!
//! Failing to consume a `LinearInt` produces a compiler warning (via `#[must_use]`).
//! In Ephapax's type checker, this would be a hard error; in Rust we approximate
//! the guarantee with `#[must_use]` since Rust does not natively enforce linear types.

use crate::ffi;
use crate::{int_result_to_result, float_result_to_result, Result};

/// A linear integer value that must be consumed exactly once.
///
/// This type wraps an `i64` with linear semantics:
/// - No implicit `Drop` (not calling `.consume()` or `.into_inner()` leaks the value
///   and produces a `#[must_use]` warning)
/// - No `Clone` or `Copy`
/// - All arithmetic operations consume `self` and return a new `LinearInt`
///
/// ## Example
///
/// ```ignore
/// let a = LinearInt::new(10);
/// let b = LinearInt::new(20);
/// let sum = a.add(b).unwrap();  // a and b are consumed
/// let value = sum.into_inner(); // sum is consumed
/// assert_eq!(value, 30);
/// ```
#[must_use = "linear values must be consumed; call .into_inner() or .consume()"]
pub struct LinearInt {
    value: i64,
}

// No Drop impl: this is intentional for linear semantics.
// No Clone/Copy: each value is unique and must be consumed exactly once.

impl LinearInt {
    /// Create a new linear integer.
    ///
    /// The returned value must be consumed exactly once.
    pub fn new(value: i64) -> Self {
        LinearInt { value }
    }

    /// Extract the inner value, consuming this linear wrapper.
    ///
    /// This is the primary way to "finish" a linear computation chain.
    pub fn into_inner(self) -> i64 {
        // We use ManuallyDrop-like semantics: since there is no Drop impl,
        // moving out of self is the consumption.
        self.value
    }

    /// Explicitly discard this linear value without extracting it.
    ///
    /// Use this when you need to satisfy the linear consumption requirement
    /// but do not need the value.
    pub fn consume(self) {
        // Intentionally empty: moving self into this function consumes it.
        let _ = self.value;
    }

    /// Peek at the inner value without consuming.
    ///
    /// Note: In a true linear type system this would not be allowed. This
    /// is provided as a concession to Rust's type system limitations, since
    /// Rust cannot enforce that a value is used exactly once at the type level.
    /// Use sparingly.
    pub fn peek(&self) -> i64 {
        self.value
    }

    /// Safe addition: consumes both operands, returns a new linear result.
    ///
    /// Delegates to `proven_math_add_checked` in libproven.
    /// Returns `Err(Error::Overflow)` on overflow.
    pub fn add(self, other: LinearInt) -> Result<LinearInt> {
        // SAFETY: proven_math_add_checked is a pure function with no
        // pointer arguments; always safe to call.
        let result = unsafe { ffi::proven_math_add_checked(self.value, other.value) };
        int_result_to_result(result).map(LinearInt::new)
    }

    /// Safe subtraction: consumes both operands, returns a new linear result.
    ///
    /// Delegates to `proven_math_sub_checked` in libproven.
    /// Returns `Err(Error::Underflow)` on underflow.
    pub fn sub(self, other: LinearInt) -> Result<LinearInt> {
        // SAFETY: proven_math_sub_checked is a pure function.
        let result = unsafe { ffi::proven_math_sub_checked(self.value, other.value) };
        int_result_to_result(result).map(LinearInt::new)
    }

    /// Safe multiplication: consumes both operands, returns a new linear result.
    ///
    /// Delegates to `proven_math_mul_checked` in libproven.
    /// Returns `Err(Error::Overflow)` on overflow.
    pub fn mul(self, other: LinearInt) -> Result<LinearInt> {
        // SAFETY: proven_math_mul_checked is a pure function.
        let result = unsafe { ffi::proven_math_mul_checked(self.value, other.value) };
        int_result_to_result(result).map(LinearInt::new)
    }

    /// Safe division: consumes both operands, returns a new linear result.
    ///
    /// Delegates to `proven_math_div` in libproven.
    /// Returns `Err(Error::DivisionByZero)` if denominator is 0.
    pub fn div(self, other: LinearInt) -> Result<LinearInt> {
        // SAFETY: proven_math_div is a pure function.
        let result = unsafe { ffi::proven_math_div(self.value, other.value) };
        int_result_to_result(result).map(LinearInt::new)
    }

    /// Safe modulo: consumes both operands, returns a new linear result.
    ///
    /// Delegates to `proven_math_mod` in libproven.
    /// Returns `Err(Error::DivisionByZero)` if denominator is 0.
    pub fn modulo(self, other: LinearInt) -> Result<LinearInt> {
        // SAFETY: proven_math_mod is a pure function.
        let result = unsafe { ffi::proven_math_mod(self.value, other.value) };
        int_result_to_result(result).map(LinearInt::new)
    }

    /// Safe absolute value: consumes self, returns a new linear result.
    ///
    /// Delegates to `proven_math_abs_safe` in libproven.
    /// Returns `Err(Error::Overflow)` for `i64::MIN`.
    pub fn abs(self) -> Result<LinearInt> {
        // SAFETY: proven_math_abs_safe is a pure function.
        let result = unsafe { ffi::proven_math_abs_safe(self.value) };
        int_result_to_result(result).map(LinearInt::new)
    }

    /// Safe negation: consumes self, returns a new linear result.
    ///
    /// Delegates to `proven_math_negate_safe` in libproven.
    /// Returns `Err(Error::Overflow)` for `i64::MIN`.
    pub fn negate(self) -> Result<LinearInt> {
        // SAFETY: proven_math_negate_safe is a pure function.
        let result = unsafe { ffi::proven_math_negate_safe(self.value) };
        int_result_to_result(result).map(LinearInt::new)
    }

    /// Clamp to range [lo, hi]: consumes self and both bounds.
    ///
    /// Delegates to `proven_math_clamp` in libproven. Always succeeds.
    pub fn clamp(self, lo: LinearInt, hi: LinearInt) -> LinearInt {
        // SAFETY: proven_math_clamp is a pure function.
        let clamped = unsafe { ffi::proven_math_clamp(lo.value, hi.value, self.value) };
        LinearInt::new(clamped)
    }

    /// Safe exponentiation: consumes self, returns a new linear result.
    ///
    /// Delegates to `proven_math_pow_checked` in libproven.
    /// Returns `Err(Error::Overflow)` on overflow.
    pub fn pow(self, exp: u32) -> Result<LinearInt> {
        // SAFETY: proven_math_pow_checked is a pure function.
        let result = unsafe { ffi::proven_math_pow_checked(self.value, exp) };
        int_result_to_result(result).map(LinearInt::new)
    }
}

/// A linear floating-point value that must be consumed exactly once.
///
/// Wraps an `f64` with the same linear semantics as [`LinearInt`].
/// All arithmetic prevents NaN and Infinity via the Idris 2 verified backend.
#[must_use = "linear values must be consumed; call .into_inner() or .consume()"]
pub struct LinearFloat {
    value: f64,
}

// No Drop impl: linear semantics.
// No Clone/Copy: each value is unique.

impl LinearFloat {
    /// Create a new linear float.
    pub fn new(value: f64) -> Self {
        LinearFloat { value }
    }

    /// Extract the inner value, consuming this linear wrapper.
    pub fn into_inner(self) -> f64 {
        self.value
    }

    /// Explicitly discard this linear value.
    pub fn consume(self) {
        let _ = self.value;
    }

    /// Peek at the inner value without consuming.
    ///
    /// See [`LinearInt::peek`] for caveats about linear type approximation.
    pub fn peek(&self) -> f64 {
        self.value
    }

    /// Safe addition: consumes both operands, returns a new linear result.
    ///
    /// Delegates to `proven_float_add` in libproven.
    /// Returns an error if the result would be NaN or Infinity.
    pub fn add(self, other: LinearFloat) -> Result<LinearFloat> {
        // SAFETY: proven_float_add is a pure function.
        let result = unsafe { ffi::proven_float_add(self.value, other.value) };
        float_result_to_result(result).map(LinearFloat::new)
    }

    /// Safe multiplication: consumes both operands, returns a new linear result.
    ///
    /// Delegates to `proven_float_mul` in libproven.
    /// Returns an error if the result would be NaN or Infinity.
    pub fn mul(self, other: LinearFloat) -> Result<LinearFloat> {
        // SAFETY: proven_float_mul is a pure function.
        let result = unsafe { ffi::proven_float_mul(self.value, other.value) };
        float_result_to_result(result).map(LinearFloat::new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linear_int_create_and_consume() {
        let x = LinearInt::new(42);
        assert_eq!(x.into_inner(), 42);
    }

    #[test]
    fn test_linear_int_explicit_consume() {
        let x = LinearInt::new(99);
        x.consume(); // Satisfies linear requirement without extracting
    }

    #[test]
    fn test_linear_int_peek() {
        let x = LinearInt::new(7);
        assert_eq!(x.peek(), 7);
        x.consume();
    }

    #[test]
    fn test_linear_float_create_and_consume() {
        let x = LinearFloat::new(3.14);
        assert!((x.into_inner() - 3.14).abs() < f64::EPSILON);
    }
}
