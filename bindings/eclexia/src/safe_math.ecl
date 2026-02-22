// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// SafeMath -- Resource-typed wrappers around libproven arithmetic FFI.
///
/// All computation is delegated to the formally verified Idris 2
/// implementation via the Zig FFI layer. No arithmetic logic is
/// reimplemented here. Resource budgets apply to computational cost.

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Convert an FFI IntResult into a Result[Int, ProvenError].
fn int_result_to_result(result: IntResult) -> Result[Int, ProvenError] {
    if result.status == STATUS_OK {
        Ok(result.value)
    } else {
        Err(status_to_error(result.status))
    }
}

/// Convert an FFI FloatResult into a Result[Float, ProvenError].
fn float_result_to_result(result: FloatResult) -> Result[Float, ProvenError] {
    if result.status == STATUS_OK {
        Ok(result.value)
    } else {
        Err(status_to_error(result.status))
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Safe addition with overflow detection.
/// Delegates to proven_math_add_checked via FFI.
fn safe_add(a: Int, b: Int) -> Result[Int, ProvenError] {
    int_result_to_result(proven_math_add_checked(a, b))
}

/// Safe subtraction with underflow detection.
/// Delegates to proven_math_sub_checked via FFI.
fn safe_sub(a: Int, b: Int) -> Result[Int, ProvenError] {
    int_result_to_result(proven_math_sub_checked(a, b))
}

/// Safe multiplication with overflow detection.
/// Delegates to proven_math_mul_checked via FFI.
fn safe_mul(a: Int, b: Int) -> Result[Int, ProvenError] {
    int_result_to_result(proven_math_mul_checked(a, b))
}

/// Safe division with zero-check.
/// Delegates to proven_math_div via FFI.
fn safe_div(numerator: Int, denominator: Int) -> Result[Int, ProvenError] {
    int_result_to_result(proven_math_div(numerator, denominator))
}

/// Safe modulo with zero-check.
/// Delegates to proven_math_mod via FFI.
fn safe_mod(numerator: Int, denominator: Int) -> Result[Int, ProvenError] {
    int_result_to_result(proven_math_mod(numerator, denominator))
}

/// Safe absolute value (handles MIN_INT correctly).
/// Delegates to proven_math_abs_safe via FFI.
fn safe_abs(n: Int) -> Result[Int, ProvenError] {
    int_result_to_result(proven_math_abs_safe(n))
}

/// Clamp value to range [lo, hi]. Always succeeds.
/// Delegates to proven_math_clamp via FFI.
fn safe_clamp(value: Int, lo: Int, hi: Int) -> Int {
    proven_math_clamp(lo, hi, value)
}

/// Safe integer power with overflow checking.
/// Delegates to proven_math_pow_checked via FFI.
fn safe_pow(base: Int, exponent: Int) -> Result[Int, ProvenError] {
    int_result_to_result(proven_math_pow_checked(base, exponent))
}

/// Calculate percentage safely: (percent * total) / 100.
fn percent_of(percent: Int, total: Int) -> Result[Int, ProvenError] {
    match safe_mul(percent, total) {
        Err(e) => Err(e),
        Ok(product) => safe_div(product, 100)
    }
}

/// Calculate what percentage part is of whole: (part * 100) / whole.
fn as_percent(part: Int, whole: Int) -> Result[Int, ProvenError] {
    match safe_mul(part, 100) {
        Err(e) => Err(e),
        Ok(scaled) => safe_div(scaled, whole)
    }
}

/// Safe floating-point division.
/// Delegates to proven_float_div via FFI.
fn safe_float_div(a: Float, b: Float) -> Result[Float, ProvenError] {
    float_result_to_result(proven_float_div(a, b))
}

/// Safe square root.
/// Delegates to proven_float_sqrt via FFI.
fn safe_sqrt(x: Float) -> Result[Float, ProvenError] {
    float_result_to_result(proven_float_sqrt(x))
}

/// Safe natural logarithm.
/// Delegates to proven_float_ln via FFI.
fn safe_ln(x: Float) -> Result[Float, ProvenError] {
    float_result_to_result(proven_float_ln(x))
}

/// Check if a float is finite (not NaN, not infinity).
/// Delegates to proven_float_is_finite via FFI.
fn is_finite(x: Float) -> Bool {
    proven_float_is_finite(x)
}

/// Check if a float is NaN.
/// Delegates to proven_float_is_nan via FFI.
fn is_nan(x: Float) -> Bool {
    proven_float_is_nan(x)
}
