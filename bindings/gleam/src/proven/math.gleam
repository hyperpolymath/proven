// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeMath - Arithmetic operations that cannot crash.
////
//// Thin FFI wrapper over libproven proven_math_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Safe integer division. Returns Error on division by zero or overflow.
@external(erlang, "proven_nif", "math_div")
pub fn div(numerator: Int, denominator: Int) -> Result(Int, String)

/// Safe modulo operation. Returns Error on division by zero.
@external(erlang, "proven_nif", "math_mod")
pub fn modulo(numerator: Int, denominator: Int) -> Result(Int, String)

/// Checked addition with overflow detection.
@external(erlang, "proven_nif", "math_add_checked")
pub fn add_checked(a: Int, b: Int) -> Result(Int, String)

/// Checked subtraction with underflow detection.
@external(erlang, "proven_nif", "math_sub_checked")
pub fn sub_checked(a: Int, b: Int) -> Result(Int, String)

/// Checked multiplication with overflow detection.
@external(erlang, "proven_nif", "math_mul_checked")
pub fn mul_checked(a: Int, b: Int) -> Result(Int, String)

/// Safe absolute value. Returns Error for MIN_INT.
@external(erlang, "proven_nif", "math_abs_safe")
pub fn abs_safe(n: Int) -> Result(Int, String)

/// Clamp value to range [lo, hi].
@external(erlang, "proven_nif", "math_clamp")
pub fn clamp(lo: Int, hi: Int, value: Int) -> Int

/// Integer exponentiation with overflow checking.
@external(erlang, "proven_nif", "math_pow_checked")
pub fn pow_checked(base: Int, exp: Int) -> Result(Int, String)
