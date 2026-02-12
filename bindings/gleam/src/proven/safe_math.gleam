// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeMath - Arithmetic operations that cannot crash.
////
//// All operations handle edge cases like division by zero, overflow, and underflow
//// without panicking. Operations return Result or Option types.

import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result

/// Safe division that returns Error on division by zero.
pub fn div(numerator: Int, denominator: Int) -> Result(Int, Nil) {
  case denominator {
    0 -> Error(Nil)
    _ -> Ok(numerator / denominator)
  }
}

/// Safe division with default value for division by zero.
pub fn div_or(default: Int, numerator: Int, denominator: Int) -> Int {
  case div(numerator, denominator) {
    Ok(v) -> v
    Error(_) -> default
  }
}

/// Safe modulo that returns Error on division by zero.
pub fn modulo(numerator: Int, denominator: Int) -> Result(Int, Nil) {
  case denominator {
    0 -> Error(Nil)
    _ -> Ok(numerator % denominator)
  }
}

/// Addition - Gleam has arbitrary precision integers, so no overflow.
pub fn add(a: Int, b: Int) -> Int {
  a + b
}

/// Addition returning Result for API consistency.
pub fn add_checked(a: Int, b: Int) -> Result(Int, Nil) {
  Ok(a + b)
}

/// Subtraction - Gleam has arbitrary precision integers, so no underflow.
pub fn sub(a: Int, b: Int) -> Int {
  a - b
}

/// Subtraction returning Result for API consistency.
pub fn sub_checked(a: Int, b: Int) -> Result(Int, Nil) {
  Ok(a - b)
}

/// Multiplication - Gleam has arbitrary precision integers, so no overflow.
pub fn mul(a: Int, b: Int) -> Int {
  a * b
}

/// Multiplication returning Result for API consistency.
pub fn mul_checked(a: Int, b: Int) -> Result(Int, Nil) {
  Ok(a * b)
}

/// Safe absolute value - Gleam has arbitrary precision, so always safe.
pub fn abs_safe(n: Int) -> Int {
  int.absolute_value(n)
}

/// Clamp a value to range [lo, hi].
pub fn clamp(lo: Int, hi: Int, value: Int) -> Int {
  int.clamp(value, lo, hi)
}

/// Integer exponentiation.
pub fn pow(base: Int, exp: Int) -> Result(Int, Nil) {
  case exp < 0 {
    True -> Error(Nil)
    False -> Ok(do_pow(base, exp, 1))
  }
}

fn do_pow(base: Int, exp: Int, acc: Int) -> Int {
  case exp {
    0 -> acc
    _ -> do_pow(base, exp - 1, acc * base)
  }
}

/// Calculate percentage safely.
pub fn percent_of(percent: Int, total: Int) -> Result(Int, Nil) {
  div(percent * total, 100)
}

/// Calculate what percentage part is of whole.
pub fn as_percent(part: Int, whole: Int) -> Result(Int, Nil) {
  div(part * 100, whole)
}
