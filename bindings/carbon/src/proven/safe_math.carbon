// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_math.carbon - Safe arithmetic operations via libproven FFI.
//
// All operations delegate to the Idris 2 formally verified implementation
// through the Zig FFI layer. No arithmetic logic is implemented in Carbon.

package Proven api;

import Proven library "lib_proven";

// Safe math operations that cannot crash.
//
// All methods call the formally verified Idris 2 implementation via FFI.
// Returns Optional(i64) where None indicates an error (overflow, underflow,
// division by zero, etc.).
class SafeMath {

  // Safe addition with overflow detection.
  //
  // Returns None if the result would exceed i64 range.
  fn Add(a: i64, b: i64) -> Optional(i64) {
    var result: IntResult = proven_math_add_checked(a, b);
    if (Succeeded(result.status)) {
      return Optional(i64).Create(result.value);
    }
    return Optional(i64).None();
  }

  // Safe subtraction with underflow detection.
  //
  // Returns None if the result would go below i64 minimum.
  fn Sub(a: i64, b: i64) -> Optional(i64) {
    var result: IntResult = proven_math_sub_checked(a, b);
    if (Succeeded(result.status)) {
      return Optional(i64).Create(result.value);
    }
    return Optional(i64).None();
  }

  // Safe multiplication with overflow detection.
  //
  // Returns None if the result would overflow i64.
  fn Mul(a: i64, b: i64) -> Optional(i64) {
    var result: IntResult = proven_math_mul_checked(a, b);
    if (Succeeded(result.status)) {
      return Optional(i64).Create(result.value);
    }
    return Optional(i64).None();
  }

  // Safe division with zero-check.
  //
  // Returns None if the denominator is 0, or if INT64_MIN / -1 would overflow.
  fn Div(numerator: i64, denominator: i64) -> Optional(i64) {
    var result: IntResult = proven_math_div(numerator, denominator);
    if (Succeeded(result.status)) {
      return Optional(i64).Create(result.value);
    }
    return Optional(i64).None();
  }

  // Safe modulo with zero-check.
  //
  // Returns None if the denominator is 0.
  fn Mod(numerator: i64, denominator: i64) -> Optional(i64) {
    var result: IntResult = proven_math_mod(numerator, denominator);
    if (Succeeded(result.status)) {
      return Optional(i64).Create(result.value);
    }
    return Optional(i64).None();
  }

  // Safe absolute value.
  //
  // Returns None for i64 minimum (its absolute value cannot be represented).
  fn Abs(n: i64) -> Optional(i64) {
    var result: IntResult = proven_math_abs_safe(n);
    if (Succeeded(result.status)) {
      return Optional(i64).Create(result.value);
    }
    return Optional(i64).None();
  }

  // Clamp value to range [lo, hi].
  //
  // Always succeeds. Returns lo if value < lo, hi if value > hi.
  fn Clamp(lo: i64, hi: i64, value: i64) -> i64 {
    return proven_math_clamp(lo, hi, value);
  }

  // Integer power with overflow checking.
  //
  // Returns None if the result would overflow i64.
  fn Pow(base: i64, exp: u32) -> Optional(i64) {
    var result: IntResult = proven_math_pow_checked(base, exp);
    if (Succeeded(result.status)) {
      return Optional(i64).Create(result.value);
    }
    return Optional(i64).None();
  }
}
