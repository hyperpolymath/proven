// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMath - Overflow-safe arithmetic operations.
 *
 * Thin wrapper around libproven's SafeMath functions. All arithmetic
 * computation is performed by the formally verified Idris 2 implementation
 * via host-provided WASM imports. This module only handles data marshaling.
 *
 * Corresponds to the SafeMath section in proven.h:
 *   - proven_math_div, proven_math_mod
 *   - proven_math_add_checked, proven_math_sub_checked, proven_math_mul_checked
 *   - proven_math_abs_safe, proven_math_clamp, proven_math_pow_checked
 */

import { Result, RESULT_BUF, readIntResult } from "./common";
import {
  proven_math_div,
  proven_math_mod,
  proven_math_add_checked,
  proven_math_sub_checked,
  proven_math_mul_checked,
  proven_math_abs_safe,
  proven_math_clamp,
  proven_math_pow_checked,
} from "./ffi";

/**
 * SafeMath provides overflow-checked arithmetic operations.
 *
 * Every method delegates to the corresponding libproven host function.
 * No arithmetic logic is implemented in AssemblyScript.
 */
export class SafeMath {
  /**
   * Checked addition with overflow detection.
   * Delegates to proven_math_add_checked (Idris 2 verified).
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result with sum, or ErrOverflow if overflow would occur.
   */
  static checkedAdd(a: i64, b: i64): Result<i64> {
    proven_math_add_checked(a, b, RESULT_BUF);
    return readIntResult(RESULT_BUF);
  }

  /**
   * Checked subtraction with underflow detection.
   * Delegates to proven_math_sub_checked (Idris 2 verified).
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result with difference, or ErrUnderflow if underflow would occur.
   */
  static checkedSub(a: i64, b: i64): Result<i64> {
    proven_math_sub_checked(a, b, RESULT_BUF);
    return readIntResult(RESULT_BUF);
  }

  /**
   * Checked multiplication with overflow detection.
   * Delegates to proven_math_mul_checked (Idris 2 verified).
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result with product, or ErrOverflow if overflow would occur.
   */
  static checkedMul(a: i64, b: i64): Result<i64> {
    proven_math_mul_checked(a, b, RESULT_BUF);
    return readIntResult(RESULT_BUF);
  }

  /**
   * Safe integer division.
   * Delegates to proven_math_div (Idris 2 verified).
   *
   * @param numerator - Dividend.
   * @param denominator - Divisor.
   * @returns Result with quotient, or ErrDivisionByZero/ErrOverflow.
   */
  static checkedDiv(numerator: i64, denominator: i64): Result<i64> {
    proven_math_div(numerator, denominator, RESULT_BUF);
    return readIntResult(RESULT_BUF);
  }

  /**
   * Safe modulo operation.
   * Delegates to proven_math_mod (Idris 2 verified).
   *
   * @param numerator - Dividend.
   * @param denominator - Divisor.
   * @returns Result with remainder, or ErrDivisionByZero.
   */
  static checkedMod(numerator: i64, denominator: i64): Result<i64> {
    proven_math_mod(numerator, denominator, RESULT_BUF);
    return readIntResult(RESULT_BUF);
  }

  /**
   * Safe absolute value.
   * Delegates to proven_math_abs_safe (Idris 2 verified).
   *
   * @param n - Value to take absolute value of.
   * @returns Result with absolute value, or ErrOverflow for i64.MIN_VALUE.
   */
  static checkedAbs(n: i64): Result<i64> {
    proven_math_abs_safe(n, RESULT_BUF);
    return readIntResult(RESULT_BUF);
  }

  /**
   * Clamp value to [lo, hi] range.
   * Delegates to proven_math_clamp (Idris 2 verified).
   *
   * This is infallible -- always returns a valid i64.
   *
   * @param lo - Lower bound (inclusive).
   * @param hi - Upper bound (inclusive).
   * @param value - Value to clamp.
   * @returns Clamped value.
   */
  static clamp(lo: i64, hi: i64, value: i64): i64 {
    return proven_math_clamp(lo, hi, value);
  }

  /**
   * Integer exponentiation with overflow checking.
   * Delegates to proven_math_pow_checked (Idris 2 verified).
   *
   * @param base - Base value.
   * @param exp - Exponent (non-negative).
   * @returns Result with power, or ErrOverflow if overflow would occur.
   */
  static checkedPow(base: i64, exp: u32): Result<i64> {
    proven_math_pow_checked(base, exp, RESULT_BUF);
    return readIntResult(RESULT_BUF);
  }
}
