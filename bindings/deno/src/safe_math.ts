// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMath - Arithmetic operations that cannot overflow or crash.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/** Result type alias for math operations. */
export type MathResult = Result<number>;

/**
 * Safe math operations with overflow detection.
 * Every method calls through to the proven FFI.
 */
export class SafeMath {
  /**
   * Safe addition with overflow check.
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result containing the sum or an error string.
   */
  static add(a: number | bigint, b: number | bigint): MathResult {
    const symbols = getLib();
    const result = symbols.proven_math_add_checked(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe subtraction with underflow check.
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result containing the difference or an error string.
   */
  static sub(a: number | bigint, b: number | bigint): MathResult {
    const symbols = getLib();
    const result = symbols.proven_math_sub_checked(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe multiplication with overflow check.
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result containing the product or an error string.
   */
  static mul(a: number | bigint, b: number | bigint): MathResult {
    const symbols = getLib();
    const result = symbols.proven_math_mul_checked(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe integer division with division-by-zero check.
   *
   * @param a - Dividend.
   * @param b - Divisor.
   * @returns Result containing the quotient or an error string.
   */
  static div(a: number | bigint, b: number | bigint): MathResult {
    const symbols = getLib();
    const result = symbols.proven_math_div(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe modulo operation.
   *
   * @param a - Dividend.
   * @param b - Divisor.
   * @returns Result containing the remainder or an error string.
   */
  static mod(a: number | bigint, b: number | bigint): MathResult {
    const symbols = getLib();
    const result = symbols.proven_math_mod(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe absolute value (handles MIN_INT correctly).
   *
   * @param n - Input value.
   * @returns Result containing the absolute value or an error string.
   */
  static abs(n: number | bigint): MathResult {
    const symbols = getLib();
    const result = symbols.proven_math_abs_safe(BigInt(n));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Clamp value to range [lo, hi].
   *
   * @param value - Value to clamp.
   * @param lo - Minimum value.
   * @param hi - Maximum value.
   * @returns The clamped value.
   */
  static clamp(value: number | bigint, lo: number | bigint, hi: number | bigint): number {
    const symbols = getLib();
    return Number(symbols.proven_math_clamp(BigInt(lo), BigInt(hi), BigInt(value)));
  }

  /**
   * Safe integer power with overflow checking.
   *
   * @param base - Base value.
   * @param exponent - Exponent (non-negative integer).
   * @returns Result containing the power or an error string.
   */
  static pow(base: number | bigint, exponent: number): MathResult {
    const symbols = getLib();
    const result = symbols.proven_math_pow_checked(BigInt(base), exponent >>> 0);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }
}
