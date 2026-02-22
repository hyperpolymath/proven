// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMath - Arithmetic operations that cannot overflow or crash.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Safe math operations with overflow detection.
 * Every method calls through to the proven FFI.
 */
export class SafeMath {
  /**
   * Safe addition with overflow check.
   *
   * @param {number|bigint} a - First operand.
   * @param {number|bigint} b - Second operand.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static add(a, b) {
    const symbols = getLib();
    const result = symbols.proven_math_add_checked(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe subtraction with underflow check.
   *
   * @param {number|bigint} a - First operand.
   * @param {number|bigint} b - Second operand.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static sub(a, b) {
    const symbols = getLib();
    const result = symbols.proven_math_sub_checked(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe multiplication with overflow check.
   *
   * @param {number|bigint} a - First operand.
   * @param {number|bigint} b - Second operand.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static mul(a, b) {
    const symbols = getLib();
    const result = symbols.proven_math_mul_checked(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe integer division with division-by-zero check.
   *
   * @param {number|bigint} a - Dividend.
   * @param {number|bigint} b - Divisor.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static div(a, b) {
    const symbols = getLib();
    const result = symbols.proven_math_div(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe modulo operation.
   *
   * @param {number|bigint} a - Dividend.
   * @param {number|bigint} b - Divisor.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static mod(a, b) {
    const symbols = getLib();
    const result = symbols.proven_math_mod(BigInt(a), BigInt(b));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Safe absolute value (handles MIN_INT correctly).
   *
   * @param {number|bigint} n - Input value.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static abs(n) {
    const symbols = getLib();
    const result = symbols.proven_math_abs_safe(BigInt(n));
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Clamp value to range [lo, hi].
   *
   * @param {number|bigint} value - Value to clamp.
   * @param {number|bigint} lo - Minimum value.
   * @param {number|bigint} hi - Maximum value.
   * @returns {number}
   */
  static clamp(value, lo, hi) {
    const symbols = getLib();
    return Number(symbols.proven_math_clamp(BigInt(lo), BigInt(hi), BigInt(value)));
  }

  /**
   * Safe integer power with overflow checking.
   *
   * @param {number|bigint} base - Base value.
   * @param {number} exponent - Exponent (non-negative integer).
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static pow(base, exponent) {
    const symbols = getLib();
    const result = symbols.proven_math_pow_checked(BigInt(base), exponent >>> 0);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }
}
