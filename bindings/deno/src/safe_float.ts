// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeFloat - Floating-point operations that prevent NaN/Infinity.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.ts';
import { err, ok, type Result } from './result.ts';

/**
 * Safe floating-point operations backed by formally verified Idris 2 code.
 */
export class SafeFloat {
  /**
   * Safe floating-point division.
   *
   * @param a - Dividend.
   * @param b - Divisor.
   * @returns Result containing the quotient or an error string.
   */
  static div(a: number, b: number): Result<number> {
    const symbols = getLib();
    const result = symbols.proven_float_div(a, b);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a float is finite (not NaN or Infinity).
   *
   * @param x - The value to check.
   * @returns True if finite.
   */
  static isFinite(x: number): boolean {
    const symbols = getLib();
    return symbols.proven_float_is_finite(x);
  }

  /**
   * Check if a float is NaN.
   *
   * @param x - The value to check.
   * @returns True if NaN.
   */
  static isNaN(x: number): boolean {
    const symbols = getLib();
    return symbols.proven_float_is_nan(x);
  }

  /**
   * Safe square root (returns error for negative input).
   *
   * @param x - The value.
   * @returns Result containing the square root or an error string.
   */
  static sqrt(x: number): Result<number> {
    const symbols = getLib();
    const result = symbols.proven_float_sqrt(x);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Safe natural logarithm (returns error for non-positive input).
   *
   * @param x - The value.
   * @returns Result containing the natural log or an error string.
   */
  static ln(x: number): Result<number> {
    const symbols = getLib();
    const result = symbols.proven_float_ln(x);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}
