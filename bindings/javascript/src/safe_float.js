// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeFloat - Floating-point operations that prevent NaN/Infinity.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Safe floating-point operations backed by formally verified Idris 2 code.
 */
export class SafeFloat {
  /**
   * Safe floating-point division.
   *
   * @param {number} a - Dividend.
   * @param {number} b - Divisor.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static div(a, b) {
    const symbols = getLib();
    const result = symbols.proven_float_div(a, b);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Check if a float is finite (not NaN or Infinity).
   *
   * @param {number} x - The value to check.
   * @returns {boolean}
   */
  static isFinite(x) {
    const symbols = getLib();
    return symbols.proven_float_is_finite(x);
  }

  /**
   * Check if a float is NaN.
   *
   * @param {number} x - The value to check.
   * @returns {boolean}
   */
  static isNaN(x) {
    const symbols = getLib();
    return symbols.proven_float_is_nan(x);
  }

  /**
   * Safe square root (returns error for negative input).
   *
   * @param {number} x - The value.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static sqrt(x) {
    const symbols = getLib();
    const result = symbols.proven_float_sqrt(x);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }

  /**
   * Safe natural logarithm (returns error for non-positive input).
   *
   * @param {number} x - The value.
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static ln(x) {
    const symbols = getLib();
    const result = symbols.proven_float_ln(x);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(result[1]);
  }
}
