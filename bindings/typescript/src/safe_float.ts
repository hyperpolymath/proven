// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeFloat - Typed wrapper for floating-point operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeFloat as JsSafeFloat } from '../../javascript/src/safe_float.js';

/** Result type for float operations. */
export type FloatResult<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Safe floating-point operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export class SafeFloat {
  /**
   * Safe floating-point division.
   * Delegates to proven_float_div via FFI.
   *
   * @param a - Dividend.
   * @param b - Divisor.
   * @returns Result with quotient, or error on division by zero / NaN.
   */
  static div(a: number, b: number): FloatResult<number> {
    return JsSafeFloat.div(a, b) as FloatResult<number>;
  }

  /**
   * Check if a float is finite (not NaN or Infinity).
   * Delegates to proven_float_is_finite via FFI.
   *
   * @param x - The value to check.
   * @returns true if the value is finite.
   */
  static isFinite(x: number): boolean {
    return JsSafeFloat.isFinite(x);
  }

  /**
   * Check if a float is NaN.
   * Delegates to proven_float_is_nan via FFI.
   *
   * @param x - The value to check.
   * @returns true if the value is NaN.
   */
  static isNaN(x: number): boolean {
    return JsSafeFloat.isNaN(x);
  }

  /**
   * Safe square root (returns error for negative input).
   * Delegates to proven_float_sqrt via FFI.
   *
   * @param x - The value.
   * @returns Result with square root, or error.
   */
  static sqrt(x: number): FloatResult<number> {
    return JsSafeFloat.sqrt(x) as FloatResult<number>;
  }

  /**
   * Safe natural logarithm (returns error for non-positive input).
   * Delegates to proven_float_ln via FFI.
   *
   * @param x - The value.
   * @returns Result with natural log, or error.
   */
  static ln(x: number): FloatResult<number> {
    return JsSafeFloat.ln(x) as FloatResult<number>;
  }
}
