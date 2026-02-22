// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMath - Typed wrapper for arithmetic operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeMath as JsSafeMath } from '../../javascript/src/safe_math.js';

/** Result type returned by SafeMath operations. */
export type MathResult =
  | { readonly ok: true; readonly value: number }
  | { readonly ok: false; readonly error: string };

/**
 * Safe arithmetic operations with overflow detection.
 * Every method calls through to the JavaScript FFI wrapper, which in turn
 * calls the formally verified Idris 2 code via the Zig FFI bridge.
 */
export class SafeMath {
  /**
   * Safe addition with overflow check.
   * Delegates to proven_math_add_checked via FFI.
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result with the sum, or error on overflow.
   */
  static add(a: number, b: number): MathResult {
    return JsSafeMath.add(a, b) as MathResult;
  }

  /**
   * Safe subtraction with underflow check.
   * Delegates to proven_math_sub_checked via FFI.
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result with the difference, or error on underflow.
   */
  static sub(a: number, b: number): MathResult {
    return JsSafeMath.sub(a, b) as MathResult;
  }

  /**
   * Safe multiplication with overflow check.
   * Delegates to proven_math_mul_checked via FFI.
   *
   * @param a - First operand.
   * @param b - Second operand.
   * @returns Result with the product, or error on overflow.
   */
  static mul(a: number, b: number): MathResult {
    return JsSafeMath.mul(a, b) as MathResult;
  }

  /**
   * Safe integer division with division-by-zero check.
   * Delegates to proven_math_div via FFI.
   *
   * @param a - Dividend.
   * @param b - Divisor.
   * @returns Result with the quotient, or error on division by zero.
   */
  static div(a: number, b: number): MathResult {
    return JsSafeMath.div(a, b) as MathResult;
  }

  /**
   * Safe modulo operation.
   * Delegates to proven_math_mod via FFI.
   *
   * @param a - Dividend.
   * @param b - Divisor.
   * @returns Result with the remainder, or error on division by zero.
   */
  static mod(a: number, b: number): MathResult {
    return JsSafeMath.mod(a, b) as MathResult;
  }

  /**
   * Safe absolute value (handles MIN_INT correctly).
   * Delegates to proven_math_abs_safe via FFI.
   *
   * @param n - Input value.
   * @returns Result with absolute value, or error if n is MIN_INT.
   */
  static abs(n: number): MathResult {
    return JsSafeMath.abs(n) as MathResult;
  }

  /**
   * Clamp value to range [lo, hi].
   * Delegates to proven_math_clamp via FFI.
   *
   * @param value - Value to clamp.
   * @param lo - Minimum value.
   * @param hi - Maximum value.
   * @returns The clamped value.
   */
  static clamp(value: number, lo: number, hi: number): number {
    return JsSafeMath.clamp(value, lo, hi);
  }

  /**
   * Safe integer power with overflow checking.
   * Delegates to proven_math_pow_checked via FFI.
   *
   * @param base - Base value.
   * @param exponent - Exponent (non-negative integer).
   * @returns Result with the power, or error on overflow.
   */
  static pow(base: number, exponent: number): MathResult {
    return JsSafeMath.pow(base, exponent) as MathResult;
  }

  /**
   * Calculate percentage safely using FFI-backed multiply and divide.
   *
   * @param percent - The percentage (e.g. 50 for 50%).
   * @param total - The total value.
   * @returns percent% of total, or null on overflow/division-by-zero.
   */
  static percentOf(percent: number, total: number): number | null {
    const product = SafeMath.mul(percent, total);
    if (!product.ok) return null;
    const result = SafeMath.div(product.value, 100);
    if (!result.ok) return null;
    return result.value;
  }

  /**
   * Calculate what percentage part is of whole using FFI-backed operations.
   *
   * @param part - The part value.
   * @param whole - The whole value.
   * @returns The percentage (0-100+), or null on division by zero / overflow.
   */
  static asPercent(part: number, whole: number): number | null {
    const scaled = SafeMath.mul(part, 100);
    if (!scaled.ok) return null;
    const result = SafeMath.div(scaled.value, whole);
    if (!result.ok) return null;
    return result.value;
  }
}
