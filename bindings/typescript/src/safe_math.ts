// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeMath - Arithmetic operations that cannot crash.
 *
 * All operations handle edge cases like division by zero, overflow, and underflow
 * without throwing exceptions. Operations return null on failure.
 */

import { getExports } from './wasm.js';
import { statusFromCode } from './error.js';

/**
 * Safe arithmetic operations with proven correctness guarantees.
 */
export class SafeMath {
  /**
   * Safe division that returns null on division by zero.
   *
   * @example
   * SafeMath.div(10, 2)  // 5
   * SafeMath.div(10, 0)  // null
   */
  static div(numerator: number, denominator: number): number | null {
    const exports = getExports();
    const fn = exports['proven_math_div'] as (a: bigint, b: bigint) => bigint;

    // Pack result into i64: status in high 32 bits, value in low 32 bits
    const result = fn(BigInt(numerator), BigInt(denominator));
    const status = Number(result >> 32n);
    const value = Number(result & 0xffffffffn);

    if (statusFromCode(status) !== 'ok') {
      return null;
    }
    return value;
  }

  /**
   * Safe division with a default value for division by zero.
   */
  static divOr(defaultValue: number, numerator: number, denominator: number): number {
    return SafeMath.div(numerator, denominator) ?? defaultValue;
  }

  /**
   * Safe modulo that returns null on division by zero.
   */
  static mod(numerator: number, denominator: number): number | null {
    const exports = getExports();
    const fn = exports['proven_math_mod'] as (a: bigint, b: bigint) => bigint;

    const result = fn(BigInt(numerator), BigInt(denominator));
    const status = Number(result >> 32n);
    const value = Number(result & 0xffffffffn);

    if (statusFromCode(status) !== 'ok') {
      return null;
    }
    return value;
  }

  /**
   * Addition with overflow detection.
   *
   * Returns null if the result would overflow.
   *
   * @example
   * SafeMath.addChecked(5, 3)  // 8
   * SafeMath.addChecked(Number.MAX_SAFE_INTEGER, 1)  // null
   */
  static addChecked(a: number, b: number): number | null {
    const exports = getExports();
    const fn = exports['proven_math_add_checked'] as (a: bigint, b: bigint) => bigint;

    const result = fn(BigInt(a), BigInt(b));
    const status = Number(result >> 32n);
    const value = Number(result & 0xffffffffn);

    if (statusFromCode(status) !== 'ok') {
      return null;
    }
    return value;
  }

  /**
   * Subtraction with underflow detection.
   *
   * Returns null if the result would underflow.
   */
  static subChecked(a: number, b: number): number | null {
    const exports = getExports();
    const fn = exports['proven_math_sub_checked'] as (a: bigint, b: bigint) => bigint;

    const result = fn(BigInt(a), BigInt(b));
    const status = Number(result >> 32n);
    const value = Number(result & 0xffffffffn);

    if (statusFromCode(status) !== 'ok') {
      return null;
    }
    return value;
  }

  /**
   * Multiplication with overflow detection.
   *
   * Returns null if the result would overflow.
   */
  static mulChecked(a: number, b: number): number | null {
    const exports = getExports();
    const fn = exports['proven_math_mul_checked'] as (a: bigint, b: bigint) => bigint;

    const result = fn(BigInt(a), BigInt(b));
    const status = Number(result >> 32n);
    const value = Number(result & 0xffffffffn);

    if (statusFromCode(status) !== 'ok') {
      return null;
    }
    return value;
  }

  /**
   * Safe absolute value that handles MIN_INT correctly.
   *
   * Returns null if n is MIN_INT (cannot be represented).
   */
  static absSafe(n: number): number | null {
    const exports = getExports();
    const fn = exports['proven_math_abs_safe'] as (n: bigint) => bigint;

    const result = fn(BigInt(n));
    const status = Number(result >> 32n);
    const value = Number(result & 0xffffffffn);

    if (statusFromCode(status) !== 'ok') {
      return null;
    }
    return value;
  }

  /**
   * Clamp a value to range [lo, hi].
   *
   * @example
   * SafeMath.clamp(0, 100, 50)   // 50
   * SafeMath.clamp(0, 100, 150)  // 100
   * SafeMath.clamp(0, 100, -10)  // 0
   */
  static clamp(lo: number, hi: number, value: number): number {
    const exports = getExports();
    const fn = exports['proven_math_clamp'] as (lo: bigint, hi: bigint, value: bigint) => bigint;

    return Number(fn(BigInt(lo), BigInt(hi), BigInt(value)));
  }

  /**
   * Integer exponentiation with overflow detection.
   *
   * @param exp - Must be non-negative
   * @throws If exp is negative
   */
  static powChecked(base: number, exp: number): number | null {
    if (exp < 0) {
      throw new Error('Exponent must be non-negative');
    }

    const exports = getExports();
    const fn = exports['proven_math_pow_checked'] as (base: bigint, exp: number) => bigint;

    const result = fn(BigInt(base), exp);
    const status = Number(result >> 32n);
    const value = Number(result & 0xffffffffn);

    if (statusFromCode(status) !== 'ok') {
      return null;
    }
    return value;
  }

  /**
   * Calculate percentage safely.
   *
   * @returns percent% of total, or null on overflow/division-by-zero
   *
   * @example
   * SafeMath.percentOf(50, 200)  // 100
   */
  static percentOf(percent: number, total: number): number | null {
    const product = SafeMath.mulChecked(percent, total);
    if (product === null) return null;
    return SafeMath.div(product, 100);
  }

  /**
   * Calculate what percentage part is of whole.
   *
   * @returns The percentage (0-100+), or null on division by zero
   *
   * @example
   * SafeMath.asPercent(50, 200)  // 25
   */
  static asPercent(part: number, whole: number): number | null {
    const scaled = SafeMath.mulChecked(part, 100);
    if (scaled === null) return null;
    return SafeMath.div(scaled, whole);
  }
}
