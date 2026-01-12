// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * Safe mathematical operations with overflow detection.
 */

import { err, ok, type Result } from './result.ts';

export type MathResult = Result<number>;

const MAX_SAFE = Number.MAX_SAFE_INTEGER;
const MIN_SAFE = Number.MIN_SAFE_INTEGER;

/**
 * Safe mathematical operations that never throw.
 */
export class SafeMath {
  /**
   * Safe addition with overflow detection.
   *
   * @example
   * ```ts
   * const result = SafeMath.add(1, 2);
   * if (result.ok) console.log(result.value); // 3
   *
   * const overflow = SafeMath.add(Number.MAX_SAFE_INTEGER, 1);
   * if (!overflow.ok) console.log(overflow.error); // "Overflow"
   * ```
   */
  static add(a: number, b: number): MathResult {
    const result = a + b;
    if (result > MAX_SAFE || result < MIN_SAFE) {
      return err(`Overflow: ${a} + ${b}`);
    }
    return ok(result);
  }

  /** Safe subtraction with underflow detection. */
  static sub(a: number, b: number): MathResult {
    const result = a - b;
    if (result > MAX_SAFE || result < MIN_SAFE) {
      return err(`Underflow: ${a} - ${b}`);
    }
    return ok(result);
  }

  /** Safe multiplication with overflow detection. */
  static mul(a: number, b: number): MathResult {
    const result = a * b;
    if (result > MAX_SAFE || result < MIN_SAFE) {
      return err(`Overflow: ${a} * ${b}`);
    }
    return ok(result);
  }

  /** Safe division with zero check. */
  static div(a: number, b: number): MathResult {
    if (b === 0) {
      return err('Division by zero');
    }
    return ok(a / b);
  }

  /** Safe integer division with zero check. */
  static divInt(a: number, b: number): MathResult {
    if (b === 0) {
      return err('Division by zero');
    }
    return ok(Math.trunc(a / b));
  }

  /** Safe modulo with zero check. */
  static mod(a: number, b: number): MathResult {
    if (b === 0) {
      return err('Division by zero');
    }
    return ok(a % b);
  }

  /** Safe absolute value. */
  static abs(a: number): MathResult {
    return ok(Math.abs(a));
  }

  /** Safe power with overflow detection. */
  static pow(base: number, exp: number): MathResult {
    const result = Math.pow(base, exp);
    if (!Number.isFinite(result)) {
      return err(`Overflow: ${base}^${exp}`);
    }
    return ok(result);
  }

  /** Check if a value is within safe integer range. */
  static isSafeInteger(n: number): boolean {
    return Number.isSafeInteger(n);
  }

  /** Clamp a value to a range. */
  static clamp(value: number, min: number, max: number): number {
    return Math.max(min, Math.min(max, value));
  }
}
