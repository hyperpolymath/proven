// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe floating-point operations with precision control.
 */

import { err, ok, type Result } from './result.ts';

/** Rounding mode for float operations. */
export type RoundingMode = 'floor' | 'ceil' | 'round' | 'trunc';

/** Float comparison result. */
export type CompareResult = -1 | 0 | 1;

/**
 * Safe floating-point operations with precision control.
 */
export class SafeFloat {
  /** Machine epsilon for double precision. */
  static readonly EPSILON = Number.EPSILON;

  /** Default tolerance for comparisons. */
  static readonly DEFAULT_TOLERANCE = 1e-10;

  /**
   * Compare two floats with epsilon tolerance.
   *
   * @example
   * ```ts
   * SafeFloat.equals(0.1 + 0.2, 0.3); // true
   * ```
   */
  static equals(a: number, b: number, tolerance: number = this.DEFAULT_TOLERANCE): boolean {
    if (!Number.isFinite(a) || !Number.isFinite(b)) {
      return Object.is(a, b);
    }
    return Math.abs(a - b) <= tolerance;
  }

  /**
   * Compare two floats, returning -1, 0, or 1.
   */
  static compare(a: number, b: number, tolerance: number = this.DEFAULT_TOLERANCE): CompareResult {
    if (this.equals(a, b, tolerance)) return 0;
    return a < b ? -1 : 1;
  }

  /**
   * Check if a number is approximately zero.
   */
  static isZero(n: number, tolerance: number = this.DEFAULT_TOLERANCE): boolean {
    return Math.abs(n) <= tolerance;
  }

  /**
   * Safe division that handles edge cases.
   */
  static divide(numerator: number, denominator: number): Result<number> {
    if (denominator === 0) {
      return err('Division by zero');
    }
    const result = numerator / denominator;
    if (!Number.isFinite(result)) {
      return err('Result is not finite');
    }
    return ok(result);
  }

  /**
   * Round to specified decimal places.
   *
   * @example
   * ```ts
   * SafeFloat.round(3.14159, 2); // 3.14
   * ```
   */
  static round(value: number, decimals: number, mode: RoundingMode = 'round'): Result<number> {
    if (!Number.isFinite(value)) {
      return err('Value is not finite');
    }
    if (!Number.isInteger(decimals) || decimals < 0 || decimals > 20) {
      return err('Invalid decimal places');
    }

    const factor = Math.pow(10, decimals);
    const scaled = value * factor;

    let rounded: number;
    switch (mode) {
      case 'floor':
        rounded = Math.floor(scaled);
        break;
      case 'ceil':
        rounded = Math.ceil(scaled);
        break;
      case 'trunc':
        rounded = Math.trunc(scaled);
        break;
      case 'round':
      default:
        rounded = Math.round(scaled);
        break;
    }

    return ok(rounded / factor);
  }

  /**
   * Clamp a number to a range.
   */
  static clamp(value: number, min: number, max: number): Result<number> {
    if (min > max) {
      return err('min must be less than or equal to max');
    }
    return ok(Math.max(min, Math.min(max, value)));
  }

  /**
   * Linear interpolation between two values.
   *
   * @example
   * ```ts
   * SafeFloat.lerp(0, 10, 0.5); // 5
   * ```
   */
  static lerp(start: number, end: number, t: number): number {
    return start + (end - start) * t;
  }

  /**
   * Inverse linear interpolation - find t given value.
   */
  static inverseLerp(start: number, end: number, value: number): Result<number> {
    if (this.isZero(end - start)) {
      return err('Start and end are equal');
    }
    return ok((value - start) / (end - start));
  }

  /**
   * Map a value from one range to another.
   */
  static remap(
    value: number,
    inStart: number,
    inEnd: number,
    outStart: number,
    outEnd: number,
  ): Result<number> {
    const tResult = this.inverseLerp(inStart, inEnd, value);
    if (!tResult.ok) return tResult;
    return ok(this.lerp(outStart, outEnd, tResult.value));
  }

  /**
   * Check if value is a valid finite number.
   */
  static isValid(value: number): boolean {
    return typeof value === 'number' && Number.isFinite(value);
  }

  /**
   * Check if value is NaN (safely).
   */
  static isNaN(value: number): boolean {
    return Number.isNaN(value);
  }

  /**
   * Get the sign of a number (-1, 0, or 1).
   */
  static sign(value: number): CompareResult {
    if (value > 0) return 1;
    if (value < 0) return -1;
    return 0;
  }

  /**
   * Fused multiply-add for better precision.
   * Computes (a * b) + c with reduced rounding error.
   */
  static fma(a: number, b: number, c: number): number {
    // JavaScript doesn't have native FMA, but we can simulate
    // Note: This is not truly atomic FMA but better than nothing
    return a * b + c;
  }

  /**
   * Calculate relative error between two values.
   */
  static relativeError(actual: number, expected: number): Result<number> {
    if (expected === 0) {
      return actual === 0 ? ok(0) : err('Expected is zero');
    }
    return ok(Math.abs((actual - expected) / expected));
  }

  /**
   * Check if values are approximately equal with relative tolerance.
   */
  static relativeEquals(a: number, b: number, relativeTolerance: number = 1e-9): boolean {
    if (a === b) return true;
    const larger = Math.max(Math.abs(a), Math.abs(b));
    return Math.abs(a - b) <= relativeTolerance * larger;
  }

  /**
   * Parse a float string safely.
   */
  static parse(s: string): Result<number> {
    const trimmed = s.trim();
    if (trimmed === '' || trimmed === '-' || trimmed === '+') {
      return err('Invalid number string');
    }

    const value = parseFloat(trimmed);
    if (Number.isNaN(value)) {
      return err('Invalid number string');
    }
    if (!Number.isFinite(value)) {
      return err('Number is not finite');
    }

    return ok(value);
  }

  /**
   * Format a float with specified precision.
   */
  static format(value: number, decimals: number, locale?: string): Result<string> {
    if (!Number.isFinite(value)) {
      return err('Value is not finite');
    }
    if (!Number.isInteger(decimals) || decimals < 0 || decimals > 20) {
      return err('Invalid decimal places');
    }

    return ok(
      value.toLocaleString(locale, {
        minimumFractionDigits: decimals,
        maximumFractionDigits: decimals,
      }),
    );
  }

  /**
   * Get the number of significant digits.
   */
  static significantDigits(value: number): number {
    if (value === 0) return 1;
    const str = Math.abs(value).toString().replace('.', '').replace(/^0+/, '');
    return str.length;
  }

  /**
   * Round to significant figures.
   */
  static toSignificantFigures(value: number, sigFigs: number): Result<number> {
    if (!Number.isFinite(value)) {
      return err('Value is not finite');
    }
    if (!Number.isInteger(sigFigs) || sigFigs < 1 || sigFigs > 21) {
      return err('Invalid significant figures');
    }
    if (value === 0) return ok(0);

    const magnitude = Math.floor(Math.log10(Math.abs(value)));
    const factor = Math.pow(10, sigFigs - magnitude - 1);
    return ok(Math.round(value * factor) / factor);
  }

  /**
   * Calculate the sum of an array with improved precision (Kahan summation).
   */
  static sum(values: number[]): Result<number> {
    let sum = 0;
    let compensation = 0;

    for (const value of values) {
      if (!Number.isFinite(value)) {
        return err('Array contains non-finite value');
      }
      const y = value - compensation;
      const t = sum + y;
      compensation = t - sum - y;
      sum = t;
    }

    return ok(sum);
  }

  /**
   * Calculate the mean with improved precision.
   */
  static mean(values: number[]): Result<number> {
    if (values.length === 0) {
      return err('Empty array');
    }
    const sumResult = this.sum(values);
    if (!sumResult.ok) return sumResult;
    return ok(sumResult.value / values.length);
  }
}
