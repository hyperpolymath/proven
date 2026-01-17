// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeFloat - Floating point operations without NaN/Infinity surprises.
 *
 * Provides safe float operations with proper handling of edge cases.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Safe floating point operations.
 */
export class SafeFloat {
  /**
   * Safe division with zero check.
   *
   * @param {number} a - Dividend
   * @param {number} b - Divisor
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   *
   * @example
   * SafeFloat.div(10, 3)  // { ok: true, value: 3.333... }
   * SafeFloat.div(1, 0)   // { ok: false, error: "Division by zero" }
   */
  static div(a, b) {
    if (b === 0) {
      return err('Division by zero');
    }
    const result = a / b;
    if (!Number.isFinite(result)) {
      return err('Result is not finite');
    }
    return ok(result);
  }

  /**
   * Safe square root.
   *
   * @param {number} x - Input value
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static sqrt(x) {
    if (x < 0) {
      return err('Cannot take square root of negative number');
    }
    if (!Number.isFinite(x)) {
      return err('Input is not finite');
    }
    return ok(Math.sqrt(x));
  }

  /**
   * Safe logarithm.
   *
   * @param {number} x - Input value
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static log(x) {
    if (x <= 0) {
      return err('Logarithm undefined for non-positive numbers');
    }
    if (!Number.isFinite(x)) {
      return err('Input is not finite');
    }
    return ok(Math.log(x));
  }

  /**
   * Safe power operation.
   *
   * @param {number} base - Base
   * @param {number} exponent - Exponent
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static pow(base, exponent) {
    if (base === 0 && exponent < 0) {
      return err('Cannot raise zero to negative power');
    }
    const result = Math.pow(base, exponent);
    if (!Number.isFinite(result)) {
      return err('Result is not finite');
    }
    return ok(result);
  }

  /**
   * Check if value is finite (not NaN, not Infinity).
   *
   * @param {number} x - Value to check
   * @returns {boolean}
   */
  static isFinite(x) {
    return Number.isFinite(x);
  }

  /**
   * Check if value is NaN.
   *
   * @param {number} x - Value to check
   * @returns {boolean}
   */
  static isNaN(x) {
    return Number.isNaN(x);
  }

  /**
   * Check if value is Infinity (positive or negative).
   *
   * @param {number} x - Value to check
   * @returns {boolean}
   */
  static isInfinity(x) {
    return x === Infinity || x === -Infinity;
  }

  /**
   * Clamp value to range.
   *
   * @param {number} value - Value to clamp
   * @param {number} min - Minimum value
   * @param {number} max - Maximum value
   * @returns {number}
   */
  static clamp(value, min, max) {
    if (Number.isNaN(value)) return min;
    return Math.min(Math.max(value, min), max);
  }

  /**
   * Compare floats with epsilon tolerance.
   *
   * @param {number} a - First value
   * @param {number} b - Second value
   * @param {number} [epsilon=Number.EPSILON] - Tolerance
   * @returns {boolean}
   */
  static approxEqual(a, b, epsilon = Number.EPSILON) {
    if (!Number.isFinite(a) || !Number.isFinite(b)) {
      return false;
    }
    return Math.abs(a - b) <= epsilon;
  }

  /**
   * Round to specified decimal places.
   *
   * @param {number} value - Value to round
   * @param {number} decimals - Number of decimal places
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static round(value, decimals) {
    if (!Number.isFinite(value)) {
      return err('Value is not finite');
    }
    if (decimals < 0 || decimals > 20) {
      return err('Decimals must be 0-20');
    }
    const factor = Math.pow(10, decimals);
    return ok(Math.round(value * factor) / factor);
  }

  /**
   * Truncate to specified decimal places.
   *
   * @param {number} value - Value to truncate
   * @param {number} decimals - Number of decimal places
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static truncate(value, decimals) {
    if (!Number.isFinite(value)) {
      return err('Value is not finite');
    }
    if (decimals < 0 || decimals > 20) {
      return err('Decimals must be 0-20');
    }
    const factor = Math.pow(10, decimals);
    return ok(Math.trunc(value * factor) / factor);
  }

  /**
   * Linear interpolation with bounds checking.
   *
   * @param {number} a - Start value
   * @param {number} b - End value
   * @param {number} t - Interpolation factor
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static lerp(a, b, t) {
    if (!Number.isFinite(a) || !Number.isFinite(b) || !Number.isFinite(t)) {
      return err('All inputs must be finite');
    }
    const result = a + (b - a) * t;
    if (!Number.isFinite(result)) {
      return err('Result is not finite');
    }
    return ok(result);
  }

  /**
   * Inverse linear interpolation.
   *
   * @param {number} a - Start value
   * @param {number} b - End value
   * @param {number} value - Value to find t for
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static inverseLerp(a, b, value) {
    if (!Number.isFinite(a) || !Number.isFinite(b) || !Number.isFinite(value)) {
      return err('All inputs must be finite');
    }
    if (a === b) {
      return err('Cannot compute inverse lerp when a equals b');
    }
    return ok((value - a) / (b - a));
  }

  /**
   * Map value from one range to another.
   *
   * @param {number} value - Input value
   * @param {number} inMin - Input range minimum
   * @param {number} inMax - Input range maximum
   * @param {number} outMin - Output range minimum
   * @param {number} outMax - Output range maximum
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static map(value, inMin, inMax, outMin, outMax) {
    if (inMin === inMax) {
      return err('Input range cannot be zero');
    }
    const t = (value - inMin) / (inMax - inMin);
    const result = outMin + (outMax - outMin) * t;
    if (!Number.isFinite(result)) {
      return err('Result is not finite');
    }
    return ok(result);
  }

  /**
   * Normalize value to 0-1 range.
   *
   * @param {number} value - Value to normalize
   * @param {number} min - Range minimum
   * @param {number} max - Range maximum
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static normalize(value, min, max) {
    return SafeFloat.map(value, min, max, 0, 1);
  }

  /**
   * Get sign of a number (-1, 0, or 1).
   *
   * @param {number} x - Input value
   * @returns {number}
   */
  static sign(x) {
    if (Number.isNaN(x)) return 0;
    return Math.sign(x);
  }

  /**
   * Safe modulo (always returns positive for positive divisor).
   *
   * @param {number} a - Dividend
   * @param {number} b - Divisor
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static mod(a, b) {
    if (b === 0) {
      return err('Division by zero');
    }
    return ok(((a % b) + b) % b);
  }
}
