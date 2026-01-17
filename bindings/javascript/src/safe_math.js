// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeMath - Arithmetic operations that cannot overflow or crash.
 *
 * Provides checked arithmetic with overflow/underflow detection.
 * @module
 */

import { ok, err } from './result.js';

/** @type {number} Maximum safe integer in JavaScript */
const MAX_SAFE = Number.MAX_SAFE_INTEGER;

/** @type {number} Minimum safe integer in JavaScript */
const MIN_SAFE = Number.MIN_SAFE_INTEGER;

/**
 * Safe math operations with overflow detection.
 */
export class SafeMath {
  /**
   * Safe addition with overflow check.
   *
   * @param {number} a - First operand
   * @param {number} b - Second operand
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   *
   * @example
   * SafeMath.add(1, 2)  // { ok: true, value: 3 }
   */
  static add(a, b) {
    const result = a + b;
    if (result > MAX_SAFE || result < MIN_SAFE) {
      return err('Integer overflow');
    }
    return ok(result);
  }

  /**
   * Safe subtraction with underflow check.
   *
   * @param {number} a - First operand
   * @param {number} b - Second operand
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static sub(a, b) {
    const result = a - b;
    if (result > MAX_SAFE || result < MIN_SAFE) {
      return err('Integer underflow');
    }
    return ok(result);
  }

  /**
   * Safe multiplication with overflow check.
   *
   * @param {number} a - First operand
   * @param {number} b - Second operand
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static mul(a, b) {
    const result = a * b;
    if (result > MAX_SAFE || result < MIN_SAFE) {
      return err('Integer overflow');
    }
    return ok(result);
  }

  /**
   * Safe division with division-by-zero check.
   *
   * @param {number} a - Dividend
   * @param {number} b - Divisor
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static div(a, b) {
    if (b === 0) {
      return err('Division by zero');
    }
    return ok(a / b);
  }

  /**
   * Safe integer division.
   *
   * @param {number} a - Dividend
   * @param {number} b - Divisor
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static divInt(a, b) {
    if (b === 0) {
      return err('Division by zero');
    }
    return ok(Math.trunc(a / b));
  }

  /**
   * Safe modulo operation.
   *
   * @param {number} a - Dividend
   * @param {number} b - Divisor
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static mod(a, b) {
    if (b === 0) {
      return err('Division by zero');
    }
    return ok(a % b);
  }

  /**
   * Safe power operation.
   *
   * @param {number} base - Base
   * @param {number} exponent - Exponent
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static pow(base, exponent) {
    if (exponent < 0 && base === 0) {
      return err('Cannot raise zero to negative power');
    }
    const result = Math.pow(base, exponent);
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
    return ok(Math.sqrt(x));
  }

  /**
   * Absolute value (always safe).
   *
   * @param {number} x - Input value
   * @returns {number}
   */
  static abs(x) {
    return Math.abs(x);
  }

  /**
   * Safe natural logarithm.
   *
   * @param {number} x - Input value
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static ln(x) {
    if (x <= 0) {
      return err('Logarithm undefined for non-positive numbers');
    }
    return ok(Math.log(x));
  }

  /**
   * Safe log base 10.
   *
   * @param {number} x - Input value
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static log10(x) {
    if (x <= 0) {
      return err('Logarithm undefined for non-positive numbers');
    }
    return ok(Math.log10(x));
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
    return Math.min(Math.max(value, min), max);
  }

  /**
   * Linear interpolation.
   *
   * @param {number} a - Start value
   * @param {number} b - End value
   * @param {number} t - Interpolation factor (0-1)
   * @returns {number}
   */
  static lerp(a, b, t) {
    return a + (b - a) * t;
  }

  /**
   * Greatest common divisor.
   *
   * @param {number} a - First number
   * @param {number} b - Second number
   * @returns {number}
   */
  static gcd(a, b) {
    a = Math.abs(Math.floor(a));
    b = Math.abs(Math.floor(b));
    while (b !== 0) {
      const temp = b;
      b = a % b;
      a = temp;
    }
    return a;
  }

  /**
   * Least common multiple.
   *
   * @param {number} a - First number
   * @param {number} b - Second number
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static lcm(a, b) {
    if (a === 0 || b === 0) {
      return ok(0);
    }
    const result = Math.abs(a * b) / SafeMath.gcd(a, b);
    if (result > MAX_SAFE) {
      return err('Integer overflow');
    }
    return ok(result);
  }
}
