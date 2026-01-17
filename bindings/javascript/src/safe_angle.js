// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeAngle - Degree/radian conversions that cannot crash.
 *
 * Provides safe angle operations with normalization.
 * @module
 */

import { ok, err } from './result.js';

/** @type {number} PI constant */
const PI = Math.PI;

/** @type {number} TAU (2 * PI) */
const TAU = 2 * Math.PI;

/**
 * Degrees wrapper with validation.
 */
export class Degrees {
  /** @type {number} */
  #value;

  /**
   * Create a Degrees value.
   *
   * @param {number} value - Angle in degrees
   */
  constructor(value) {
    if (!Number.isFinite(value)) {
      throw new Error('Degrees must be a finite number');
    }
    this.#value = value;
  }

  /**
   * Get the raw value.
   *
   * @returns {number}
   */
  get value() {
    return this.#value;
  }

  /**
   * Convert to radians.
   *
   * @returns {Radians}
   */
  toRadians() {
    return new Radians((this.#value * PI) / 180);
  }

  /**
   * Normalize to 0-360 range.
   *
   * @returns {Degrees}
   */
  normalize() {
    return new Degrees(((this.#value % 360) + 360) % 360);
  }

  /**
   * Normalize to -180 to 180 range.
   *
   * @returns {Degrees}
   */
  normalizeSigned() {
    let normalized = ((this.#value % 360) + 360) % 360;
    if (normalized > 180) {
      normalized -= 360;
    }
    return new Degrees(normalized);
  }

  /**
   * Add degrees.
   *
   * @param {Degrees | number} other - Degrees to add
   * @returns {Degrees}
   */
  add(other) {
    const otherValue = other instanceof Degrees ? other.value : other;
    return new Degrees(this.#value + otherValue);
  }

  /**
   * Subtract degrees.
   *
   * @param {Degrees | number} other - Degrees to subtract
   * @returns {Degrees}
   */
  sub(other) {
    const otherValue = other instanceof Degrees ? other.value : other;
    return new Degrees(this.#value - otherValue);
  }

  /**
   * Multiply by scalar.
   *
   * @param {number} scalar - Scalar value
   * @returns {Degrees}
   */
  mul(scalar) {
    return new Degrees(this.#value * scalar);
  }

  /**
   * Calculate sine.
   *
   * @returns {number}
   */
  sin() {
    return Math.sin((this.#value * PI) / 180);
  }

  /**
   * Calculate cosine.
   *
   * @returns {number}
   */
  cos() {
    return Math.cos((this.#value * PI) / 180);
  }

  /**
   * Calculate tangent.
   *
   * @returns {number}
   */
  tan() {
    return Math.tan((this.#value * PI) / 180);
  }

  /**
   * String representation.
   *
   * @returns {string}
   */
  toString() {
    return `${this.#value}°`;
  }
}

/**
 * Radians wrapper with validation.
 */
export class Radians {
  /** @type {number} */
  #value;

  /**
   * Create a Radians value.
   *
   * @param {number} value - Angle in radians
   */
  constructor(value) {
    if (!Number.isFinite(value)) {
      throw new Error('Radians must be a finite number');
    }
    this.#value = value;
  }

  /**
   * Get the raw value.
   *
   * @returns {number}
   */
  get value() {
    return this.#value;
  }

  /**
   * Convert to degrees.
   *
   * @returns {Degrees}
   */
  toDegrees() {
    return new Degrees((this.#value * 180) / PI);
  }

  /**
   * Normalize to 0-2π range.
   *
   * @returns {Radians}
   */
  normalize() {
    return new Radians(((this.#value % TAU) + TAU) % TAU);
  }

  /**
   * Normalize to -π to π range.
   *
   * @returns {Radians}
   */
  normalizeSigned() {
    let normalized = ((this.#value % TAU) + TAU) % TAU;
    if (normalized > PI) {
      normalized -= TAU;
    }
    return new Radians(normalized);
  }

  /**
   * Add radians.
   *
   * @param {Radians | number} other - Radians to add
   * @returns {Radians}
   */
  add(other) {
    const otherValue = other instanceof Radians ? other.value : other;
    return new Radians(this.#value + otherValue);
  }

  /**
   * Subtract radians.
   *
   * @param {Radians | number} other - Radians to subtract
   * @returns {Radians}
   */
  sub(other) {
    const otherValue = other instanceof Radians ? other.value : other;
    return new Radians(this.#value - otherValue);
  }

  /**
   * Multiply by scalar.
   *
   * @param {number} scalar - Scalar value
   * @returns {Radians}
   */
  mul(scalar) {
    return new Radians(this.#value * scalar);
  }

  /**
   * Calculate sine.
   *
   * @returns {number}
   */
  sin() {
    return Math.sin(this.#value);
  }

  /**
   * Calculate cosine.
   *
   * @returns {number}
   */
  cos() {
    return Math.cos(this.#value);
  }

  /**
   * Calculate tangent.
   *
   * @returns {number}
   */
  tan() {
    return Math.tan(this.#value);
  }

  /**
   * String representation.
   *
   * @returns {string}
   */
  toString() {
    return `${this.#value} rad`;
  }
}

/**
 * Safe angle operations.
 */
export class SafeAngle {
  /**
   * Convert degrees to radians.
   *
   * @param {number} degrees - Angle in degrees
   * @returns {number}
   */
  static degToRad(degrees) {
    return (degrees * PI) / 180;
  }

  /**
   * Convert radians to degrees.
   *
   * @param {number} radians - Angle in radians
   * @returns {number}
   */
  static radToDeg(radians) {
    return (radians * 180) / PI;
  }

  /**
   * Normalize degrees to 0-360 range.
   *
   * @param {number} degrees - Angle in degrees
   * @returns {number}
   */
  static normalizeDegrees(degrees) {
    return ((degrees % 360) + 360) % 360;
  }

  /**
   * Normalize radians to 0-2π range.
   *
   * @param {number} radians - Angle in radians
   * @returns {number}
   */
  static normalizeRadians(radians) {
    return ((radians % TAU) + TAU) % TAU;
  }

  /**
   * Calculate shortest angular distance between two angles (in degrees).
   *
   * @param {number} from - Starting angle in degrees
   * @param {number} to - Ending angle in degrees
   * @returns {number} Signed distance (-180 to 180)
   */
  static shortestDistance(from, to) {
    const diff = SafeAngle.normalizeDegrees(to - from);
    return diff > 180 ? diff - 360 : diff;
  }

  /**
   * Linear interpolation between angles (in degrees).
   *
   * @param {number} from - Starting angle in degrees
   * @param {number} to - Ending angle in degrees
   * @param {number} t - Interpolation factor (0-1)
   * @returns {number}
   */
  static lerp(from, to, t) {
    const distance = SafeAngle.shortestDistance(from, to);
    return SafeAngle.normalizeDegrees(from + distance * t);
  }

  /**
   * Check if angle is within a range (in degrees).
   *
   * @param {number} angle - Angle to check
   * @param {number} start - Range start
   * @param {number} end - Range end
   * @returns {boolean}
   */
  static isInRange(angle, start, end) {
    const normalizedAngle = SafeAngle.normalizeDegrees(angle);
    const normalizedStart = SafeAngle.normalizeDegrees(start);
    const normalizedEnd = SafeAngle.normalizeDegrees(end);

    if (normalizedStart <= normalizedEnd) {
      return normalizedAngle >= normalizedStart && normalizedAngle <= normalizedEnd;
    } else {
      // Range crosses 0/360
      return normalizedAngle >= normalizedStart || normalizedAngle <= normalizedEnd;
    }
  }

  /**
   * Clamp angle to a range (in degrees).
   *
   * @param {number} angle - Angle to clamp
   * @param {number} min - Minimum angle
   * @param {number} max - Maximum angle
   * @returns {number}
   */
  static clamp(angle, min, max) {
    const normalized = SafeAngle.normalizeDegrees(angle);
    if (normalized < min) return min;
    if (normalized > max) return max;
    return normalized;
  }
}

// Export convenience functions
export const degToRad = SafeAngle.degToRad;
export const radToDeg = SafeAngle.radToDeg;
