// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeAngle - Angle conversions and normalization.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib } from './ffi.js';

/**
 * Degrees value wrapper.
 */
export class Degrees {
  /** @type {number} */
  value;

  /** @param {number} value */
  constructor(value) { this.value = value; }

  /** @returns {string} */
  toString() { return `${this.value} deg`; }
}

/**
 * Radians value wrapper.
 */
export class Radians {
  /** @type {number} */
  value;

  /** @param {number} value */
  constructor(value) { this.value = value; }

  /** @returns {string} */
  toString() { return `${this.value} rad`; }
}

/**
 * Safe angle operations backed by formally verified Idris 2 code.
 */
export class SafeAngle {
  /**
   * Convert degrees to radians.
   *
   * @param {number} degrees - Angle in degrees.
   * @returns {number} Angle in radians.
   */
  static degToRad(degrees) {
    const symbols = getLib();
    return symbols.proven_angle_deg_to_rad(degrees);
  }

  /**
   * Convert radians to degrees.
   *
   * @param {number} radians - Angle in radians.
   * @returns {number} Angle in degrees.
   */
  static radToDeg(radians) {
    const symbols = getLib();
    return symbols.proven_angle_rad_to_deg(radians);
  }

  /**
   * Normalize angle to 0-360 degrees.
   *
   * @param {number} degrees - Angle in degrees.
   * @returns {number} Normalized angle.
   */
  static normalizeDegrees(degrees) {
    const symbols = getLib();
    return symbols.proven_angle_normalize_degrees(degrees);
  }

  /**
   * Normalize angle to 0-2pi radians.
   *
   * @param {number} radians - Angle in radians.
   * @returns {number} Normalized angle.
   */
  static normalizeRadians(radians) {
    const symbols = getLib();
    return symbols.proven_angle_normalize_radians(radians);
  }
}
