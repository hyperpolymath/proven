// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeColor - Color space conversions.
 *
 * Thin FFI wrapper: proven_color_parse_hex, proven_color_rgb_to_hsl,
 * and proven_color_to_hex use struct-by-value parameters (RGBColor,
 * HSLColor) that require buffer marshaling for Deno FFI.
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * RGB color representation.
 */
export class Rgb {
  /** @type {number} */ r;
  /** @type {number} */ g;
  /** @type {number} */ b;

  /**
   * @param {number} r - Red (0-255).
   * @param {number} g - Green (0-255).
   * @param {number} b - Blue (0-255).
   */
  constructor(r, g, b) {
    this.r = r & 0xff;
    this.g = g & 0xff;
    this.b = b & 0xff;
  }

  /** @returns {string} Hex string (e.g. "#ff0000"). */
  toHex() {
    const hex = (n) => n.toString(16).padStart(2, '0');
    return `#${hex(this.r)}${hex(this.g)}${hex(this.b)}`;
  }

  /** @returns {string} */
  toString() { return this.toHex(); }
}

/**
 * RGBA color representation.
 */
export class Rgba {
  /** @type {number} */ r;
  /** @type {number} */ g;
  /** @type {number} */ b;
  /** @type {number} */ a;

  /**
   * @param {number} r - Red (0-255).
   * @param {number} g - Green (0-255).
   * @param {number} b - Blue (0-255).
   * @param {number} a - Alpha (0.0-1.0).
   */
  constructor(r, g, b, a = 1.0) {
    this.r = r & 0xff;
    this.g = g & 0xff;
    this.b = b & 0xff;
    this.a = Math.max(0, Math.min(1, a));
  }
}

/**
 * Safe color operations.
 * Note: color FFI functions use struct-by-value parameters
 * (RGBColor = 3 bytes, HSLColor = 3 f64s) which require
 * buffer marshaling. Will be fully wired when buffer protocol
 * is complete.
 */
export class SafeColor {}
