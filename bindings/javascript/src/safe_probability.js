// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeProbability - Probability values clamped to [0, 1].
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib } from './ffi.js';

/**
 * Probability value (always in [0, 1]).
 */
export class Probability {
  /** @type {number} */
  #value;

  /**
   * @param {number} value - Probability value (will be clamped to [0,1]).
   */
  constructor(value) {
    const symbols = getLib();
    this.#value = symbols.proven_probability_create(value);
  }

  /** @returns {number} The probability value. */
  valueOf() { return this.#value; }

  /** @returns {string} */
  toString() { return this.#value.toString(); }
}

/**
 * Safe probability operations backed by formally verified Idris 2 code.
 */
export class SafeProbability {
  /**
   * Create a probability value (clamped to [0, 1]).
   *
   * @param {number} value - Raw value.
   * @returns {number} Clamped probability.
   */
  static create(value) {
    const symbols = getLib();
    return symbols.proven_probability_create(value);
  }

  /**
   * Multiply probabilities (independent events: P(A and B) = P(A) * P(B)).
   *
   * @param {number} a - First probability.
   * @param {number} b - Second probability.
   * @returns {number}
   */
  static and(a, b) {
    const symbols = getLib();
    return symbols.proven_probability_and(a, b);
  }

  /**
   * Add probabilities (mutually exclusive: P(A or B) = P(A) + P(B)).
   *
   * @param {number} a - First probability.
   * @param {number} b - Second probability.
   * @returns {number}
   */
  static orExclusive(a, b) {
    const symbols = getLib();
    return symbols.proven_probability_or_exclusive(a, b);
  }

  /**
   * Complement (P(not A) = 1 - P(A)).
   *
   * @param {number} p - Probability.
   * @returns {number}
   */
  static not(p) {
    const symbols = getLib();
    return symbols.proven_probability_not(p);
  }
}
