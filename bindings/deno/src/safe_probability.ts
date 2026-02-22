// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeProbability - Probability values clamped to [0, 1].
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 *
 * @module
 */

import { getLib } from './ffi.ts';

/**
 * Safe probability operations backed by formally verified Idris 2 code.
 */
export class SafeProbability {
  /**
   * Create a probability value (clamped to [0, 1]).
   *
   * @param value - Raw value.
   * @returns Clamped probability.
   */
  static create(value: number): number {
    const symbols = getLib();
    return symbols.proven_probability_create(value);
  }

  /**
   * Multiply probabilities (independent events: P(A and B) = P(A) * P(B)).
   *
   * @param a - First probability.
   * @param b - Second probability.
   * @returns Combined probability.
   */
  static and(a: number, b: number): number {
    const symbols = getLib();
    return symbols.proven_probability_and(a, b);
  }

  /**
   * Add probabilities (mutually exclusive: P(A or B) = P(A) + P(B)).
   *
   * @param a - First probability.
   * @param b - Second probability.
   * @returns Combined probability.
   */
  static orExclusive(a: number, b: number): number {
    const symbols = getLib();
    return symbols.proven_probability_or_exclusive(a, b);
  }

  /**
   * Complement (P(not A) = 1 - P(A)).
   *
   * @param p - Probability.
   * @returns Complement probability.
   */
  static not(p: number): number {
    const symbols = getLib();
    return symbols.proven_probability_not(p);
  }
}
