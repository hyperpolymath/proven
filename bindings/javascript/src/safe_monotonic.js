// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMonotonic - Monotonically increasing sequences.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Monotonic counter backed by the libproven FFI.
 */
export class MonotonicCounter {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create a monotonic counter.
   *
   * @param {number|bigint} initial - Initial value.
   * @param {number|bigint} maxValue - Maximum value before wrapping.
   * @returns {{ ok: true, value: MonotonicCounter } | { ok: false, error: string }}
   */
  static create(initial, maxValue) {
    const symbols = getLib();
    const ptr = symbols.proven_monotonic_create(BigInt(initial), BigInt(maxValue));
    if (ptr === null) return err('Failed to create monotonic counter');
    const counter = new MonotonicCounter();
    counter.#ptr = ptr;
    return ok(counter);
  }

  /**
   * Get the next value (atomically increments).
   *
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  next() {
    if (this.#ptr === null) return err('Counter is closed');
    const symbols = getLib();
    const result = symbols.proven_monotonic_next(this.#ptr);
    if (result[0] !== ProvenStatus.OK) return err(statusToError(result[0]));
    return ok(Number(result[1]));
  }

  /**
   * Close and free the native counter.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_monotonic_free(this.#ptr);
      this.#ptr = null;
    }
  }
}

/**
 * Monotonic timestamp (placeholder -- will wrap a counter with clock source).
 */
export class MonotonicTimestamp {}

/**
 * Monotonic ID generator (placeholder).
 */
export class MonotonicId {}

/**
 * Monotonic sequence (placeholder).
 */
export class MonotonicSequence {}

/**
 * SafeMonotonic namespace.
 */
export class SafeMonotonic {
  /**
   * Create a monotonic counter.
   *
   * @param {number|bigint} initial
   * @param {number|bigint} maxValue
   * @returns {{ ok: true, value: MonotonicCounter } | { ok: false, error: string }}
   */
  static counter(initial, maxValue) {
    return MonotonicCounter.create(initial, maxValue);
  }
}
