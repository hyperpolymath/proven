// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeMonotonic - Typed wrapper for monotonically increasing sequences.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import {
  MonotonicCounter as JsMonotonicCounter,
  MonotonicTimestamp as JsMonotonicTimestamp,
  MonotonicId as JsMonotonicId,
  MonotonicSequence as JsMonotonicSequence,
  SafeMonotonic as JsSafeMonotonic,
} from '../../javascript/src/safe_monotonic.js';

/** Result type returned by monotonic operations. */
export type Result<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Typed wrapper around the FFI-backed MonotonicCounter.
 *
 * Every method calls through to the JavaScript FFI wrapper, which in turn
 * calls the formally verified Idris 2 code via the Zig FFI bridge.
 */
export class MonotonicCounter {
  /** The underlying JS FFI monotonic counter instance. */
  readonly #inner: InstanceType<typeof JsMonotonicCounter>;

  /** Private constructor -- use MonotonicCounter.create() instead. */
  private constructor(inner: InstanceType<typeof JsMonotonicCounter>) {
    this.#inner = inner;
  }

  /**
   * Create a monotonic counter via FFI.
   * Delegates to proven_monotonic_create.
   *
   * @param initial - Initial value.
   * @param maxValue - Maximum value before wrapping.
   * @returns Result with the MonotonicCounter instance, or error.
   */
  static create(initial: number | bigint, maxValue: number | bigint): Result<MonotonicCounter> {
    const result = JsSafeMonotonic.counter(
      initial,
      maxValue,
    ) as Result<InstanceType<typeof JsMonotonicCounter>>;
    if (!result.ok) return result;
    return { ok: true, value: new MonotonicCounter(result.value) };
  }

  /**
   * Get the next value (atomically increments).
   * Delegates to proven_monotonic_next via FFI.
   *
   * @returns Result with the next counter value, or error.
   */
  next(): Result<number> {
    return this.#inner.next() as Result<number>;
  }

  /**
   * Close and free the native counter.
   * Delegates to proven_monotonic_free via FFI.
   */
  close(): void {
    this.#inner.close();
  }
}

/**
 * Monotonic timestamp (placeholder).
 * Will wrap a counter with clock source when the Idris implementation
 * is complete.
 */
export class MonotonicTimestamp extends JsMonotonicTimestamp {}

/**
 * Monotonic ID generator (placeholder).
 * Will be wired when the Idris implementation is complete.
 */
export class MonotonicId extends JsMonotonicId {}

/**
 * Monotonic sequence (placeholder).
 * Will be wired when the Idris implementation is complete.
 */
export class MonotonicSequence extends JsMonotonicSequence {}

/**
 * SafeMonotonic namespace.
 * All operations delegate to the JavaScript FFI binding.
 */
export class SafeMonotonic {
  /**
   * Create a monotonic counter.
   * Delegates to proven_monotonic_create via FFI.
   *
   * @param initial - Initial value.
   * @param maxValue - Maximum value.
   * @returns Result with the MonotonicCounter instance, or error.
   */
  static counter(initial: number | bigint, maxValue: number | bigint): Result<MonotonicCounter> {
    return MonotonicCounter.create(initial, maxValue);
  }
}
