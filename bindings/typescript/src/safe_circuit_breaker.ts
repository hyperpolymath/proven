// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCircuitBreaker - Typed wrapper for the circuit breaker pattern.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import {
  CircuitBreaker as JsCircuitBreaker,
  CircuitState as JsCircuitState,
  SafeCircuitBreaker as JsSafeCircuitBreaker,
} from '../../javascript/src/safe_circuit_breaker.js';

/** Result type returned by circuit breaker operations. */
export type Result<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Circuit breaker state enumeration (matches the FFI CircuitState).
 * @readonly
 */
export const CircuitState = JsCircuitState as {
  readonly CLOSED: 0;
  readonly OPEN: 1;
  readonly HALF_OPEN: 2;
};

/** Circuit state type derived from the FFI enum values. */
export type CircuitStateValue = 0 | 1 | 2;

/**
 * Typed wrapper around the FFI-backed CircuitBreaker.
 *
 * Every method calls through to the JavaScript FFI wrapper, which in turn
 * calls the formally verified Idris 2 code via the Zig FFI bridge.
 */
export class CircuitBreaker {
  /** The underlying JS FFI circuit breaker instance. */
  readonly #inner: InstanceType<typeof JsCircuitBreaker>;

  /** Private constructor -- use CircuitBreaker.create() instead. */
  private constructor(inner: InstanceType<typeof JsCircuitBreaker>) {
    this.#inner = inner;
  }

  /**
   * Create a circuit breaker via FFI.
   * Delegates to proven_circuit_breaker_create.
   *
   * @param failureThreshold - Number of failures before opening.
   * @param successThreshold - Number of successes to close from half-open.
   * @param timeoutMs - Timeout in milliseconds before entering half-open.
   * @returns Result with the CircuitBreaker instance, or error.
   */
  static create(
    failureThreshold: number,
    successThreshold: number,
    timeoutMs: number,
  ): Result<CircuitBreaker> {
    const result = JsSafeCircuitBreaker.create(
      failureThreshold,
      successThreshold,
      timeoutMs,
    ) as Result<InstanceType<typeof JsCircuitBreaker>>;
    if (!result.ok) return result;
    return { ok: true, value: new CircuitBreaker(result.value) };
  }

  /**
   * Check if a request should be allowed through.
   * Delegates to proven_circuit_breaker_allow via FFI.
   *
   * @returns True if the circuit allows calls.
   */
  allow(): boolean {
    return this.#inner.allow();
  }

  /**
   * Record a successful operation.
   * Delegates to proven_circuit_breaker_success via FFI.
   */
  success(): void {
    this.#inner.success();
  }

  /**
   * Record a failed operation.
   * Delegates to proven_circuit_breaker_failure via FFI.
   */
  failure(): void {
    this.#inner.failure();
  }

  /**
   * Get the current circuit state.
   * Delegates to proven_circuit_breaker_state via FFI.
   *
   * @returns A CircuitState enum value.
   */
  getState(): CircuitStateValue {
    return this.#inner.getState() as CircuitStateValue;
  }

  /**
   * Close and free the native circuit breaker.
   * Delegates to proven_circuit_breaker_free via FFI.
   */
  close(): void {
    this.#inner.close();
  }
}

/**
 * SafeCircuitBreaker namespace.
 * All operations delegate to the JavaScript FFI binding.
 */
export class SafeCircuitBreaker {
  /**
   * Create a circuit breaker.
   * Delegates to proven_circuit_breaker_create via FFI.
   *
   * @param failureThreshold - Number of failures before opening.
   * @param successThreshold - Number of successes to close from half-open.
   * @param timeoutMs - Timeout in milliseconds before entering half-open.
   * @returns Result with the CircuitBreaker instance, or error.
   */
  static create(
    failureThreshold: number,
    successThreshold: number,
    timeoutMs: number,
  ): Result<CircuitBreaker> {
    return CircuitBreaker.create(failureThreshold, successThreshold, timeoutMs);
  }
}
