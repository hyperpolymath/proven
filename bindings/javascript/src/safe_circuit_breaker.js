// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCircuitBreaker - Fault tolerance circuit breaker pattern.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Circuit breaker state enumeration (matches Zig CircuitState).
 * @readonly
 * @enum {number}
 */
export const CircuitState = Object.freeze({
  CLOSED: 0,
  OPEN: 1,
  HALF_OPEN: 2,
});

/**
 * Circuit breaker backed by the libproven FFI.
 */
export class CircuitBreaker {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create a circuit breaker.
   *
   * @param {number} failureThreshold - Number of failures before opening.
   * @param {number} successThreshold - Number of successes to close from half-open.
   * @param {number} timeoutMs - Timeout in milliseconds before entering half-open.
   * @returns {{ ok: true, value: CircuitBreaker } | { ok: false, error: string }}
   */
  static create(failureThreshold, successThreshold, timeoutMs) {
    const symbols = getLib();
    const ptr = symbols.proven_circuit_breaker_create(
      failureThreshold >>> 0,
      successThreshold >>> 0,
      BigInt(timeoutMs),
    );
    if (ptr === null) return err('Failed to create circuit breaker');
    const cb = new CircuitBreaker();
    cb.#ptr = ptr;
    return ok(cb);
  }

  /**
   * Check if a request should be allowed through.
   *
   * @returns {boolean}
   */
  allow() {
    if (this.#ptr === null) return false;
    const symbols = getLib();
    return symbols.proven_circuit_breaker_allow(this.#ptr);
  }

  /**
   * Record a successful operation.
   */
  success() {
    if (this.#ptr === null) return;
    const symbols = getLib();
    symbols.proven_circuit_breaker_success(this.#ptr);
  }

  /**
   * Record a failed operation.
   */
  failure() {
    if (this.#ptr === null) return;
    const symbols = getLib();
    symbols.proven_circuit_breaker_failure(this.#ptr);
  }

  /**
   * Get the current circuit state.
   *
   * @returns {number} A CircuitState enum value.
   */
  getState() {
    if (this.#ptr === null) return CircuitState.OPEN;
    const symbols = getLib();
    return symbols.proven_circuit_breaker_state(this.#ptr);
  }

  /**
   * Close and free the native circuit breaker.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_circuit_breaker_free(this.#ptr);
      this.#ptr = null;
    }
  }
}

/**
 * SafeCircuitBreaker namespace.
 */
export class SafeCircuitBreaker {
  /**
   * Create a circuit breaker.
   *
   * @param {number} failureThreshold
   * @param {number} successThreshold
   * @param {number} timeoutMs
   * @returns {{ ok: true, value: CircuitBreaker } | { ok: false, error: string }}
   */
  static create(failureThreshold, successThreshold, timeoutMs) {
    return CircuitBreaker.create(failureThreshold, successThreshold, timeoutMs);
  }
}
