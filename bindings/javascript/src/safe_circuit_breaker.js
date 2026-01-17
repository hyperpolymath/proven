// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeCircuitBreaker - Circuit breaker pattern implementation.
 *
 * Provides fault tolerance with automatic recovery.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Circuit breaker states.
 * @readonly
 * @enum {string}
 */
export const CircuitState = {
  CLOSED: 'closed',
  OPEN: 'open',
  HALF_OPEN: 'half_open',
};

/**
 * Circuit breaker for fault tolerance.
 */
export class CircuitBreaker {
  /** @type {number} */
  #failureThreshold;
  /** @type {number} */
  #successThreshold;
  /** @type {number} */
  #timeout;
  /** @type {CircuitState} */
  #state;
  /** @type {number} */
  #failures;
  /** @type {number} */
  #successes;
  /** @type {number} */
  #lastFailureTime;
  /** @type {number} */
  #totalCalls;
  /** @type {number} */
  #totalFailures;
  /** @type {number} */
  #totalSuccesses;

  /**
   * Create a circuit breaker.
   *
   * @param {Object} options - Options
   * @param {number} [options.failureThreshold=5] - Failures before opening
   * @param {number} [options.successThreshold=2] - Successes in half-open before closing
   * @param {number} [options.timeout=30000] - Time in ms before trying again
   */
  constructor(options = {}) {
    const { failureThreshold = 5, successThreshold = 2, timeout = 30000 } = options;

    if (failureThreshold < 1) {
      throw new Error('Failure threshold must be at least 1');
    }
    if (successThreshold < 1) {
      throw new Error('Success threshold must be at least 1');
    }
    if (timeout < 0) {
      throw new Error('Timeout must be non-negative');
    }

    this.#failureThreshold = failureThreshold;
    this.#successThreshold = successThreshold;
    this.#timeout = timeout;
    this.#state = CircuitState.CLOSED;
    this.#failures = 0;
    this.#successes = 0;
    this.#lastFailureTime = 0;
    this.#totalCalls = 0;
    this.#totalFailures = 0;
    this.#totalSuccesses = 0;
  }

  /**
   * Get current state.
   *
   * @returns {CircuitState}
   */
  get state() {
    this.#checkTimeout();
    return this.#state;
  }

  /**
   * Get current failure count.
   *
   * @returns {number}
   */
  get failures() {
    return this.#failures;
  }

  /**
   * Get failure threshold.
   *
   * @returns {number}
   */
  get failureThreshold() {
    return this.#failureThreshold;
  }

  /**
   * Get total calls.
   *
   * @returns {number}
   */
  get totalCalls() {
    return this.#totalCalls;
  }

  /**
   * Get total failures.
   *
   * @returns {number}
   */
  get totalFailures() {
    return this.#totalFailures;
  }

  /**
   * Get total successes.
   *
   * @returns {number}
   */
  get totalSuccesses() {
    return this.#totalSuccesses;
  }

  /**
   * Check if circuit allows calls.
   *
   * @returns {boolean}
   */
  isAllowed() {
    this.#checkTimeout();
    return this.#state !== CircuitState.OPEN;
  }

  /**
   * Check if circuit is open (blocking calls).
   *
   * @returns {boolean}
   */
  isOpen() {
    this.#checkTimeout();
    return this.#state === CircuitState.OPEN;
  }

  /**
   * Check if circuit is closed (allowing calls).
   *
   * @returns {boolean}
   */
  isClosed() {
    this.#checkTimeout();
    return this.#state === CircuitState.CLOSED;
  }

  /**
   * Check if circuit is half-open (testing).
   *
   * @returns {boolean}
   */
  isHalfOpen() {
    this.#checkTimeout();
    return this.#state === CircuitState.HALF_OPEN;
  }

  /**
   * Record a successful call.
   */
  recordSuccess() {
    this.#totalCalls++;
    this.#totalSuccesses++;
    this.#checkTimeout();

    if (this.#state === CircuitState.HALF_OPEN) {
      this.#successes++;
      if (this.#successes >= this.#successThreshold) {
        this.#close();
      }
    } else if (this.#state === CircuitState.CLOSED) {
      // Reset failure count on success
      this.#failures = 0;
    }
  }

  /**
   * Record a failed call.
   */
  recordFailure() {
    this.#totalCalls++;
    this.#totalFailures++;
    this.#checkTimeout();

    this.#failures++;
    this.#lastFailureTime = Date.now();

    if (this.#state === CircuitState.HALF_OPEN) {
      this.#open();
    } else if (this.#state === CircuitState.CLOSED && this.#failures >= this.#failureThreshold) {
      this.#open();
    }
  }

  /**
   * Execute a function with circuit breaker protection.
   *
   * @template T
   * @param {() => T} fn - Function to execute
   * @returns {{ ok: true, value: T } | { ok: false, error: string }}
   */
  call(fn) {
    if (!this.isAllowed()) {
      return err('Circuit is open');
    }

    try {
      const result = fn();
      this.recordSuccess();
      return ok(result);
    } catch (error) {
      this.recordFailure();
      return err(error instanceof Error ? error.message : String(error));
    }
  }

  /**
   * Execute an async function with circuit breaker protection.
   *
   * @template T
   * @param {() => Promise<T>} fn - Async function to execute
   * @returns {Promise<{ ok: true, value: T } | { ok: false, error: string }>}
   */
  async callAsync(fn) {
    if (!this.isAllowed()) {
      return err('Circuit is open');
    }

    try {
      const result = await fn();
      this.recordSuccess();
      return ok(result);
    } catch (error) {
      this.recordFailure();
      return err(error instanceof Error ? error.message : String(error));
    }
  }

  /**
   * Force the circuit open.
   */
  forceOpen() {
    this.#open();
  }

  /**
   * Force the circuit closed.
   */
  forceClose() {
    this.#close();
  }

  /**
   * Reset the circuit to initial state.
   */
  reset() {
    this.#state = CircuitState.CLOSED;
    this.#failures = 0;
    this.#successes = 0;
    this.#lastFailureTime = 0;
  }

  /**
   * Reset all statistics.
   */
  resetStats() {
    this.#totalCalls = 0;
    this.#totalFailures = 0;
    this.#totalSuccesses = 0;
  }

  /**
   * Get time until circuit might close.
   *
   * @returns {number} Milliseconds (0 if not open)
   */
  timeUntilRetry() {
    if (this.#state !== CircuitState.OPEN) {
      return 0;
    }

    const elapsed = Date.now() - this.#lastFailureTime;
    return Math.max(0, this.#timeout - elapsed);
  }

  /**
   * Get statistics.
   *
   * @returns {Object}
   */
  getStats() {
    return {
      state: this.state,
      failures: this.#failures,
      successes: this.#successes,
      totalCalls: this.#totalCalls,
      totalFailures: this.#totalFailures,
      totalSuccesses: this.#totalSuccesses,
      failureRate: this.#totalCalls > 0 ? this.#totalFailures / this.#totalCalls : 0,
    };
  }

  /**
   * Open the circuit.
   */
  #open() {
    this.#state = CircuitState.OPEN;
    this.#successes = 0;
  }

  /**
   * Close the circuit.
   */
  #close() {
    this.#state = CircuitState.CLOSED;
    this.#failures = 0;
    this.#successes = 0;
  }

  /**
   * Check if timeout has elapsed and transition to half-open.
   */
  #checkTimeout() {
    if (this.#state === CircuitState.OPEN) {
      const elapsed = Date.now() - this.#lastFailureTime;
      if (elapsed >= this.#timeout) {
        this.#state = CircuitState.HALF_OPEN;
        this.#successes = 0;
      }
    }
  }
}

/**
 * Safe circuit breaker utilities.
 */
export class SafeCircuitBreaker {
  /**
   * Create a circuit breaker with default settings.
   *
   * @returns {CircuitBreaker}
   */
  static create() {
    return new CircuitBreaker();
  }

  /**
   * Create a circuit breaker with custom settings.
   *
   * @param {number} failureThreshold - Failures before opening
   * @param {number} successThreshold - Successes before closing
   * @param {number} timeout - Recovery timeout in ms
   * @returns {CircuitBreaker}
   */
  static withConfig(failureThreshold, successThreshold, timeout) {
    return new CircuitBreaker({ failureThreshold, successThreshold, timeout });
  }

  /**
   * Create a lenient circuit breaker (high threshold).
   *
   * @returns {CircuitBreaker}
   */
  static lenient() {
    return new CircuitBreaker({
      failureThreshold: 10,
      successThreshold: 1,
      timeout: 60000,
    });
  }

  /**
   * Create a strict circuit breaker (low threshold).
   *
   * @returns {CircuitBreaker}
   */
  static strict() {
    return new CircuitBreaker({
      failureThreshold: 2,
      successThreshold: 3,
      timeout: 15000,
    });
  }
}
