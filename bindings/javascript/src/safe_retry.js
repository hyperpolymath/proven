// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeRetry - Exponential backoff retry strategies.
 *
 * Thin FFI wrapper: proven_retry_delay and proven_retry_should_retry
 * take a RetryConfig struct by value which requires buffer marshaling.
 * @module
 */

import { getLib, ProvenStatus, statusToError } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Retry strategy enumeration.
 * @readonly
 * @enum {string}
 */
export const RetryStrategy = Object.freeze({
  EXPONENTIAL: 'exponential',
  LINEAR: 'linear',
  CONSTANT: 'constant',
});

/**
 * Retry configuration.
 */
export class RetryConfig {
  /** @type {number} */ maxAttempts;
  /** @type {number} */ baseDelayMs;
  /** @type {number} */ maxDelayMs;
  /** @type {number} */ multiplier;

  /**
   * @param {object} opts
   * @param {number} opts.maxAttempts
   * @param {number} [opts.baseDelayMs=100]
   * @param {number} [opts.maxDelayMs=30000]
   * @param {number} [opts.multiplier=2.0]
   */
  constructor({ maxAttempts, baseDelayMs = 100, maxDelayMs = 30000, multiplier = 2.0 }) {
    this.maxAttempts = maxAttempts;
    this.baseDelayMs = baseDelayMs;
    this.maxDelayMs = maxDelayMs;
    this.multiplier = multiplier;
  }
}

/**
 * Retry result type.
 */
export class RetryResult {
  /** @type {boolean} */
  shouldRetry;
  /** @type {number} */
  delayMs;

  /**
   * @param {boolean} shouldRetry
   * @param {number} delayMs
   */
  constructor(shouldRetry, delayMs) {
    this.shouldRetry = shouldRetry;
    this.delayMs = delayMs;
  }
}

/**
 * Retry helper.
 * Note: The FFI functions proven_retry_delay and proven_retry_should_retry
 * take a RetryConfig struct by value. This requires buffer-based marshaling
 * which will be wired when the buffer protocol is complete.
 */
export class Retry {}

/**
 * SafeRetry namespace.
 */
export class SafeRetry {}
