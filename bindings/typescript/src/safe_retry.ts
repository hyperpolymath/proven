// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeRetry - Typed wrapper for retry strategies with exponential backoff.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * Note: The FFI functions proven_retry_delay and proven_retry_should_retry
 * take a RetryConfig struct by value which requires buffer marshaling.
 * The JS FFI binding will be fully wired when the buffer protocol is complete.
 *
 * @module
 */

import {
  RetryStrategy as JsRetryStrategy,
  RetryConfig as JsRetryConfig,
  RetryResult as JsRetryResult,
  Retry as JsRetry,
  SafeRetry as JsSafeRetry,
} from '../../javascript/src/safe_retry.js';

/** Result type returned by retry operations. */
export type Result<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Retry strategy enumeration (matches the FFI RetryStrategy).
 * @readonly
 */
export const RetryStrategy = JsRetryStrategy as {
  readonly EXPONENTIAL: 'exponential';
  readonly LINEAR: 'linear';
  readonly CONSTANT: 'constant';
};

/** Retry strategy type derived from the FFI enum values. */
export type RetryStrategyValue = 'exponential' | 'linear' | 'constant';

/** Options for creating a RetryConfig. */
export interface RetryConfigOptions {
  readonly maxAttempts: number;
  readonly baseDelayMs?: number;
  readonly maxDelayMs?: number;
  readonly multiplier?: number;
}

/**
 * Typed wrapper around the FFI RetryConfig.
 * Delegates to the JavaScript FFI RetryConfig class.
 */
export class RetryConfig {
  /** The underlying JS FFI retry config instance. */
  readonly #inner: InstanceType<typeof JsRetryConfig>;

  /** Maximum number of retry attempts. */
  get maxAttempts(): number {
    return this.#inner.maxAttempts;
  }

  /** Base delay in milliseconds. */
  get baseDelayMs(): number {
    return this.#inner.baseDelayMs;
  }

  /** Maximum delay in milliseconds. */
  get maxDelayMs(): number {
    return this.#inner.maxDelayMs;
  }

  /** Backoff multiplier. */
  get multiplier(): number {
    return this.#inner.multiplier;
  }

  /**
   * Create a retry configuration.
   * Delegates to the JS FFI RetryConfig.
   *
   * @param opts - Configuration options.
   */
  constructor(opts: RetryConfigOptions) {
    this.#inner = new JsRetryConfig(opts);
  }
}

/**
 * Typed wrapper around the FFI RetryResult.
 * Delegates to the JavaScript FFI RetryResult class.
 */
export class RetryResult {
  /** Whether the operation should be retried. */
  readonly shouldRetry: boolean;

  /** Delay in milliseconds before the next retry. */
  readonly delayMs: number;

  /**
   * Create a retry result.
   * Delegates to the JS FFI RetryResult.
   *
   * @param shouldRetry - Whether to retry.
   * @param delayMs - Delay before next retry in milliseconds.
   */
  constructor(shouldRetry: boolean, delayMs: number) {
    const inner = new JsRetryResult(shouldRetry, delayMs);
    this.shouldRetry = inner.shouldRetry;
    this.delayMs = inner.delayMs;
  }
}

/**
 * Retry helper (placeholder).
 * Will be fully wired when the FFI buffer protocol for struct-by-value
 * marshaling is complete.
 */
export class Retry extends JsRetry {}

/**
 * SafeRetry namespace (placeholder).
 * Will be fully wired when the FFI buffer protocol is complete.
 */
export class SafeRetry extends JsSafeRetry {}
