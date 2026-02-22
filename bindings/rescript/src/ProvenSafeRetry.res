// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeRetry - Typed wrapper for retry with exponential backoff.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides retry, retryAsync, RetryBuilder, and
 * RetryPresets backed by native retry implementations.
 */

/** Retry strategy enumeration. */
type retryStrategy =
  | Fixed
  | Linear
  | Exponential

/** Retry configuration. */
type retryConfig = {
  maxAttempts: int,
  strategy: string,
  baseDelay: int,
  maxDelay: option<int>,
  jitterFactor: option<float>,
  multiplier: option<float>,
}

/** Retry result. */
type retryResult<'a> = {
  ok: bool,
  value: option<'a>,
  error: option<string>,
  attempts: int,
  totalDelay: float,
}

/** Convert a retryStrategy variant to its JS string representation. */
let strategyToString = (strategy: retryStrategy): string => {
  switch strategy {
  | Fixed => "fixed"
  | Linear => "linear"
  | Exponential => "exponential"
  }
}

/** JavaScript bindings to the SafeRetry FFI wrapper. */
module SafeRetryJs = {
  @module("../../javascript/src/safe_retry.js")
  external calculateDelay: (retryConfig, int) => float = "calculateDelay"

  @module("../../javascript/src/safe_retry.js")
  external retry: (unit => 'a, retryConfig) => retryResult<'a> = "retry"
}

/**
 * Calculate delay for a given retry attempt.
 *
 * @param config Retry configuration.
 * @param attempt The attempt number (0-based).
 * @returns The delay in milliseconds.
 */
let calculateDelay = SafeRetryJs.calculateDelay

/**
 * Retry a synchronous operation with backoff.
 *
 * @param fn The function to retry.
 * @param config Retry configuration.
 * @returns The retry result.
 */
let retry = SafeRetryJs.retry

/** Quick retries for local operations. */
let presetQuick = (): retryConfig => {
  maxAttempts: 3,
  strategy: strategyToString(Fixed),
  baseDelay: 100,
  maxDelay: None,
  jitterFactor: Some(0.0),
  multiplier: None,
}

/** Standard exponential backoff for network operations. */
let presetStandard = (): retryConfig => {
  maxAttempts: 5,
  strategy: strategyToString(Exponential),
  baseDelay: 1000,
  maxDelay: Some(30000),
  jitterFactor: Some(0.1),
  multiplier: Some(2.0),
}

/** Aggressive retries for critical operations. */
let presetAggressive = (): retryConfig => {
  maxAttempts: 10,
  strategy: strategyToString(Exponential),
  baseDelay: 500,
  maxDelay: Some(60000),
  jitterFactor: Some(0.2),
  multiplier: Some(2.0),
}

/** Gentle retries for rate-limited APIs. */
let presetGentle = (): retryConfig => {
  maxAttempts: 5,
  strategy: strategyToString(Linear),
  baseDelay: 5000,
  maxDelay: Some(60000),
  jitterFactor: Some(0.1),
  multiplier: Some(1.0),
}
