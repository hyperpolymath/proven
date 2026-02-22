// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeRateLimiter - Typed wrapper for token bucket rate limiting.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import {
  TokenBucket as JsTokenBucket,
  SlidingWindowLimiter as JsSlidingWindowLimiter,
  FixedWindowLimiter as JsFixedWindowLimiter,
  SafeRateLimiter as JsSafeRateLimiter,
} from '../../javascript/src/safe_rate_limiter.js';

/** Result type returned by rate limiter operations. */
export type Result<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Typed wrapper around the FFI-backed TokenBucket.
 *
 * Every method calls through to the JavaScript FFI wrapper, which in turn
 * calls the formally verified Idris 2 code via the Zig FFI bridge.
 */
export class TokenBucket {
  /** The underlying JS FFI token bucket instance. */
  readonly #inner: InstanceType<typeof JsTokenBucket>;

  /** Private constructor -- use TokenBucket.create() instead. */
  private constructor(inner: InstanceType<typeof JsTokenBucket>) {
    this.#inner = inner;
  }

  /**
   * Create a token bucket rate limiter via FFI.
   * Delegates to proven_rate_limiter_create.
   *
   * @param capacity - Maximum number of tokens.
   * @param refillRate - Tokens per second refill rate.
   * @returns Result with the TokenBucket instance, or error.
   */
  static create(capacity: number, refillRate: number): Result<TokenBucket> {
    const result = JsSafeRateLimiter.tokenBucket(
      capacity,
      refillRate,
    ) as Result<InstanceType<typeof JsTokenBucket>>;
    if (!result.ok) return result;
    return { ok: true, value: new TokenBucket(result.value) };
  }

  /**
   * Try to acquire tokens from the bucket.
   * Delegates to proven_rate_limiter_try_acquire via FFI.
   *
   * @param tokens - Number of tokens to consume (default 1.0).
   * @returns True if tokens were available and consumed.
   */
  tryAcquire(tokens: number = 1.0): boolean {
    return this.#inner.tryAcquire(tokens);
  }

  /**
   * Close and free the native rate limiter.
   * Delegates to proven_rate_limiter_free via FFI.
   */
  close(): void {
    this.#inner.close();
  }
}

/**
 * Sliding window rate limiter (placeholder).
 * Will be wired when the Idris implementation is complete.
 */
export class SlidingWindowLimiter extends JsSlidingWindowLimiter {}

/**
 * Fixed window rate limiter (placeholder).
 * Will be wired when the Idris implementation is complete.
 */
export class FixedWindowLimiter extends JsFixedWindowLimiter {}

/**
 * SafeRateLimiter namespace.
 * All operations delegate to the JavaScript FFI binding.
 */
export class SafeRateLimiter {
  /**
   * Create a token bucket rate limiter.
   * Delegates to proven_rate_limiter_create via FFI.
   *
   * @param capacity - Maximum tokens.
   * @param refillRate - Tokens per second.
   * @returns Result with the TokenBucket instance, or error.
   */
  static tokenBucket(capacity: number, refillRate: number): Result<TokenBucket> {
    return TokenBucket.create(capacity, refillRate);
  }
}
