// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeRateLimiter - Token bucket rate limiting.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib } from './ffi.js';
import { ok, err } from './result.js';

/**
 * Token bucket rate limiter backed by the libproven FFI.
 */
export class TokenBucket {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create a token bucket rate limiter.
   *
   * @param {number} capacity - Maximum number of tokens.
   * @param {number} refillRate - Tokens per second refill rate.
   * @returns {{ ok: true, value: TokenBucket } | { ok: false, error: string }}
   */
  static create(capacity, refillRate) {
    const symbols = getLib();
    const ptr = symbols.proven_rate_limiter_create(capacity, refillRate);
    if (ptr === null) return err('Failed to create rate limiter');
    const bucket = new TokenBucket();
    bucket.#ptr = ptr;
    return ok(bucket);
  }

  /**
   * Try to acquire tokens from the bucket.
   *
   * @param {number} [tokens=1.0] - Number of tokens to consume.
   * @returns {boolean} True if tokens were available and consumed.
   */
  tryAcquire(tokens = 1.0) {
    if (this.#ptr === null) return false;
    const symbols = getLib();
    return symbols.proven_rate_limiter_try_acquire(this.#ptr, tokens);
  }

  /**
   * Close and free the native rate limiter.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_rate_limiter_free(this.#ptr);
      this.#ptr = null;
    }
  }
}

/**
 * Sliding window rate limiter (placeholder -- will be wired when
 * the Idris implementation is complete).
 */
export class SlidingWindowLimiter {}

/**
 * Fixed window rate limiter (placeholder).
 */
export class FixedWindowLimiter {}

/**
 * SafeRateLimiter namespace.
 */
export class SafeRateLimiter {
  /**
   * Create a token bucket rate limiter.
   *
   * @param {number} capacity - Maximum tokens.
   * @param {number} refillRate - Tokens per second.
   * @returns {{ ok: true, value: TokenBucket } | { ok: false, error: string }}
   */
  static tokenBucket(capacity, refillRate) {
    return TokenBucket.create(capacity, refillRate);
  }
}
