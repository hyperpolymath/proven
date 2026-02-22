// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeRateLimiter - Typed wrapper for rate limiting algorithms.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides TokenBucket, SlidingWindowLimiter,
 * FixedWindowLimiter, LeakyBucket, and ConcurrencyLimiter classes
 * backed by native rate limiter implementations.
 */

/** Rate limit result returned by all limiter types. */
type rateLimitResult = {
  allowed: bool,
  remaining: int,
  resetAt: float,
  retryAfter: option<float>,
}

/** Opaque handle to a JavaScript TokenBucket instance. */
type tokenBucket

/** Opaque handle to a JavaScript SlidingWindowLimiter instance. */
type slidingWindowLimiter

/** Opaque handle to a JavaScript FixedWindowLimiter instance. */
type fixedWindowLimiter

/** Opaque handle to a JavaScript LeakyBucket instance. */
type leakyBucket

/** Opaque handle to a JavaScript ConcurrencyLimiter instance. */
type concurrencyLimiter

/** JavaScript bindings to the SafeRateLimiter FFI wrapper. */
module SafeRateLimiterJs = {
  @module("../../javascript/src/safe_rate_limiter.js")
  @new
  external createTokenBucket: (int, float) => tokenBucket = "TokenBucket"

  @module("../../javascript/src/safe_rate_limiter.js")
  @new
  external createSlidingWindow: (int, int) => slidingWindowLimiter = "SlidingWindowLimiter"

  @module("../../javascript/src/safe_rate_limiter.js")
  @new
  external createFixedWindow: (int, int) => fixedWindowLimiter = "FixedWindowLimiter"

  @module("../../javascript/src/safe_rate_limiter.js")
  @new
  external createLeakyBucket: (int, float) => leakyBucket = "LeakyBucket"

  @module("../../javascript/src/safe_rate_limiter.js")
  @new
  external createConcurrencyLimiter: int => concurrencyLimiter = "ConcurrencyLimiter"
}

/**
 * Create a new token bucket rate limiter.
 *
 * @param capacity Maximum tokens.
 * @param refillRate Tokens added per second.
 * @returns A new TokenBucket handle.
 */
let createTokenBucket = SafeRateLimiterJs.createTokenBucket

/**
 * Create a new sliding window rate limiter.
 *
 * @param maxRequests Maximum requests per window.
 * @param windowMs Window size in milliseconds.
 * @returns A new SlidingWindowLimiter handle.
 */
let createSlidingWindow = SafeRateLimiterJs.createSlidingWindow

/**
 * Create a new fixed window rate limiter.
 *
 * @param maxRequests Maximum requests per window.
 * @param windowMs Window size in milliseconds.
 * @returns A new FixedWindowLimiter handle.
 */
let createFixedWindow = SafeRateLimiterJs.createFixedWindow

/**
 * Create a new leaky bucket rate limiter.
 *
 * @param capacity Maximum bucket size.
 * @param leakRate Requests leaked per second.
 * @returns A new LeakyBucket handle.
 */
let createLeakyBucket = SafeRateLimiterJs.createLeakyBucket

/**
 * Create a new concurrency limiter.
 *
 * @param maxConcurrent Maximum concurrent operations.
 * @returns A new ConcurrencyLimiter handle.
 */
let createConcurrencyLimiter = SafeRateLimiterJs.createConcurrencyLimiter
