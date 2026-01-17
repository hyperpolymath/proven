// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeRateLimiter - Token bucket and sliding window rate limiters.
 *
 * Provides rate limiting with configurable algorithms.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Token bucket rate limiter.
 */
export class TokenBucket {
  /** @type {number} */
  #capacity;
  /** @type {number} */
  #tokens;
  /** @type {number} */
  #refillRate;
  /** @type {number} */
  #lastRefill;

  /**
   * Create a token bucket rate limiter.
   *
   * @param {Object} options - Options
   * @param {number} options.capacity - Maximum tokens
   * @param {number} options.refillRate - Tokens per second
   */
  constructor(options) {
    const { capacity, refillRate } = options;

    if (capacity < 1) {
      throw new Error('Capacity must be at least 1');
    }
    if (refillRate <= 0) {
      throw new Error('Refill rate must be positive');
    }

    this.#capacity = capacity;
    this.#tokens = capacity;
    this.#refillRate = refillRate;
    this.#lastRefill = Date.now();
  }

  /**
   * Get current token count.
   *
   * @returns {number}
   */
  get tokens() {
    this.#refill();
    return this.#tokens;
  }

  /**
   * Get capacity.
   *
   * @returns {number}
   */
  get capacity() {
    return this.#capacity;
  }

  /**
   * Try to acquire tokens.
   *
   * @param {number} [count=1] - Number of tokens to acquire
   * @returns {boolean} True if tokens were acquired
   */
  tryAcquire(count = 1) {
    if (count < 1) {
      return false;
    }

    this.#refill();

    if (this.#tokens >= count) {
      this.#tokens -= count;
      return true;
    }

    return false;
  }

  /**
   * Check if tokens are available (without consuming).
   *
   * @param {number} [count=1] - Number of tokens to check
   * @returns {boolean}
   */
  canAcquire(count = 1) {
    this.#refill();
    return this.#tokens >= count;
  }

  /**
   * Get time until tokens are available.
   *
   * @param {number} [count=1] - Number of tokens needed
   * @returns {number} Milliseconds until available (0 if available now)
   */
  timeUntilAvailable(count = 1) {
    this.#refill();

    if (this.#tokens >= count) {
      return 0;
    }

    const needed = count - this.#tokens;
    return Math.ceil((needed / this.#refillRate) * 1000);
  }

  /**
   * Reset the bucket to full capacity.
   */
  reset() {
    this.#tokens = this.#capacity;
    this.#lastRefill = Date.now();
  }

  /**
   * Refill tokens based on elapsed time.
   */
  #refill() {
    const now = Date.now();
    const elapsed = (now - this.#lastRefill) / 1000;
    const newTokens = elapsed * this.#refillRate;

    this.#tokens = Math.min(this.#capacity, this.#tokens + newTokens);
    this.#lastRefill = now;
  }
}

/**
 * Sliding window rate limiter.
 */
export class SlidingWindowLimiter {
  /** @type {number} */
  #maxRequests;
  /** @type {number} */
  #windowMs;
  /** @type {number[]} */
  #timestamps;

  /**
   * Create a sliding window rate limiter.
   *
   * @param {Object} options - Options
   * @param {number} options.maxRequests - Maximum requests per window
   * @param {number} options.windowMs - Window size in milliseconds
   */
  constructor(options) {
    const { maxRequests, windowMs } = options;

    if (maxRequests < 1) {
      throw new Error('Max requests must be at least 1');
    }
    if (windowMs < 1) {
      throw new Error('Window must be at least 1ms');
    }

    this.#maxRequests = maxRequests;
    this.#windowMs = windowMs;
    this.#timestamps = [];
  }

  /**
   * Get maximum requests per window.
   *
   * @returns {number}
   */
  get maxRequests() {
    return this.#maxRequests;
  }

  /**
   * Get window size in milliseconds.
   *
   * @returns {number}
   */
  get windowMs() {
    return this.#windowMs;
  }

  /**
   * Get current request count in window.
   *
   * @returns {number}
   */
  get currentCount() {
    this.#cleanup();
    return this.#timestamps.length;
  }

  /**
   * Try to record a request.
   *
   * @returns {boolean} True if request was allowed
   */
  tryRequest() {
    this.#cleanup();

    if (this.#timestamps.length >= this.#maxRequests) {
      return false;
    }

    this.#timestamps.push(Date.now());
    return true;
  }

  /**
   * Check if request would be allowed (without recording).
   *
   * @returns {boolean}
   */
  canRequest() {
    this.#cleanup();
    return this.#timestamps.length < this.#maxRequests;
  }

  /**
   * Get remaining requests in current window.
   *
   * @returns {number}
   */
  remaining() {
    this.#cleanup();
    return Math.max(0, this.#maxRequests - this.#timestamps.length);
  }

  /**
   * Get time until oldest request expires.
   *
   * @returns {number} Milliseconds until a slot opens (0 if available now)
   */
  timeUntilAvailable() {
    this.#cleanup();

    if (this.#timestamps.length < this.#maxRequests) {
      return 0;
    }

    const oldest = this.#timestamps[0];
    const expiresAt = oldest + this.#windowMs;
    return Math.max(0, expiresAt - Date.now());
  }

  /**
   * Reset the limiter.
   */
  reset() {
    this.#timestamps = [];
  }

  /**
   * Remove expired timestamps.
   */
  #cleanup() {
    const cutoff = Date.now() - this.#windowMs;
    this.#timestamps = this.#timestamps.filter((timestamp) => timestamp > cutoff);
  }
}

/**
 * Fixed window rate limiter.
 */
export class FixedWindowLimiter {
  /** @type {number} */
  #maxRequests;
  /** @type {number} */
  #windowMs;
  /** @type {number} */
  #count;
  /** @type {number} */
  #windowStart;

  /**
   * Create a fixed window rate limiter.
   *
   * @param {Object} options - Options
   * @param {number} options.maxRequests - Maximum requests per window
   * @param {number} options.windowMs - Window size in milliseconds
   */
  constructor(options) {
    const { maxRequests, windowMs } = options;

    if (maxRequests < 1) {
      throw new Error('Max requests must be at least 1');
    }
    if (windowMs < 1) {
      throw new Error('Window must be at least 1ms');
    }

    this.#maxRequests = maxRequests;
    this.#windowMs = windowMs;
    this.#count = 0;
    this.#windowStart = Date.now();
  }

  /**
   * Get current request count in window.
   *
   * @returns {number}
   */
  get currentCount() {
    this.#checkWindow();
    return this.#count;
  }

  /**
   * Try to record a request.
   *
   * @returns {boolean} True if request was allowed
   */
  tryRequest() {
    this.#checkWindow();

    if (this.#count >= this.#maxRequests) {
      return false;
    }

    this.#count++;
    return true;
  }

  /**
   * Get remaining requests in current window.
   *
   * @returns {number}
   */
  remaining() {
    this.#checkWindow();
    return Math.max(0, this.#maxRequests - this.#count);
  }

  /**
   * Get time until window resets.
   *
   * @returns {number} Milliseconds until reset
   */
  timeUntilReset() {
    const elapsed = Date.now() - this.#windowStart;
    return Math.max(0, this.#windowMs - elapsed);
  }

  /**
   * Reset the limiter.
   */
  reset() {
    this.#count = 0;
    this.#windowStart = Date.now();
  }

  /**
   * Check and reset window if needed.
   */
  #checkWindow() {
    const now = Date.now();
    if (now - this.#windowStart >= this.#windowMs) {
      this.#count = 0;
      this.#windowStart = now;
    }
  }
}

/**
 * Safe rate limiter utilities.
 */
export class SafeRateLimiter {
  /**
   * Create a token bucket rate limiter.
   *
   * @param {number} capacity - Maximum tokens
   * @param {number} refillRate - Tokens per second
   * @returns {TokenBucket}
   */
  static tokenBucket(capacity, refillRate) {
    return new TokenBucket({ capacity, refillRate });
  }

  /**
   * Create a sliding window rate limiter.
   *
   * @param {number} maxRequests - Maximum requests per window
   * @param {number} windowMs - Window size in milliseconds
   * @returns {SlidingWindowLimiter}
   */
  static slidingWindow(maxRequests, windowMs) {
    return new SlidingWindowLimiter({ maxRequests, windowMs });
  }

  /**
   * Create a fixed window rate limiter.
   *
   * @param {number} maxRequests - Maximum requests per window
   * @param {number} windowMs - Window size in milliseconds
   * @returns {FixedWindowLimiter}
   */
  static fixedWindow(maxRequests, windowMs) {
    return new FixedWindowLimiter({ maxRequests, windowMs });
  }
}
