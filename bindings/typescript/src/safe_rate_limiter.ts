// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

export interface RateLimitResult {
  allowed: boolean;
  remaining: number;
  resetAt: number;
  retryAfter?: number;
}

/**
 * TokenBucket implements the token bucket rate limiting algorithm.
 */
export class TokenBucket {
  private readonly capacity: number;
  private readonly refillRate: number; // tokens per second
  private tokens: number;
  private lastRefill: number;
  private readonly getNow: () => number;

  /**
   * Create a token bucket.
   * @param capacity Maximum tokens
   * @param refillRate Tokens added per second
   * @param getNow Function to get current time (for testing)
   */
  constructor(capacity: number, refillRate: number, getNow: () => number = Date.now) {
    this.capacity = Math.max(1, capacity);
    this.refillRate = Math.max(0.001, refillRate);
    this.tokens = this.capacity;
    this.getNow = getNow;
    this.lastRefill = this.getNow();
  }

  private refill(): void {
    const now = this.getNow();
    const elapsed = (now - this.lastRefill) / 1000;
    const tokensToAdd = elapsed * this.refillRate;
    this.tokens = Math.min(this.capacity, this.tokens + tokensToAdd);
    this.lastRefill = now;
  }

  /**
   * Try to consume tokens.
   */
  tryConsume(tokens: number = 1): RateLimitResult {
    this.refill();

    if (tokens <= this.tokens) {
      this.tokens -= tokens;
      return {
        allowed: true,
        remaining: Math.floor(this.tokens),
        resetAt: this.lastRefill + ((this.capacity - this.tokens) / this.refillRate) * 1000,
      };
    }

    const tokensNeeded = tokens - this.tokens;
    const retryAfter = (tokensNeeded / this.refillRate) * 1000;

    return {
      allowed: false,
      remaining: Math.floor(this.tokens),
      resetAt: this.lastRefill + (this.capacity / this.refillRate) * 1000,
      retryAfter: Math.ceil(retryAfter),
    };
  }

  /**
   * Get current token count.
   */
  get available(): number {
    this.refill();
    return Math.floor(this.tokens);
  }

  /**
   * Reset bucket to full capacity.
   */
  reset(): void {
    this.tokens = this.capacity;
    this.lastRefill = this.getNow();
  }
}

/**
 * SlidingWindowLimiter implements sliding window rate limiting.
 */
export class SlidingWindowLimiter {
  private readonly maxRequests: number;
  private readonly windowMs: number;
  private readonly timestamps: number[];
  private readonly getNow: () => number;

  /**
   * Create a sliding window limiter.
   * @param maxRequests Maximum requests per window
   * @param windowMs Window size in milliseconds
   * @param getNow Function to get current time (for testing)
   */
  constructor(maxRequests: number, windowMs: number, getNow: () => number = Date.now) {
    this.maxRequests = Math.max(1, maxRequests);
    this.windowMs = Math.max(1, windowMs);
    this.timestamps = [];
    this.getNow = getNow;
  }

  private cleanup(): void {
    const cutoff = this.getNow() - this.windowMs;
    while (this.timestamps.length > 0 && this.timestamps[0] < cutoff) {
      this.timestamps.shift();
    }
  }

  /**
   * Try to record a request.
   */
  tryAcquire(): RateLimitResult {
    this.cleanup();
    const now = this.getNow();

    if (this.timestamps.length < this.maxRequests) {
      this.timestamps.push(now);
      return {
        allowed: true,
        remaining: this.maxRequests - this.timestamps.length,
        resetAt: now + this.windowMs,
      };
    }

    const oldestTimestamp = this.timestamps[0];
    const retryAfter = oldestTimestamp + this.windowMs - now;

    return {
      allowed: false,
      remaining: 0,
      resetAt: oldestTimestamp + this.windowMs,
      retryAfter: Math.ceil(retryAfter),
    };
  }

  /**
   * Get current request count in window.
   */
  get count(): number {
    this.cleanup();
    return this.timestamps.length;
  }

  /**
   * Reset the limiter.
   */
  reset(): void {
    this.timestamps.length = 0;
  }
}

/**
 * FixedWindowLimiter implements fixed window rate limiting.
 */
export class FixedWindowLimiter {
  private readonly maxRequests: number;
  private readonly windowMs: number;
  private windowStart: number;
  private requestCount: number;
  private readonly getNow: () => number;

  /**
   * Create a fixed window limiter.
   * @param maxRequests Maximum requests per window
   * @param windowMs Window size in milliseconds
   * @param getNow Function to get current time (for testing)
   */
  constructor(maxRequests: number, windowMs: number, getNow: () => number = Date.now) {
    this.maxRequests = Math.max(1, maxRequests);
    this.windowMs = Math.max(1, windowMs);
    this.getNow = getNow;
    this.windowStart = this.getNow();
    this.requestCount = 0;
  }

  private maybeResetWindow(): void {
    const now = this.getNow();
    if (now >= this.windowStart + this.windowMs) {
      this.windowStart = now;
      this.requestCount = 0;
    }
  }

  /**
   * Try to record a request.
   */
  tryAcquire(): RateLimitResult {
    this.maybeResetWindow();
    const now = this.getNow();
    const windowEnd = this.windowStart + this.windowMs;

    if (this.requestCount < this.maxRequests) {
      this.requestCount++;
      return {
        allowed: true,
        remaining: this.maxRequests - this.requestCount,
        resetAt: windowEnd,
      };
    }

    return {
      allowed: false,
      remaining: 0,
      resetAt: windowEnd,
      retryAfter: Math.ceil(windowEnd - now),
    };
  }

  /**
   * Get current request count in window.
   */
  get count(): number {
    this.maybeResetWindow();
    return this.requestCount;
  }

  /**
   * Reset the limiter.
   */
  reset(): void {
    this.windowStart = this.getNow();
    this.requestCount = 0;
  }
}

/**
 * LeakyBucket implements the leaky bucket rate limiting algorithm.
 */
export class LeakyBucket {
  private readonly capacity: number;
  private readonly leakRate: number; // requests per second
  private waterLevel: number;
  private lastLeak: number;
  private readonly getNow: () => number;

  /**
   * Create a leaky bucket.
   * @param capacity Maximum bucket size
   * @param leakRate Requests leaked per second
   * @param getNow Function to get current time (for testing)
   */
  constructor(capacity: number, leakRate: number, getNow: () => number = Date.now) {
    this.capacity = Math.max(1, capacity);
    this.leakRate = Math.max(0.001, leakRate);
    this.waterLevel = 0;
    this.getNow = getNow;
    this.lastLeak = this.getNow();
  }

  private leak(): void {
    const now = this.getNow();
    const elapsed = (now - this.lastLeak) / 1000;
    const leaked = elapsed * this.leakRate;
    this.waterLevel = Math.max(0, this.waterLevel - leaked);
    this.lastLeak = now;
  }

  /**
   * Try to add a request.
   */
  tryAdd(amount: number = 1): RateLimitResult {
    this.leak();

    if (this.waterLevel + amount <= this.capacity) {
      this.waterLevel += amount;
      return {
        allowed: true,
        remaining: Math.floor(this.capacity - this.waterLevel),
        resetAt: this.lastLeak + (this.waterLevel / this.leakRate) * 1000,
      };
    }

    const overflow = this.waterLevel + amount - this.capacity;
    const retryAfter = (overflow / this.leakRate) * 1000;

    return {
      allowed: false,
      remaining: Math.floor(this.capacity - this.waterLevel),
      resetAt: this.lastLeak + (this.capacity / this.leakRate) * 1000,
      retryAfter: Math.ceil(retryAfter),
    };
  }

  /**
   * Get current water level.
   */
  get level(): number {
    this.leak();
    return this.waterLevel;
  }

  /**
   * Reset the bucket.
   */
  reset(): void {
    this.waterLevel = 0;
    this.lastLeak = this.getNow();
  }
}

/**
 * ConcurrencyLimiter limits concurrent operations.
 */
export class ConcurrencyLimiter {
  private readonly maxConcurrent: number;
  private current: number = 0;

  constructor(maxConcurrent: number) {
    this.maxConcurrent = Math.max(1, maxConcurrent);
  }

  /**
   * Try to acquire a slot.
   */
  tryAcquire(): boolean {
    if (this.current < this.maxConcurrent) {
      this.current++;
      return true;
    }
    return false;
  }

  /**
   * Release a slot.
   */
  release(): void {
    if (this.current > 0) {
      this.current--;
    }
  }

  /**
   * Get current concurrent count.
   */
  get count(): number {
    return this.current;
  }

  /**
   * Get remaining slots.
   */
  get available(): number {
    return this.maxConcurrent - this.current;
  }
}

export const SafeRateLimiter = {
  TokenBucket,
  SlidingWindowLimiter,
  FixedWindowLimiter,
  LeakyBucket,
  ConcurrencyLimiter,
};
