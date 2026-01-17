// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeRateLimiter - Token bucket and sliding window rate limiting.
 *
 * Provides safe rate limiting algorithms that cannot crash:
 * - Token bucket rate limiter
 * - Sliding window rate limiter
 * - Fixed window counter
 *
 * All operations return Result types for error handling.
 */

// ============================================================================
// Types
// ============================================================================

/** Result of a rate limit check */
type rateLimitResult =
  | Allowed
  | Denied({retryAfter: float}) // retry_after in time units (milliseconds)

/** Token bucket rate limiter state */
type tokenBucket = {
  mutable capacity: float,
  mutable tokens: float,
  mutable refillRate: float, // tokens per millisecond
  mutable lastRefill: float,
}

/** Sliding window rate limiter state */
type slidingWindow = {
  maxRequests: int,
  windowSize: float, // milliseconds
  mutable requests: array<float>,
}

/** Fixed window counter state */
type fixedWindow = {
  maxRequests: int,
  windowSize: float, // milliseconds
  mutable windowStart: float,
  mutable count: int,
}

// ============================================================================
// Token Bucket Rate Limiter
// ============================================================================

/** Create a new token bucket rate limiter */
let makeTokenBucket = (~capacity: float, ~refillRate: float): result<tokenBucket, string> => {
  if capacity <= 0.0 {
    Error("Capacity must be positive")
  } else if refillRate <= 0.0 {
    Error("Refill rate must be positive")
  } else {
    Ok({
      capacity,
      tokens: capacity,
      refillRate,
      lastRefill: 0.0,
    })
  }
}

/** Refill tokens based on elapsed time */
let refillTokens = (bucket: tokenBucket, currentTime: float): unit => {
  let elapsed = currentTime -. bucket.lastRefill
  if elapsed > 0.0 {
    let newTokens = bucket.refillRate *. elapsed
    bucket.tokens = Js.Math.min_float(bucket.capacity, bucket.tokens +. newTokens)
    bucket.lastRefill = currentTime
  }
}

/** Try to acquire tokens from the bucket */
let tryAcquire = (bucket: tokenBucket, count: float, currentTime: float): rateLimitResult => {
  refillTokens(bucket, currentTime)

  if bucket.tokens >= count {
    bucket.tokens = bucket.tokens -. count
    Allowed
  } else {
    let needed = count -. bucket.tokens
    let waitTime = needed /. bucket.refillRate
    Denied({retryAfter: waitTime})
  }
}

/** Check if request would be allowed without consuming tokens */
let wouldAllowTokenBucket = (bucket: tokenBucket, count: float, currentTime: float): bool => {
  let elapsed = currentTime -. bucket.lastRefill
  let projectedTokens = if elapsed > 0.0 {
    Js.Math.min_float(bucket.capacity, bucket.tokens +. bucket.refillRate *. elapsed)
  } else {
    bucket.tokens
  }
  projectedTokens >= count
}

/** Get current token count */
let currentTokens = (bucket: tokenBucket, currentTime: float): float => {
  let elapsed = currentTime -. bucket.lastRefill
  if elapsed > 0.0 {
    Js.Math.min_float(bucket.capacity, bucket.tokens +. bucket.refillRate *. elapsed)
  } else {
    bucket.tokens
  }
}

/** Get remaining capacity */
let remainingCapacity = (bucket: tokenBucket, currentTime: float): float => {
  currentTokens(bucket, currentTime)
}

// ============================================================================
// Sliding Window Rate Limiter
// ============================================================================

/** Create a new sliding window rate limiter */
let makeSlidingWindow = (~maxRequests: int, ~windowSize: float): result<slidingWindow, string> => {
  if maxRequests <= 0 {
    Error("Max requests must be positive")
  } else if windowSize <= 0.0 {
    Error("Window size must be positive")
  } else {
    Ok({
      maxRequests,
      windowSize,
      requests: [],
    })
  }
}

/** Prune expired requests from the window */
let pruneRequests = (window: slidingWindow, currentTime: float): unit => {
  let cutoff = currentTime -. window.windowSize
  window.requests = Belt.Array.keep(window.requests, ts => ts >= cutoff)
}

/** Try to make a request within the sliding window */
let tryRequestSlidingWindow = (window: slidingWindow, currentTime: float): rateLimitResult => {
  pruneRequests(window, currentTime)

  if Belt.Array.length(window.requests) < window.maxRequests {
    let _ = Js.Array2.push(window.requests, currentTime)
    Allowed
  } else {
    // Find oldest request
    let oldest = Belt.Array.reduce(window.requests, currentTime, (acc, ts) =>
      Js.Math.min_float(acc, ts)
    )
    let retryAfter = window.windowSize -. (currentTime -. oldest)
    Denied({retryAfter: Js.Math.max_float(0.0, retryAfter)})
  }
}

/** Get current request count in window */
let currentCountSlidingWindow = (window: slidingWindow, currentTime: float): int => {
  pruneRequests(window, currentTime)
  Belt.Array.length(window.requests)
}

/** Get remaining allowed requests in window */
let remainingSlidingWindow = (window: slidingWindow, currentTime: float): int => {
  pruneRequests(window, currentTime)
  window.maxRequests - Belt.Array.length(window.requests)
}

// ============================================================================
// Fixed Window Counter
// ============================================================================

/** Create a new fixed window counter */
let makeFixedWindow = (~maxRequests: int, ~windowSize: float): result<fixedWindow, string> => {
  if maxRequests <= 0 {
    Error("Max requests must be positive")
  } else if windowSize <= 0.0 {
    Error("Window size must be positive")
  } else {
    Ok({
      maxRequests,
      windowSize,
      windowStart: 0.0,
      count: 0,
    })
  }
}

/** Try to make a request within the fixed window */
let tryRequestFixedWindow = (window: fixedWindow, currentTime: float): rateLimitResult => {
  let windowEnd = window.windowStart +. window.windowSize

  // Check if we're in a new window
  if currentTime >= windowEnd {
    window.windowStart = Js.Math.floor_float(currentTime /. window.windowSize) *. window.windowSize
    window.count = 0
  }

  if window.count < window.maxRequests {
    window.count = window.count + 1
    Allowed
  } else {
    let retryAfter = window.windowStart +. window.windowSize -. currentTime
    Denied({retryAfter: Js.Math.max_float(0.0, retryAfter)})
  }
}

/** Get current request count in fixed window */
let currentCountFixedWindow = (window: fixedWindow, currentTime: float): int => {
  let windowEnd = window.windowStart +. window.windowSize
  if currentTime >= windowEnd {
    0
  } else {
    window.count
  }
}

/** Get remaining allowed requests in fixed window */
let remainingFixedWindow = (window: fixedWindow, currentTime: float): int => {
  let windowEnd = window.windowStart +. window.windowSize
  if currentTime >= windowEnd {
    window.maxRequests
  } else {
    window.maxRequests - window.count
  }
}

// ============================================================================
// Utility Functions
// ============================================================================

/** Check if a rate limit result is allowed */
let isAllowed = (result: rateLimitResult): bool => {
  switch result {
  | Allowed => true
  | Denied(_) => false
  }
}

/** Get retry after time from a rate limit result */
let getRetryAfter = (result: rateLimitResult): option<float> => {
  switch result {
  | Allowed => None
  | Denied({retryAfter}) => Some(retryAfter)
  }
}

/** Get current time in milliseconds */
let now = (): float => {
  Js.Date.now()
}

/** Reset token bucket to full capacity */
let resetTokenBucket = (bucket: tokenBucket): unit => {
  bucket.tokens = bucket.capacity
  bucket.lastRefill = 0.0
}

/** Reset sliding window (clear all requests) */
let resetSlidingWindow = (window: slidingWindow): unit => {
  window.requests = []
}

/** Reset fixed window counter */
let resetFixedWindow = (window: fixedWindow): unit => {
  window.windowStart = 0.0
  window.count = 0
}
