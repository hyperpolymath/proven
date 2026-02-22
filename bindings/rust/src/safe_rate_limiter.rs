// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe rate limiting via libproven FFI.
//!
//! Provides token bucket rate limiting.
//! All operations delegate to Idris 2 verified code.

use crate::core::{Error, Result};
use crate::ffi;

/// Rate limit check result.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RateLimitResult {
    /// Request is allowed.
    Allowed,
    /// Request is denied (rate limit exceeded).
    Denied,
}

/// Token bucket rate limiter.
///
/// The underlying state is managed by libproven. The limiter is freed
/// when this struct is dropped.
pub struct TokenBucket {
    ptr: *mut ffi::RateLimiter,
}

// SAFETY: The underlying rate limiter is managed by libproven.
unsafe impl Send for TokenBucket {}

impl TokenBucket {
    /// Create a new token bucket rate limiter.
    ///
    /// - `capacity`: maximum number of tokens.
    /// - `refill_rate`: tokens added per second.
    pub fn new(capacity: f64, refill_rate: f64) -> Result<Self> {
        // SAFETY: proven_rate_limiter_create takes value-type arguments;
        // always safe to call.
        let ptr = unsafe { ffi::proven_rate_limiter_create(capacity, refill_rate) };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(TokenBucket { ptr })
    }

    /// Try to acquire tokens from the bucket.
    ///
    /// Returns `RateLimitResult::Allowed` if tokens were available,
    /// or `RateLimitResult::Denied` if the rate limit is exceeded.
    pub fn try_acquire(&mut self, tokens: f64) -> RateLimitResult {
        // SAFETY: self.ptr is valid (checked at construction).
        let allowed = unsafe { ffi::proven_rate_limiter_try_acquire(self.ptr, tokens) };
        if allowed {
            RateLimitResult::Allowed
        } else {
            RateLimitResult::Denied
        }
    }
}

impl Drop for TokenBucket {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_rate_limiter_create
        // and has not been freed yet.
        unsafe {
            ffi::proven_rate_limiter_free(self.ptr);
        }
    }
}

/// Fixed-window rate limiter.
///
/// Wraps the token bucket with fixed-window semantics.
pub struct FixedWindow {
    inner: TokenBucket,
}

impl FixedWindow {
    /// Create a fixed-window rate limiter.
    pub fn new(max_requests: f64, window_seconds: f64) -> Result<Self> {
        let refill_rate = max_requests / window_seconds;
        Ok(FixedWindow {
            inner: TokenBucket::new(max_requests, refill_rate)?,
        })
    }

    /// Check if a request is allowed.
    pub fn check(&mut self) -> RateLimitResult {
        self.inner.try_acquire(1.0)
    }
}

/// Sliding-window rate limiter.
///
/// Wraps the token bucket with sliding-window semantics.
pub struct SlidingWindow {
    inner: TokenBucket,
}

impl SlidingWindow {
    /// Create a sliding-window rate limiter.
    pub fn new(max_requests: f64, window_seconds: f64) -> Result<Self> {
        let refill_rate = max_requests / window_seconds;
        Ok(SlidingWindow {
            inner: TokenBucket::new(max_requests, refill_rate)?,
        })
    }

    /// Check if a request is allowed.
    pub fn check(&mut self) -> RateLimitResult {
        self.inner.try_acquire(1.0)
    }
}
