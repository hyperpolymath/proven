// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe rate limiting algorithms.
//!
//! Provides token bucket, sliding window, and fixed window
//! rate limiters for controlling request rates.

/// Result of a rate limit check.
#[derive(Debug, Clone, PartialEq)]
pub enum RateLimitResult {
    /// Request is allowed.
    Allowed,
    /// Request is denied; contains retry-after time in time units.
    Denied(u64),
}

impl RateLimitResult {
    /// Check if the request was allowed.
    pub fn is_allowed(&self) -> bool {
        matches!(self, RateLimitResult::Allowed)
    }
}

/// Token bucket rate limiter.
#[derive(Debug, Clone)]
pub struct TokenBucket {
    capacity: u64,
    tokens: u64,
    refill_rate: u64,
    last_refill: u64,
}

impl TokenBucket {
    /// Create a new token bucket.
    pub fn new(capacity: u64, refill_rate: u64) -> Self {
        Self {
            capacity,
            tokens: capacity,
            refill_rate,
            last_refill: 0,
        }
    }

    /// Refill tokens based on elapsed time.
    pub fn refill(&mut self, current_time: u64) {
        let elapsed = current_time.saturating_sub(self.last_refill);
        let new_tokens = self.refill_rate.saturating_mul(elapsed);
        self.tokens = self.capacity.min(self.tokens.saturating_add(new_tokens));
        self.last_refill = current_time;
    }

    /// Try to acquire tokens.
    pub fn try_acquire(&mut self, count: u64, current_time: u64) -> RateLimitResult {
        self.refill(current_time);

        if self.tokens >= count {
            self.tokens -= count;
            return RateLimitResult::Allowed;
        }

        let needed = count.saturating_sub(self.tokens);
        let wait_time = needed.saturating_add(self.refill_rate - 1) / self.refill_rate;
        RateLimitResult::Denied(wait_time)
    }

    /// Check if request would be allowed (without consuming).
    pub fn would_allow(&self, count: u64, current_time: u64) -> bool {
        let mut copy = self.clone();
        copy.refill(current_time);
        copy.tokens >= count
    }

    /// Get current token count.
    pub fn current_tokens(&self, current_time: u64) -> u64 {
        let mut copy = self.clone();
        copy.refill(current_time);
        copy.tokens
    }
}

/// Sliding window rate limiter.
#[derive(Debug, Clone)]
pub struct SlidingWindow {
    max_requests: u64,
    window_size: u64,
    requests: Vec<u64>,
}

impl SlidingWindow {
    /// Create a new sliding window rate limiter.
    pub fn new(max_requests: u64, window_size: u64) -> Self {
        Self {
            max_requests,
            window_size,
            requests: Vec::new(),
        }
    }

    /// Prune expired requests.
    fn prune(&mut self, current_time: u64) {
        let cutoff = current_time.saturating_sub(self.window_size);
        self.requests.retain(|&ts| ts >= cutoff);
    }

    /// Try to make a request.
    pub fn try_request(&mut self, current_time: u64) -> RateLimitResult {
        self.prune(current_time);

        if (self.requests.len() as u64) < self.max_requests {
            self.requests.push(current_time);
            return RateLimitResult::Allowed;
        }

        if self.requests.is_empty() {
            return RateLimitResult::Denied(self.window_size);
        }

        let oldest = *self.requests.iter().min().unwrap_or(&current_time);
        let retry_after = self.window_size.saturating_sub(current_time.saturating_sub(oldest));
        RateLimitResult::Denied(retry_after)
    }

    /// Get current request count in window.
    pub fn current_count(&mut self, current_time: u64) -> usize {
        self.prune(current_time);
        self.requests.len()
    }

    /// Get remaining allowed requests.
    pub fn remaining(&mut self, current_time: u64) -> u64 {
        self.prune(current_time);
        self.max_requests.saturating_sub(self.requests.len() as u64)
    }
}

/// Fixed window counter.
#[derive(Debug, Clone)]
pub struct FixedWindow {
    max_requests: u64,
    window_size: u64,
    window_start: u64,
    count: u64,
}

impl FixedWindow {
    /// Create a new fixed window rate limiter.
    pub fn new(max_requests: u64, window_size: u64) -> Self {
        Self {
            max_requests,
            window_size,
            window_start: 0,
            count: 0,
        }
    }

    /// Try to make a request.
    pub fn try_request(&mut self, current_time: u64) -> RateLimitResult {
        let window_end = self.window_start.saturating_add(self.window_size);

        // New window?
        if current_time >= window_end {
            self.window_start = (current_time / self.window_size) * self.window_size;
            self.count = 0;
        }

        if self.count < self.max_requests {
            self.count += 1;
            return RateLimitResult::Allowed;
        }

        let retry_after = self.window_start.saturating_add(self.window_size).saturating_sub(current_time);
        RateLimitResult::Denied(retry_after)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_bucket() {
        let mut bucket = TokenBucket::new(10, 1);
        assert_eq!(bucket.try_acquire(5, 0), RateLimitResult::Allowed);
        assert_eq!(bucket.current_tokens(0), 5);
    }

    #[test]
    fn test_sliding_window() {
        let mut window = SlidingWindow::new(3, 10);
        assert_eq!(window.try_request(0), RateLimitResult::Allowed);
        assert_eq!(window.try_request(1), RateLimitResult::Allowed);
        assert_eq!(window.try_request(2), RateLimitResult::Allowed);
        assert!(matches!(window.try_request(3), RateLimitResult::Denied(_)));
    }

    #[test]
    fn test_fixed_window() {
        let mut window = FixedWindow::new(2, 10);
        assert_eq!(window.try_request(0), RateLimitResult::Allowed);
        assert_eq!(window.try_request(1), RateLimitResult::Allowed);
        assert!(matches!(window.try_request(2), RateLimitResult::Denied(_)));
    }
}
