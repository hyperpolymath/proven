// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe retry mechanisms with exponential backoff.
//!
//! Provides configurable retry strategies with jitter
//! for resilient distributed systems.

use std::time::Duration;

/// Retry strategy configuration.
#[derive(Debug, Clone)]
pub struct RetryConfig {
    /// Maximum number of retry attempts.
    pub max_attempts: u32,
    /// Initial delay before first retry.
    pub initial_delay_ms: u64,
    /// Maximum delay between retries.
    pub max_delay_ms: u64,
    /// Multiplier for exponential backoff.
    pub multiplier: f64,
    /// Whether to add jitter to delays.
    pub use_jitter: bool,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_attempts: 3,
            initial_delay_ms: 100,
            max_delay_ms: 10000,
            multiplier: 2.0,
            use_jitter: true,
        }
    }
}

/// Retry state tracker.
#[derive(Debug, Clone)]
pub struct RetryState {
    config: RetryConfig,
    attempts: u32,
    current_delay_ms: u64,
}

impl RetryState {
    /// Create a new retry state with the given configuration.
    pub fn new(config: RetryConfig) -> Self {
        Self {
            current_delay_ms: config.initial_delay_ms,
            config,
            attempts: 0,
        }
    }

    /// Create with default configuration.
    pub fn with_defaults() -> Self {
        Self::new(RetryConfig::default())
    }

    /// Check if more retries are available.
    pub fn should_retry(&self) -> bool {
        self.attempts < self.config.max_attempts
    }

    /// Get current attempt number (0-indexed).
    pub fn current_attempt(&self) -> u32 {
        self.attempts
    }

    /// Get remaining attempts.
    pub fn remaining_attempts(&self) -> u32 {
        self.config.max_attempts.saturating_sub(self.attempts)
    }

    /// Record an attempt and calculate next delay.
    pub fn record_attempt(&mut self) -> Option<Duration> {
        if !self.should_retry() {
            return None;
        }

        self.attempts += 1;

        let delay = self.current_delay_ms;

        // Calculate next delay with exponential backoff
        let next_delay = (self.current_delay_ms as f64 * self.config.multiplier) as u64;
        self.current_delay_ms = next_delay.min(self.config.max_delay_ms);

        Some(Duration::from_millis(delay))
    }

    /// Get delay with optional jitter.
    pub fn get_delay_with_jitter(&self, jitter_factor: f64) -> Duration {
        if !self.config.use_jitter {
            return Duration::from_millis(self.current_delay_ms);
        }

        // Simple deterministic jitter based on attempt count
        let jitter = (self.attempts as f64 * jitter_factor * self.current_delay_ms as f64) as u64;
        let jittered_delay = self.current_delay_ms.saturating_add(jitter % (self.current_delay_ms / 2 + 1));
        Duration::from_millis(jittered_delay.min(self.config.max_delay_ms))
    }

    /// Reset the retry state.
    pub fn reset(&mut self) {
        self.attempts = 0;
        self.current_delay_ms = self.config.initial_delay_ms;
    }
}

/// Calculate exponential backoff delay.
pub fn exponential_backoff(attempt: u32, base_ms: u64, max_ms: u64, multiplier: f64) -> u64 {
    let delay = base_ms as f64 * multiplier.powi(attempt as i32);
    (delay as u64).min(max_ms)
}

/// Calculate delay with full jitter (Amazon style).
pub fn full_jitter(base_delay_ms: u64, seed: u64) -> u64 {
    // Deterministic pseudo-random based on seed
    let hash = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
    hash % (base_delay_ms + 1)
}

/// Calculate delay with equal jitter.
pub fn equal_jitter(base_delay_ms: u64, seed: u64) -> u64 {
    let half = base_delay_ms / 2;
    let hash = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
    half + (hash % (half + 1))
}

/// Calculate delay with decorrelated jitter.
pub fn decorrelated_jitter(prev_delay_ms: u64, base_ms: u64, cap_ms: u64, seed: u64) -> u64 {
    let hash = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
    let range = prev_delay_ms.saturating_mul(3).saturating_sub(base_ms);
    let delay = base_ms.saturating_add(hash % (range + 1));
    delay.min(cap_ms)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_retry_state() {
        let mut state = RetryState::new(RetryConfig {
            max_attempts: 3,
            initial_delay_ms: 100,
            max_delay_ms: 1000,
            multiplier: 2.0,
            use_jitter: false,
        });

        assert!(state.should_retry());
        assert_eq!(state.remaining_attempts(), 3);

        let delay1 = state.record_attempt().unwrap();
        assert_eq!(delay1, Duration::from_millis(100));

        let delay2 = state.record_attempt().unwrap();
        assert_eq!(delay2, Duration::from_millis(200));

        let delay3 = state.record_attempt().unwrap();
        assert_eq!(delay3, Duration::from_millis(400));

        assert!(!state.should_retry());
        assert!(state.record_attempt().is_none());
    }

    #[test]
    fn test_exponential_backoff() {
        assert_eq!(exponential_backoff(0, 100, 10000, 2.0), 100);
        assert_eq!(exponential_backoff(1, 100, 10000, 2.0), 200);
        assert_eq!(exponential_backoff(2, 100, 10000, 2.0), 400);
        assert_eq!(exponential_backoff(10, 100, 10000, 2.0), 10000); // capped
    }
}
