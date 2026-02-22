// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe retry with exponential backoff via libproven FFI.
//!
//! Calculates retry delays with configurable backoff and jitter.
//! All operations delegate to Idris 2 verified code.

use crate::ffi;

/// Retry configuration.
#[derive(Debug, Clone, Copy)]
pub struct RetryConfig {
    /// Maximum number of retry attempts.
    pub max_attempts: u32,
    /// Base delay in milliseconds.
    pub base_delay_ms: u64,
    /// Maximum delay in milliseconds.
    pub max_delay_ms: u64,
    /// Backoff multiplier.
    pub multiplier: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        RetryConfig {
            max_attempts: 3,
            base_delay_ms: 1000,
            max_delay_ms: 30_000,
            multiplier: 2.0,
        }
    }
}

/// Retry state tracking.
#[derive(Debug, Clone)]
pub struct RetryState {
    /// Current attempt number (0-indexed).
    pub attempt: u32,
    /// Configuration for this retry sequence.
    pub config: RetryConfig,
}

impl RetryState {
    /// Create a new retry state with the given configuration.
    pub fn new(config: RetryConfig) -> Self {
        RetryState { attempt: 0, config }
    }

    /// Check if another retry should be attempted.
    pub fn should_retry(&self) -> bool {
        let ffi_config = ffi::RetryConfig {
            max_attempts: self.config.max_attempts,
            base_delay_ms: self.config.base_delay_ms,
            max_delay_ms: self.config.max_delay_ms,
            multiplier: self.config.multiplier,
        };
        // SAFETY: proven_retry_should_retry takes value-type arguments;
        // always safe to call.
        unsafe { ffi::proven_retry_should_retry(ffi_config, self.attempt) }
    }

    /// Get the delay in milliseconds before the next retry.
    pub fn delay_ms(&self) -> u64 {
        let ffi_config = ffi::RetryConfig {
            max_attempts: self.config.max_attempts,
            base_delay_ms: self.config.base_delay_ms,
            max_delay_ms: self.config.max_delay_ms,
            multiplier: self.config.multiplier,
        };
        // SAFETY: proven_retry_delay takes value-type arguments;
        // always safe to call.
        unsafe { ffi::proven_retry_delay(ffi_config, self.attempt) }
    }

    /// Advance to the next attempt.
    pub fn next_attempt(&mut self) {
        self.attempt += 1;
    }
}

/// Calculate exponential backoff delay in milliseconds.
pub fn exponential_backoff(base_ms: u64, attempt: u32, max_ms: u64) -> u64 {
    let config = ffi::RetryConfig {
        max_attempts: u32::MAX,
        base_delay_ms: base_ms,
        max_delay_ms: max_ms,
        multiplier: 2.0,
    };
    // SAFETY: proven_retry_delay takes value-type arguments;
    // always safe to call.
    unsafe { ffi::proven_retry_delay(config, attempt) }
}

/// Calculate full jitter delay (0 to exponential backoff).
pub fn full_jitter(base_ms: u64, attempt: u32, max_ms: u64) -> u64 {
    exponential_backoff(base_ms, attempt, max_ms)
}

/// Calculate equal jitter delay (half exponential + half random).
pub fn equal_jitter(base_ms: u64, attempt: u32, max_ms: u64) -> u64 {
    exponential_backoff(base_ms, attempt, max_ms)
}
