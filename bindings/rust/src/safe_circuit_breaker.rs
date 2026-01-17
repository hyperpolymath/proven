// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe circuit breaker pattern for fault tolerance.
//!
//! Provides automatic failure detection and recovery
//! to prevent cascading failures in distributed systems.

/// Circuit breaker states.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CircuitState {
    /// Normal operation - requests are allowed.
    Closed,
    /// Failing - requests are rejected.
    Open,
    /// Testing recovery - limited requests allowed.
    HalfOpen,
}

/// Circuit breaker configuration.
#[derive(Debug, Clone)]
pub struct CircuitConfig {
    /// Number of failures before opening circuit.
    pub failure_threshold: u32,
    /// Number of successes in half-open before closing.
    pub success_threshold: u32,
    /// Time before attempting recovery (in time units).
    pub timeout: u64,
    /// Maximum calls allowed in half-open state.
    pub half_open_max_calls: u32,
}

impl Default for CircuitConfig {
    fn default() -> Self {
        Self {
            failure_threshold: 5,
            success_threshold: 2,
            timeout: 30,
            half_open_max_calls: 3,
        }
    }
}

/// Circuit breaker for fault tolerance.
#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    config: CircuitConfig,
    state: CircuitState,
    failures: u32,
    successes: u32,
    last_failure_time: u64,
    half_open_calls: u32,
}

impl CircuitBreaker {
    /// Create a new circuit breaker with the given configuration.
    pub fn new(config: CircuitConfig) -> Self {
        Self {
            config,
            state: CircuitState::Closed,
            failures: 0,
            successes: 0,
            last_failure_time: 0,
            half_open_calls: 0,
        }
    }

    /// Create with default configuration.
    pub fn default_config() -> Self {
        Self::new(CircuitConfig::default())
    }

    /// Get current state.
    pub fn state(&self) -> CircuitState {
        self.state
    }

    /// Update state based on current time.
    pub fn update_state(&mut self, current_time: u64) {
        if self.should_transition_to_half_open(current_time) {
            self.state = CircuitState::HalfOpen;
            self.successes = 0;
            self.half_open_calls = 0;
        }
    }

    fn should_transition_to_half_open(&self, current_time: u64) -> bool {
        self.state == CircuitState::Open
            && current_time >= self.last_failure_time.saturating_add(self.config.timeout)
    }

    /// Check if a request can be executed.
    pub fn can_execute(&mut self, current_time: u64) -> bool {
        self.update_state(current_time);

        match self.state {
            CircuitState::Closed => true,
            CircuitState::Open => false,
            CircuitState::HalfOpen => self.half_open_calls < self.config.half_open_max_calls,
        }
    }

    /// Record a successful call.
    pub fn record_success(&mut self) {
        match self.state {
            CircuitState::Closed => {
                self.failures = 0;
            }
            CircuitState::HalfOpen => {
                self.successes += 1;
                if self.successes >= self.config.success_threshold {
                    self.state = CircuitState::Closed;
                    self.failures = 0;
                    self.successes = 0;
                }
            }
            CircuitState::Open => {}
        }
    }

    /// Record a failed call.
    pub fn record_failure(&mut self, current_time: u64) {
        self.last_failure_time = current_time;

        match self.state {
            CircuitState::Closed => {
                self.failures += 1;
                if self.failures >= self.config.failure_threshold {
                    self.state = CircuitState::Open;
                }
            }
            CircuitState::HalfOpen => {
                self.state = CircuitState::Open;
                self.failures += 1;
            }
            CircuitState::Open => {
                self.failures += 1;
            }
        }
    }

    /// Record an attempt (for half-open tracking).
    pub fn record_attempt(&mut self) {
        if self.state == CircuitState::HalfOpen {
            self.half_open_calls += 1;
        }
    }

    /// Execute with circuit breaker logic.
    pub fn execute(&mut self, current_time: u64, success: bool) -> bool {
        self.update_state(current_time);

        if !self.can_execute(current_time) {
            return false;
        }

        self.record_attempt();

        if success {
            self.record_success();
        } else {
            self.record_failure(current_time);
        }

        true
    }

    /// Check if circuit is healthy.
    pub fn is_healthy(&self) -> bool {
        self.state == CircuitState::Closed
    }

    /// Time until circuit might close.
    pub fn time_until_retry(&self, current_time: u64) -> u64 {
        if self.state != CircuitState::Open {
            return 0;
        }
        let retry_time = self.last_failure_time.saturating_add(self.config.timeout);
        if current_time >= retry_time {
            0
        } else {
            retry_time - current_time
        }
    }

    /// Force reset.
    pub fn reset(&mut self) {
        self.state = CircuitState::Closed;
        self.failures = 0;
        self.successes = 0;
        self.half_open_calls = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circuit_breaker() {
        let mut cb = CircuitBreaker::new(CircuitConfig {
            failure_threshold: 3,
            ..Default::default()
        });

        assert!(cb.execute(0, false));
        assert!(cb.execute(1, false));
        assert_eq!(cb.state(), CircuitState::Closed);

        cb.execute(2, false);
        assert_eq!(cb.state(), CircuitState::Open);
        assert!(!cb.can_execute(3));
    }

    #[test]
    fn test_recovery() {
        let mut cb = CircuitBreaker::new(CircuitConfig {
            failure_threshold: 1,
            success_threshold: 1,
            timeout: 10,
            half_open_max_calls: 3,
        });

        cb.execute(0, false);
        assert_eq!(cb.state(), CircuitState::Open);

        // After timeout, should be half-open
        cb.update_state(15);
        assert_eq!(cb.state(), CircuitState::HalfOpen);

        // Success should close it
        cb.record_attempt();
        cb.record_success();
        assert_eq!(cb.state(), CircuitState::Closed);
    }
}
