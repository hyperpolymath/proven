// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe circuit breaker pattern via libproven FFI.
//!
//! Implements the circuit breaker fault tolerance pattern with configurable
//! thresholds. All operations delegate to Idris 2 verified code.

use crate::core::{Error, Result};
use crate::ffi;

/// Circuit breaker state.
pub use crate::ffi::CircuitState;

/// Circuit breaker configuration.
#[derive(Debug, Clone)]
pub struct CircuitConfig {
    /// Number of failures before opening the circuit.
    pub failure_threshold: u32,
    /// Number of successes in half-open state before closing.
    pub success_threshold: u32,
    /// Time in milliseconds before retrying after circuit opens.
    pub timeout_ms: i64,
}

impl Default for CircuitConfig {
    fn default() -> Self {
        CircuitConfig {
            failure_threshold: 5,
            success_threshold: 2,
            timeout_ms: 60_000,
        }
    }
}

/// Circuit breaker for fault tolerance.
///
/// States:
/// - **Closed**: Normal operation, requests pass through.
/// - **Open**: Failing, requests are rejected.
/// - **HalfOpen**: Testing recovery, limited requests pass through.
///
/// The underlying state is managed by libproven. The circuit breaker is
/// freed when this struct is dropped.
pub struct CircuitBreaker {
    ptr: *mut ffi::CircuitBreaker,
}

// SAFETY: The underlying circuit breaker is managed by libproven.
unsafe impl Send for CircuitBreaker {}

impl CircuitBreaker {
    /// Create a new circuit breaker with the given configuration.
    pub fn new(config: &CircuitConfig) -> Result<Self> {
        // SAFETY: proven_circuit_breaker_create takes value-type arguments;
        // always safe to call.
        let ptr = unsafe {
            ffi::proven_circuit_breaker_create(
                config.failure_threshold,
                config.success_threshold,
                config.timeout_ms,
            )
        };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(CircuitBreaker { ptr })
    }

    /// Check if a request should be allowed through.
    ///
    /// Returns `true` if the circuit is closed or half-open.
    pub fn allow(&mut self) -> bool {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_circuit_breaker_allow(self.ptr) }
    }

    /// Record a successful operation.
    ///
    /// In half-open state, enough successes will close the circuit.
    pub fn record_success(&mut self) {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_circuit_breaker_success(self.ptr) }
    }

    /// Record a failed operation.
    ///
    /// Enough failures will open the circuit.
    pub fn record_failure(&mut self) {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_circuit_breaker_failure(self.ptr) }
    }

    /// Get the current circuit state.
    pub fn state(&self) -> CircuitState {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_circuit_breaker_state(self.ptr) }
    }
}

impl Drop for CircuitBreaker {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_circuit_breaker_create
        // and has not been freed yet.
        unsafe {
            ffi::proven_circuit_breaker_free(self.ptr);
        }
    }
}
