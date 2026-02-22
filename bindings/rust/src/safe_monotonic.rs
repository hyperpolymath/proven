// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe monotonically increasing sequences via libproven FFI.
//!
//! Provides counters that only move forward, useful for versioning,
//! timestamps, and sequence numbers.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// Monotonic counter that can only increase.
///
/// The underlying state is managed by libproven. The counter is freed
/// when this struct is dropped.
pub struct MonotonicCounter {
    ptr: *mut ffi::MonotonicCounter,
}

// SAFETY: The underlying counter is managed by libproven.
unsafe impl Send for MonotonicCounter {}

impl MonotonicCounter {
    /// Create a new monotonic counter.
    ///
    /// - `initial`: starting value.
    /// - `max_value`: maximum value before wrapping is denied.
    pub fn new(initial: u64, max_value: u64) -> Result<Self> {
        // SAFETY: proven_monotonic_create takes value-type arguments;
        // always safe to call.
        let ptr = unsafe { ffi::proven_monotonic_create(initial, max_value) };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(MonotonicCounter { ptr })
    }

    /// Get the next value, incrementing the counter.
    ///
    /// Returns `Err(Error::Overflow)` if the counter has reached its maximum.
    pub fn next(&mut self) -> Result<i64> {
        // SAFETY: self.ptr is valid (checked at construction).
        let result = unsafe { ffi::proven_monotonic_next(self.ptr) };
        core::int_result_to_result(result)
    }
}

impl Drop for MonotonicCounter {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_monotonic_create
        // and has not been freed yet.
        unsafe {
            ffi::proven_monotonic_free(self.ptr);
        }
    }
}

/// High water mark tracker.
///
/// Wraps a monotonic counter to track the highest value seen.
pub struct HighWaterMark {
    inner: MonotonicCounter,
}

impl HighWaterMark {
    /// Create a new high water mark tracker.
    pub fn new(max_value: u64) -> Result<Self> {
        Ok(HighWaterMark {
            inner: MonotonicCounter::new(0, max_value)?,
        })
    }

    /// Record a new value, advancing if higher than current.
    pub fn record(&mut self) -> Result<i64> {
        self.inner.next()
    }
}

/// Epoch-based sequence generator.
///
/// Wraps a monotonic counter starting from a given epoch.
pub struct EpochGenerator {
    inner: MonotonicCounter,
}

impl EpochGenerator {
    /// Create a new epoch-based generator.
    pub fn new(epoch: u64) -> Result<Self> {
        Ok(EpochGenerator {
            inner: MonotonicCounter::new(epoch, u64::MAX)?,
        })
    }

    /// Generate the next value.
    pub fn next(&mut self) -> Result<i64> {
        self.inner.next()
    }
}

/// Sequence generator (alias for MonotonicCounter).
pub struct SequenceGenerator {
    inner: MonotonicCounter,
}

impl SequenceGenerator {
    /// Create a new sequence generator starting at 0.
    pub fn new() -> Result<Self> {
        Ok(SequenceGenerator {
            inner: MonotonicCounter::new(0, u64::MAX)?,
        })
    }

    /// Generate the next sequence number.
    pub fn next(&mut self) -> Result<i64> {
        self.inner.next()
    }
}
