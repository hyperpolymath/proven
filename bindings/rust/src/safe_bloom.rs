// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe Bloom filter via libproven FFI.
//!
//! Provides probabilistic set membership testing.
//! All operations delegate to Idris 2 verified code.

use crate::core::{Error, Result};
use crate::ffi;

/// A Bloom filter for probabilistic set membership.
///
/// A Bloom filter can tell you definitively that an element is NOT in the
/// set, or that it MIGHT be in the set (with a tunable false positive rate).
///
/// The underlying memory is managed by libproven. The filter is freed
/// when this struct is dropped.
pub struct BloomFilter {
    ptr: *mut ffi::BloomFilter,
}

// SAFETY: The underlying bloom filter is managed by libproven.
unsafe impl Send for BloomFilter {}

impl BloomFilter {
    /// Create a new Bloom filter.
    ///
    /// - `expected_elements`: expected number of elements to insert.
    /// - `false_positive_rate`: desired false positive rate (e.g., 0.01 for 1%).
    pub fn new(expected_elements: usize, false_positive_rate: f64) -> Result<Self> {
        // SAFETY: proven_bloom_create takes value-type arguments;
        // always safe to call.
        let ptr = unsafe {
            ffi::proven_bloom_create(expected_elements, false_positive_rate)
        };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(BloomFilter { ptr })
    }

    /// Add an element to the filter.
    pub fn add(&mut self, data: &[u8]) {
        // SAFETY: self.ptr is valid (checked at construction), and we pass
        // a valid pointer and length from a Rust slice.
        unsafe {
            ffi::proven_bloom_add(self.ptr, data.as_ptr(), data.len());
        }
    }

    /// Add a string element to the filter.
    pub fn add_str(&mut self, s: &str) {
        self.add(s.as_bytes());
    }

    /// Check if an element might be in the filter.
    ///
    /// Returns `true` if the element might be present (could be a false
    /// positive), `false` if it is definitely not present.
    pub fn contains(&self, data: &[u8]) -> bool {
        // SAFETY: self.ptr is valid (checked at construction), and we pass
        // a valid pointer and length from a Rust slice.
        unsafe {
            ffi::proven_bloom_contains(self.ptr, data.as_ptr(), data.len())
        }
    }

    /// Check if a string element might be in the filter.
    pub fn contains_str(&self, s: &str) -> bool {
        self.contains(s.as_bytes())
    }
}

impl Drop for BloomFilter {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_bloom_create
        // and has not been freed yet.
        unsafe {
            ffi::proven_bloom_free(self.ptr);
        }
    }
}
