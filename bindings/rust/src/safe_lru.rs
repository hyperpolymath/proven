// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe LRU cache via libproven FFI.
//!
//! Provides a least-recently-used eviction cache with bounded capacity.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// Least-recently-used cache with bounded capacity.
///
/// Keys are u64, values are i64. The underlying memory is managed by
/// libproven. The cache is freed when this struct is dropped.
pub struct LruCache {
    ptr: *mut ffi::LRUCache,
}

// SAFETY: The underlying LRU cache is managed by libproven.
unsafe impl Send for LruCache {}

impl LruCache {
    /// Create a new LRU cache with the given capacity.
    ///
    /// When the cache is full, the least-recently-used entry is evicted.
    pub fn new(capacity: usize) -> Result<Self> {
        // SAFETY: proven_lru_create takes a value-type usize;
        // always safe to call.
        let ptr = unsafe { ffi::proven_lru_create(capacity) };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(LruCache { ptr })
    }

    /// Get a value from the cache by key.
    ///
    /// Returns `Ok(value)` if found, `Err` if not found.
    /// Accessing a key marks it as recently used.
    pub fn get(&mut self, key: u64) -> Result<i64> {
        // SAFETY: self.ptr is valid (checked at construction).
        let result = unsafe { ffi::proven_lru_get(self.ptr, key) };
        core::int_result_to_result(result)
    }

    /// Put a key-value pair into the cache.
    ///
    /// If the cache is full, the least-recently-used entry is evicted.
    pub fn put(&mut self, key: u64, value: i64) -> Result<()> {
        // SAFETY: self.ptr is valid (checked at construction).
        let status = unsafe { ffi::proven_lru_put(self.ptr, key, value) };
        core::status_to_result(status)
    }
}

impl Drop for LruCache {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_lru_create
        // and has not been freed yet.
        unsafe {
            ffi::proven_lru_free(self.ptr);
        }
    }
}
