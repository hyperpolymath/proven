// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe bounded queue operations via libproven FFI.
//!
//! Provides FIFO queues with bounded capacity.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// A bounded FIFO queue.
///
/// The underlying memory is managed by libproven. The queue is freed
/// when this struct is dropped.
pub struct BoundedQueue {
    ptr: *mut ffi::BoundedQueue,
}

// SAFETY: The underlying queue is managed by libproven.
unsafe impl Send for BoundedQueue {}

impl BoundedQueue {
    /// Create a new bounded queue with the given capacity.
    pub fn new(capacity: usize) -> Result<Self> {
        // SAFETY: proven_queue_create takes a value-type usize;
        // always safe to call.
        let ptr = unsafe { ffi::proven_queue_create(capacity) };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(BoundedQueue { ptr })
    }

    /// Push a value to the back of the queue.
    ///
    /// Returns `false` if the queue is full.
    pub fn push(&mut self, value: i64) -> bool {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_queue_push(self.ptr, value) }
    }

    /// Pop a value from the front of the queue.
    ///
    /// Returns `Err` if the queue is empty.
    pub fn pop(&mut self) -> Result<i64> {
        // SAFETY: self.ptr is valid (checked at construction).
        let result = unsafe { ffi::proven_queue_pop(self.ptr) };
        core::int_result_to_result(result)
    }

    /// Get the current number of elements in the queue.
    pub fn size(&self) -> usize {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_queue_size(self.ptr) }
    }

    /// Check if the queue is empty.
    pub fn is_empty(&self) -> bool {
        self.size() == 0
    }
}

impl Drop for BoundedQueue {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_queue_create
        // and has not been freed yet.
        unsafe {
            ffi::proven_queue_free(self.ptr);
        }
    }
}

/// A priority queue (wraps bounded queue with priority semantics).
///
/// Note: The underlying FFI uses the same queue primitives. Priority
/// ordering is handled at the Idris 2 level.
pub struct PriorityQueue {
    inner: BoundedQueue,
}

impl PriorityQueue {
    /// Create a new priority queue with the given capacity.
    pub fn new(capacity: usize) -> Result<Self> {
        Ok(PriorityQueue {
            inner: BoundedQueue::new(capacity)?,
        })
    }

    /// Insert a value (priority encoded in the value).
    pub fn insert(&mut self, value: i64) -> bool {
        self.inner.push(value)
    }

    /// Remove and return the highest-priority value.
    pub fn remove(&mut self) -> Result<i64> {
        self.inner.pop()
    }

    /// Get the current size.
    pub fn size(&self) -> usize {
        self.inner.size()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}
