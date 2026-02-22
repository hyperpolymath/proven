// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe bounded buffer operations via libproven FFI.
//!
//! Provides bounds-checked buffers that prevent overflow.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// A bounded buffer with a fixed capacity.
///
/// The underlying memory is managed by libproven. The buffer is freed
/// when this struct is dropped.
pub struct BoundedBuffer {
    ptr: *mut ffi::BoundedBuffer,
}

// SAFETY: The underlying buffer is managed by libproven with proper
// synchronization. The pointer is only accessed through our safe API.
unsafe impl Send for BoundedBuffer {}

impl BoundedBuffer {
    /// Create a new bounded buffer with the given capacity.
    ///
    /// Returns `Err(Error::AllocationFailed)` if the allocation fails.
    pub fn new(capacity: usize) -> Result<Self> {
        // SAFETY: proven_buffer_create takes a value-type usize;
        // always safe to call.
        let result = unsafe { ffi::proven_buffer_create(capacity) };
        core::status_to_result(result.status)?;

        if result.buffer.is_null() {
            return Err(Error::AllocationFailed);
        }

        Ok(BoundedBuffer {
            ptr: result.buffer,
        })
    }

    /// Append data to the buffer.
    ///
    /// Returns `Err(Error::OutOfBounds)` if appending would exceed capacity.
    pub fn append(&mut self, data: &[u8]) -> Result<()> {
        // SAFETY: self.ptr is valid (checked at construction), and we pass
        // a valid pointer and length from a Rust slice.
        let status = unsafe {
            ffi::proven_buffer_append(self.ptr, data.as_ptr(), data.len())
        };
        core::status_to_result(status)
    }

    /// Get the current buffer contents.
    ///
    /// Returns a copy of the buffered data as a `Vec<u8>`.
    pub fn get(&self) -> Result<Vec<u8>> {
        let mut out_ptr: *const u8 = std::ptr::null();
        let mut out_len: usize = 0;

        // SAFETY: self.ptr is valid, and we pass valid mutable pointers
        // for the output parameters.
        let status = unsafe {
            ffi::proven_buffer_get(
                self.ptr,
                &mut out_ptr as *mut *const u8,
                &mut out_len,
            )
        };
        core::status_to_result(status)?;

        if out_ptr.is_null() || out_len == 0 {
            return Ok(Vec::new());
        }

        // SAFETY: The FFI call succeeded, out_ptr points to valid data
        // of length out_len within the buffer. We copy the data.
        let data = unsafe {
            std::slice::from_raw_parts(out_ptr, out_len).to_vec()
        };

        Ok(data)
    }
}

impl Drop for BoundedBuffer {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_buffer_create and has
        // not been freed yet (we only free in Drop).
        unsafe {
            ffi::proven_buffer_free(self.ptr);
        }
    }
}

/// A ring buffer (circular buffer).
///
/// Wraps the same bounded buffer FFI but provides ring-buffer semantics
/// at the Zig/Idris level.
pub struct RingBuffer {
    inner: BoundedBuffer,
}

impl RingBuffer {
    /// Create a new ring buffer with the given capacity.
    pub fn new(capacity: usize) -> Result<Self> {
        Ok(RingBuffer {
            inner: BoundedBuffer::new(capacity)?,
        })
    }

    /// Write data to the ring buffer.
    pub fn write(&mut self, data: &[u8]) -> Result<()> {
        self.inner.append(data)
    }

    /// Read current buffer contents.
    pub fn read(&self) -> Result<Vec<u8>> {
        self.inner.get()
    }
}
