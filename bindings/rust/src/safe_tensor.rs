// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe tensor operations via libproven FFI.
//!
//! Provides bounds-checked 2D tensor (matrix) operations.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// A 2D tensor (matrix) with bounds-checked access.
///
/// The underlying memory is managed by libproven. The tensor is freed
/// when this struct is dropped.
pub struct SafeTensor {
    ptr: *mut ffi::Tensor2D,
    rows: usize,
    cols: usize,
}

// SAFETY: The underlying tensor is managed by libproven.
unsafe impl Send for SafeTensor {}

impl SafeTensor {
    /// Create a new 2D tensor initialized to zero.
    ///
    /// - `rows`: number of rows.
    /// - `cols`: number of columns.
    pub fn new(rows: usize, cols: usize) -> Result<Self> {
        // SAFETY: proven_tensor_create takes value-type arguments;
        // always safe to call.
        let ptr = unsafe { ffi::proven_tensor_create(rows, cols) };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(SafeTensor { ptr, rows, cols })
    }

    /// Get the number of rows.
    pub fn rows(&self) -> usize {
        self.rows
    }

    /// Get the number of columns.
    pub fn cols(&self) -> usize {
        self.cols
    }

    /// Set a value at (row, col).
    ///
    /// Returns `Err(Error::OutOfBounds)` if indices are out of range.
    pub fn set(&mut self, row: usize, col: usize, value: f64) -> Result<()> {
        // SAFETY: self.ptr is valid (checked at construction).
        let status = unsafe {
            ffi::proven_tensor_set(self.ptr, row, col, value)
        };
        core::status_to_result(status)
    }

    /// Get the value at (row, col).
    ///
    /// Returns `Err(Error::OutOfBounds)` if indices are out of range.
    pub fn get(&self, row: usize, col: usize) -> Result<f64> {
        // SAFETY: self.ptr is valid (checked at construction).
        let result = unsafe {
            ffi::proven_tensor_get(self.ptr, row, col)
        };
        core::float_result_to_result(result)
    }

    /// Matrix multiplication: self * other.
    ///
    /// Returns `Err` if dimensions are incompatible (self.cols != other.rows).
    pub fn matmul(&self, other: &SafeTensor) -> Result<SafeTensor> {
        // SAFETY: Both pointers are valid (checked at construction).
        // proven_tensor_matmul reads from both and allocates a new tensor.
        let ptr = unsafe {
            ffi::proven_tensor_matmul(self.ptr, other.ptr)
        };
        if ptr.is_null() {
            return Err(Error::InvalidArgument(
                "Incompatible tensor dimensions for matmul".to_string(),
            ));
        }
        Ok(SafeTensor {
            ptr,
            rows: self.rows,
            cols: other.cols,
        })
    }
}

impl Drop for SafeTensor {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_tensor_create or
        // proven_tensor_matmul and has not been freed yet.
        unsafe {
            ffi::proven_tensor_free(self.ptr);
        }
    }
}
