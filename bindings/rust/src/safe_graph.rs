// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe directed graph via libproven FFI.
//!
//! Provides a directed graph with edge operations.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Error, Result};
use crate::ffi;

/// A directed graph with a fixed number of nodes.
///
/// The underlying memory is managed by libproven. The graph is freed
/// when this struct is dropped.
pub struct Graph {
    ptr: *mut ffi::Graph,
}

// SAFETY: The underlying graph is managed by libproven.
unsafe impl Send for Graph {}

impl Graph {
    /// Create a new directed graph with the given number of nodes.
    ///
    /// Nodes are indexed from 0 to `node_count - 1`.
    pub fn new(node_count: usize) -> Result<Self> {
        // SAFETY: proven_graph_create takes a value-type usize;
        // always safe to call.
        let ptr = unsafe { ffi::proven_graph_create(node_count) };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(Graph { ptr })
    }

    /// Add a directed edge from node `from` to node `to`.
    ///
    /// Returns `Err(Error::OutOfBounds)` if either node index is invalid.
    pub fn add_edge(&mut self, from: usize, to: usize) -> Result<()> {
        // SAFETY: self.ptr is valid (checked at construction).
        let status = unsafe {
            ffi::proven_graph_add_edge(self.ptr, from, to)
        };
        core::status_to_result(status)
    }

    /// Check if a directed edge exists from node `from` to node `to`.
    pub fn has_edge(&self, from: usize, to: usize) -> bool {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_graph_has_edge(self.ptr, from, to) }
    }
}

impl Drop for Graph {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_graph_create
        // and has not been freed yet.
        unsafe {
            ffi::proven_graph_free(self.ptr);
        }
    }
}
