// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Runtime lifecycle management for libproven.
//!
//! The Proven runtime (including the Idris 2 runtime) must be initialized
//! before use and deinitialized on shutdown. This module provides safe
//! wrappers for lifecycle management.

use crate::core::{self, Result};
use crate::ffi;

/// Initialize the Proven runtime.
///
/// Must be called before using any other Proven functions.
/// Safe to call multiple times (subsequent calls are no-ops).
pub fn init() -> Result<()> {
    // SAFETY: proven_init takes no arguments; always safe to call.
    // It is idempotent (safe to call multiple times).
    let status = unsafe { ffi::proven_init() };
    core::status_to_result(status)
}

/// Deinitialize the Proven runtime.
///
/// Releases all resources held by the Idris 2 runtime.
/// Safe to call multiple times (subsequent calls are no-ops).
pub fn deinit() {
    // SAFETY: proven_deinit takes no arguments; always safe to call.
    // It is idempotent.
    unsafe { ffi::proven_deinit() }
}

/// Check if the runtime has been initialized.
pub fn is_initialized() -> bool {
    // SAFETY: proven_is_initialized takes no arguments; always safe to call.
    unsafe { ffi::proven_is_initialized() }
}
