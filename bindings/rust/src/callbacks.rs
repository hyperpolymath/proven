// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Callback infrastructure for bidirectional FFI via libproven.
//!
//! Enables host Rust code to register callback functions that can be
//! invoked by the Idris 2 verified code through the Zig FFI layer.
//!
//! Thread-safe: all callback operations in libproven are mutex-protected.

use std::os::raw::c_void;

use crate::core::{self, Error, Result};
use crate::ffi;

/// Re-export event types and handles from FFI.
pub use crate::ffi::{CallbackHandle, ProvenCallbackFn, ProvenEvent, ProvenEventType};

/// Invalid callback handle constant.
pub const INVALID_HANDLE: CallbackHandle = 0;

/// Register a callback for a specific event type.
///
/// - `event_type`: the type of event to listen for.
/// - `callback`: the callback function to invoke.
/// - `context`: opaque context pointer passed to the callback.
///
/// Returns a non-zero handle on success, or `Err` on failure.
///
/// # Safety
///
/// The `context` pointer must remain valid for the lifetime of the callback
/// registration. The caller is responsible for ensuring this.
pub unsafe fn register(
    event_type: ProvenEventType,
    callback: ProvenCallbackFn,
    context: *mut c_void,
) -> Result<CallbackHandle> {
    // SAFETY: Caller guarantees context pointer validity.
    let handle = ffi::proven_callback_register(event_type, callback, context);
    if handle == INVALID_HANDLE {
        return Err(Error::AllocationFailed);
    }
    Ok(handle)
}

/// Unregister a callback by its handle.
///
/// Returns `Ok(())` on success, or `Err` if the handle is invalid.
pub fn unregister(handle: CallbackHandle) -> Result<()> {
    // SAFETY: proven_callback_unregister takes a value-type handle;
    // always safe to call.
    let status = unsafe { ffi::proven_callback_unregister(handle) };
    core::status_to_result(status)
}

/// Fire an event, invoking all registered callbacks for that event type.
///
/// Returns the number of callbacks invoked.
pub fn fire(event_type: ProvenEventType, data: Option<&[u8]>, code: i32) -> i32 {
    let (data_ptr, data_len) = match data {
        Some(d) => (d.as_ptr(), d.len()),
        None => (std::ptr::null(), 0),
    };
    // SAFETY: We pass a valid pointer and length (or null/0).
    unsafe { ffi::proven_callback_fire(event_type, data_ptr, data_len, code) }
}

/// Query how many callbacks are registered for an event type.
pub fn count(event_type: ProvenEventType) -> u32 {
    // SAFETY: proven_callback_count takes a value-type enum;
    // always safe to call.
    unsafe { ffi::proven_callback_count(event_type) }
}

/// Unregister all callbacks.
///
/// Returns the number of callbacks removed.
pub fn clear_all() -> u32 {
    // SAFETY: proven_callback_clear_all takes no arguments;
    // always safe to call.
    unsafe { ffi::proven_callback_clear_all() }
}
