// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Library version information from libproven.
//!
//! Queries the compiled libproven for version and module count information.

use crate::ffi;

/// Get the major version number of libproven.
pub fn major() -> u32 {
    // SAFETY: proven_version_major takes no arguments; always safe to call.
    unsafe { ffi::proven_version_major() }
}

/// Get the minor version number of libproven.
pub fn minor() -> u32 {
    // SAFETY: proven_version_minor takes no arguments; always safe to call.
    unsafe { ffi::proven_version_minor() }
}

/// Get the patch version number of libproven.
pub fn patch() -> u32 {
    // SAFETY: proven_version_patch takes no arguments; always safe to call.
    unsafe { ffi::proven_version_patch() }
}

/// Get the number of modules in libproven.
pub fn module_count() -> u32 {
    // SAFETY: proven_module_count takes no arguments; always safe to call.
    unsafe { ffi::proven_module_count() }
}

/// Get the FFI ABI version for compatibility checking.
pub fn ffi_abi_version() -> u32 {
    // SAFETY: proven_ffi_abi_version takes no arguments; always safe to call.
    unsafe { ffi::proven_ffi_abi_version() }
}

/// Get the version as a formatted string (e.g., "1.0.0").
pub fn version_string() -> String {
    format!("{}.{}.{}", major(), minor(), patch())
}
