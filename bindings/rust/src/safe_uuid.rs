// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe UUID operations via libproven FFI.
//!
//! Generates and parses RFC 4122 UUIDs.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// UUID (128-bit universally unique identifier).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Uuid {
    /// Raw 16 bytes of the UUID.
    pub bytes: [u8; 16],
}

impl std::fmt::Display for Uuid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Format as standard UUID string (8-4-4-4-12)
        match SafeUuid::to_string(self) {
            Ok(s) => write!(f, "{}", s),
            Err(_) => write!(f, "<invalid-uuid>"),
        }
    }
}

/// Safe UUID operations.
pub struct SafeUuid;

impl SafeUuid {
    /// Generate a new UUID v4 (random).
    pub fn v4() -> Result<Uuid> {
        // SAFETY: proven_uuid_v4 takes no arguments and allocates no external
        // memory; always safe to call.
        let result = unsafe { ffi::proven_uuid_v4() };
        core::status_to_result(result.status)?;
        Ok(Uuid {
            bytes: result.uuid.bytes,
        })
    }

    /// Format a UUID as a string (36 chars with hyphens).
    pub fn to_string(uuid: &Uuid) -> Result<String> {
        let ffi_uuid = ffi::UUID { bytes: uuid.bytes };
        // SAFETY: proven_uuid_to_string takes a value-type UUID struct;
        // always safe to call. Returns a StringResult that we free.
        let result = unsafe { ffi::proven_uuid_to_string(ffi_uuid) };
        core::string_result_to_result(result)
    }

    /// Parse a UUID from a string.
    ///
    /// Accepts the standard 8-4-4-4-12 format with or without hyphens.
    pub fn parse(input: &str) -> Result<Uuid> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        let result = unsafe {
            ffi::proven_uuid_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;
        Ok(Uuid {
            bytes: result.uuid.bytes,
        })
    }

    /// Check if a UUID is nil (all zeros).
    pub fn is_nil(uuid: &Uuid) -> bool {
        let ffi_uuid = ffi::UUID { bytes: uuid.bytes };
        // SAFETY: proven_uuid_is_nil takes a value-type UUID struct;
        // always safe to call.
        unsafe { ffi::proven_uuid_is_nil(ffi_uuid) }
    }

    /// Get the UUID version number.
    pub fn version(uuid: &Uuid) -> u8 {
        let ffi_uuid = ffi::UUID { bytes: uuid.bytes };
        // SAFETY: proven_uuid_version takes a value-type UUID struct;
        // always safe to call.
        unsafe { ffi::proven_uuid_version(ffi_uuid) }
    }
}
