// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe phone number operations via libproven FFI.
//!
//! Parses and formats E.164 phone numbers.
//! All operations delegate to Idris 2 verified code.

use crate::core::{self, Result};
use crate::ffi;

/// Parsed phone number.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhoneNumber {
    /// Country calling code (e.g., 1 for US, 44 for UK).
    pub country_code: u16,
    /// National number (without country code).
    pub national_number: u64,
    /// Whether the phone number passed validation.
    pub is_valid: bool,
}

/// Safe phone number operations.
pub struct SafePhone;

impl SafePhone {
    /// Parse a phone number string.
    ///
    /// Accepts various formats including E.164 (`+1234567890`).
    pub fn parse(input: &str) -> Result<PhoneNumber> {
        let bytes = input.as_bytes();
        // SAFETY: We pass a valid pointer and length from the string's bytes.
        // The FFI function only reads from the pointer and returns a
        // value-type struct.
        let result = unsafe {
            ffi::proven_phone_parse(bytes.as_ptr(), bytes.len())
        };
        core::status_to_result(result.status)?;
        Ok(PhoneNumber {
            country_code: result.country_code,
            national_number: result.national_number,
            is_valid: result.is_valid,
        })
    }

    /// Format a phone number as E.164 (e.g., "+12025551234").
    pub fn format_e164(country_code: u16, national_number: u64) -> Result<String> {
        // SAFETY: proven_phone_format_e164 takes value-type arguments;
        // always safe to call.
        let result = unsafe {
            ffi::proven_phone_format_e164(country_code, national_number)
        };
        core::string_result_to_result(result)
    }
}
