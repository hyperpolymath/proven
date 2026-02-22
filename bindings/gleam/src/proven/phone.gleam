// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafePhone - Phone number validation that cannot crash.
////
//// Thin FFI wrapper over libproven phone_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// A parsed phone number.
pub type PhoneNumber {
  PhoneNumber(country_code: Int, national_number: String)
}

/// Parse a phone number from string.
/// Accepts formats like: +1 555 123 4567, 15551234567, +1-555-123-4567
@external(erlang, "proven_nif", "phone_parse")
pub fn parse(input: String) -> Result(PhoneNumber, String)

/// Check if a string is a valid phone number.
@external(erlang, "proven_nif", "phone_is_valid")
pub fn is_valid(input: String) -> Bool

/// Format phone number in E.164 format (e.g., "+15551234567").
@external(erlang, "proven_nif", "phone_format_e164")
pub fn format_e164(country_code: Int, national_number: String) -> Result(String, String)

/// Format phone number in international format (e.g., "+1 555 123 4567").
@external(erlang, "proven_nif", "phone_format_international")
pub fn format_international(
  country_code: Int,
  national_number: String,
) -> Result(String, String)
