// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeHex - Hexadecimal encoding and decoding that cannot crash.
////
//// Thin FFI wrapper over libproven hex_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Encode bytes to lowercase hex string.
@external(erlang, "proven_nif", "hex_encode")
pub fn encode(bytes: BitArray) -> Result(String, String)

/// Encode bytes to uppercase hex string.
@external(erlang, "proven_nif", "hex_encode_upper")
pub fn encode_upper(bytes: BitArray) -> Result(String, String)

/// Decode a hex string to bytes.
@external(erlang, "proven_nif", "hex_decode")
pub fn decode(hex: String) -> Result(BitArray, String)

/// Check if a string contains only valid hex characters.
@external(erlang, "proven_nif", "hex_is_valid")
pub fn is_valid(input: String) -> Bool

/// Check if a string is valid hex bytes (even length, valid chars).
@external(erlang, "proven_nif", "hex_is_valid_bytes")
pub fn is_valid_bytes(input: String) -> Bool

/// Constant-time hex string comparison (timing-safe, case-insensitive).
/// Use for comparing security-sensitive values like API keys and hashes.
@external(erlang, "proven_nif", "hex_constant_time_eq")
pub fn constant_time_eq(a: String, b: String) -> Bool

/// Parse a hex string to an integer.
@external(erlang, "proven_nif", "hex_to_int")
pub fn to_int(hex: String) -> Result(Int, String)

/// Convert an integer to a hex string with minimum width (zero-padded).
@external(erlang, "proven_nif", "hex_from_int")
pub fn from_int(value: Int, min_width: Int) -> Result(String, String)
