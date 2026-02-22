// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeUuid - UUID operations that cannot crash.
////
//// Thin FFI wrapper over libproven uuid_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// UUID version types per RFC 4122.
pub type UuidVersion {
  VersionNil
  Version1
  Version2
  Version3
  Version4
  Version5
}

/// UUID variant types per RFC 4122.
pub type UuidVariant {
  VariantNcs
  VariantRfc4122
  VariantMicrosoft
  VariantFuture
}

/// Generate a v4 (random) UUID using system CSPRNG.
/// Returns the 16-byte UUID as a BitArray.
@external(erlang, "proven_nif", "uuid_v4_generate")
pub fn v4_generate() -> Result(BitArray, String)

/// Parse a UUID from canonical string format.
/// Returns the 16-byte UUID as a BitArray.
@external(erlang, "proven_nif", "uuid_parse")
pub fn parse(input: String) -> Result(BitArray, String)

/// Format UUID bytes as canonical string (lowercase).
/// Input must be exactly 16 bytes.
@external(erlang, "proven_nif", "uuid_to_string")
pub fn to_string(bytes: BitArray) -> Result(String, String)

/// Format UUID bytes as URN (urn:uuid:...).
/// Input must be exactly 16 bytes.
@external(erlang, "proven_nif", "uuid_to_urn")
pub fn to_urn(bytes: BitArray) -> Result(String, String)

/// Check if UUID bytes are nil (all zeros).
/// Input must be exactly 16 bytes.
@external(erlang, "proven_nif", "uuid_is_nil")
pub fn is_nil(bytes: BitArray) -> Bool

/// Check if a string is valid UUID format.
@external(erlang, "proven_nif", "uuid_is_valid")
pub fn is_valid(input: String) -> Bool

/// Get the UUID version from bytes.
@external(erlang, "proven_nif", "uuid_get_version")
pub fn get_version(bytes: BitArray) -> Result(UuidVersion, String)

/// Get the UUID variant from bytes.
@external(erlang, "proven_nif", "uuid_get_variant")
pub fn get_variant(bytes: BitArray) -> Result(UuidVariant, String)

/// Check if two UUIDs are equal.
@external(erlang, "proven_nif", "uuid_equals")
pub fn equals(a: BitArray, b: BitArray) -> Bool
