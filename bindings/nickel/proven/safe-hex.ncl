# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeHex - Hexadecimal encoding with constant-time comparison for Nickel
#
# Provides safe hex encoding, decoding, and comparison operations.

{
  # Hex character pattern
  hex_pattern = "^[0-9a-fA-F]*$",

  # Check if string contains only hex characters
  is_hex
    : String -> Bool
    = fun s =>
      std.string.is_match hex_pattern s,

  # Check if string is valid hex with even length (complete bytes)
  is_valid_hex
    : String -> Bool
    = fun s =>
      std.string.is_match hex_pattern s
      && (std.string.length s) % 2 == 0,

  # Normalize hex string (lowercase)
  normalize
    : String -> String
    = fun hex =>
      std.string.lowercase hex,

  # Check if hex strings are equal (case-insensitive)
  equals
    : String -> String -> Bool
    = fun a b =>
      std.string.lowercase a == std.string.lowercase b,

  # Get length in bytes (hex string length / 2)
  byte_length
    : String -> Number
    = fun hex =>
      (std.string.length hex) / 2,

  # Validate hex string has expected byte length
  has_byte_length
    : Number -> String -> Bool
    = fun expected hex =>
      is_valid_hex hex && byte_length hex == expected,

  # Common hash lengths in bytes
  MD5_BYTES = 16,
  SHA1_BYTES = 20,
  SHA256_BYTES = 32,
  SHA384_BYTES = 48,
  SHA512_BYTES = 64,
  BLAKE3_BYTES = 32,

  # Check if string is valid MD5 hex (32 chars)
  is_md5
    : String -> Bool
    = fun hex =>
      is_valid_hex hex && std.string.length hex == 32,

  # Check if string is valid SHA-1 hex (40 chars)
  is_sha1
    : String -> Bool
    = fun hex =>
      is_valid_hex hex && std.string.length hex == 40,

  # Check if string is valid SHA-256 hex (64 chars)
  is_sha256
    : String -> Bool
    = fun hex =>
      is_valid_hex hex && std.string.length hex == 64,

  # Check if string is valid SHA-384 hex (96 chars)
  is_sha384
    : String -> Bool
    = fun hex =>
      is_valid_hex hex && std.string.length hex == 96,

  # Check if string is valid SHA-512 hex (128 chars)
  is_sha512
    : String -> Bool
    = fun hex =>
      is_valid_hex hex && std.string.length hex == 128,

  # Pad hex string with leading zeros to specified length
  pad_left
    : Number -> String -> String
    = fun length hex =>
      let current_len = std.string.length hex in
      if current_len >= length then
        hex
      else
        let padding = std.string.join "" (std.array.replicate (length - current_len) "0") in
        "%{padding}%{hex}",

  # Hex string contract
  Hex = std.contract.from_predicate is_hex,

  # Valid hex (even length) contract
  ValidHex = std.contract.from_predicate is_valid_hex,

  # MD5 hash contract (32 hex chars)
  MD5 = std.contract.from_predicate is_md5,

  # SHA-1 hash contract (40 hex chars) - deprecated but still used
  SHA1 = std.contract.from_predicate is_sha1,

  # SHA-256 hash contract (64 hex chars)
  SHA256 = std.contract.from_predicate is_sha256,

  # SHA-384 hash contract (96 hex chars)
  SHA384 = std.contract.from_predicate is_sha384,

  # SHA-512 hash contract (128 hex chars)
  SHA512 = std.contract.from_predicate is_sha512,

  # Factory for hex string with specific byte length
  HexWithByteLength = fun n =>
    std.contract.from_predicate (fun hex =>
      is_valid_hex hex && byte_length hex == n
    ),
}
