# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeCrypto - Cryptographic validation contracts for Nickel
#
# Provides validation and contracts for cryptographic values.
# Note: Nickel is a configuration language, so actual crypto
# operations would be performed by the target runtime.

{
  # Result type for crypto operations
  Result = {
    value | String,
    ok | Bool,
  },

  # Hash length constants (in hex characters)
  MD5_HEX_LENGTH = 32,
  SHA1_HEX_LENGTH = 40,
  SHA256_HEX_LENGTH = 64,
  SHA384_HEX_LENGTH = 96,
  SHA512_HEX_LENGTH = 128,

  # Key size constants (in bits)
  AES_128_BITS = 128,
  AES_192_BITS = 192,
  AES_256_BITS = 256,
  RSA_2048_BITS = 2048,
  RSA_4096_BITS = 4096,

  # Check if string is valid hex
  is_valid_hex
    : String -> Bool
    = fun s =>
      let chars = std.string.characters (std.string.lowercase s) in
      std.array.all (fun c =>
        (c >= "0" && c <= "9") || (c >= "a" && c <= "f")
      ) chars,

  # Validate MD5 hash format
  is_valid_md5
    : String -> Bool
    = fun hash =>
      std.string.length hash == MD5_HEX_LENGTH && is_valid_hex hash,

  # Validate SHA-1 hash format
  is_valid_sha1
    : String -> Bool
    = fun hash =>
      std.string.length hash == SHA1_HEX_LENGTH && is_valid_hex hash,

  # Validate SHA-256 hash format
  is_valid_sha256
    : String -> Bool
    = fun hash =>
      std.string.length hash == SHA256_HEX_LENGTH && is_valid_hex hash,

  # Validate SHA-384 hash format
  is_valid_sha384
    : String -> Bool
    = fun hash =>
      std.string.length hash == SHA384_HEX_LENGTH && is_valid_hex hash,

  # Validate SHA-512 hash format
  is_valid_sha512
    : String -> Bool
    = fun hash =>
      std.string.length hash == SHA512_HEX_LENGTH && is_valid_hex hash,

  # Validate Base64 encoding
  is_valid_base64
    : String -> Bool
    = fun s =>
      let len = std.string.length s in
      if len == 0 || len % 4 != 0 then
        false
      else
        let chars = std.string.characters s in
        std.array.all (fun c =>
          (c >= "A" && c <= "Z") ||
          (c >= "a" && c <= "z") ||
          (c >= "0" && c <= "9") ||
          c == "+" || c == "/" || c == "="
        ) chars,

  # Validate Base64URL encoding (URL-safe base64)
  is_valid_base64url
    : String -> Bool
    = fun s =>
      let len = std.string.length s in
      if len == 0 then
        false
      else
        let chars = std.string.characters s in
        std.array.all (fun c =>
          (c >= "A" && c <= "Z") ||
          (c >= "a" && c <= "z") ||
          (c >= "0" && c <= "9") ||
          c == "-" || c == "_" || c == "="
        ) chars,

  # Check if string could be a valid JWT structure
  is_valid_jwt_structure
    : String -> Bool
    = fun jwt =>
      let parts = std.string.split "." jwt in
      std.array.length parts == 3 &&
      std.array.all (fun part => std.string.length part > 0) parts,

  # Crypto key configuration
  KeyConfig = {
    algorithm | String,
    key_size_bits | Number,
    purpose | String,
  },

  # Common algorithm configurations
  aes_128_config = {
    algorithm = "AES",
    key_size_bits = AES_128_BITS,
    purpose = "symmetric_encryption",
  },

  aes_256_config = {
    algorithm = "AES",
    key_size_bits = AES_256_BITS,
    purpose = "symmetric_encryption",
  },

  rsa_2048_config = {
    algorithm = "RSA",
    key_size_bits = RSA_2048_BITS,
    purpose = "asymmetric_encryption",
  },

  rsa_4096_config = {
    algorithm = "RSA",
    key_size_bits = RSA_4096_BITS,
    purpose = "asymmetric_encryption",
  },

  # Contracts for hash values
  Md5Hash = std.contract.from_predicate is_valid_md5,
  Sha1Hash = std.contract.from_predicate is_valid_sha1,
  Sha256Hash = std.contract.from_predicate is_valid_sha256,
  Sha384Hash = std.contract.from_predicate is_valid_sha384,
  Sha512Hash = std.contract.from_predicate is_valid_sha512,

  # Contracts for encodings
  Base64 = std.contract.from_predicate is_valid_base64,
  Base64Url = std.contract.from_predicate is_valid_base64url,

  # JWT structure contract
  JwtStructure = std.contract.from_predicate is_valid_jwt_structure,

  # Validate key size is appropriate for algorithm
  validate_key_size
    : String -> Number -> Bool
    = fun algorithm bits =>
      if algorithm == "AES" then
        bits == 128 || bits == 192 || bits == 256
      else if algorithm == "RSA" then
        bits >= 2048
      else if algorithm == "ED25519" then
        bits == 256
      else
        bits > 0,
}
