{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-
  safe_crypto.orc - Cryptographic operations for Orc via libproven

  All cryptographic computation is performed by libproven's formally
  verified Idris 2 core. This module provides Orc-idiomatic wrappers
  for constant-time comparison, random bytes, and hex encoding/decoding.
-}

include "src/ffi.orc"
include "src/proven.orc"
include "src/safe_string.orc"

-- ============================================================================
-- Constant-Time Comparison
-- ============================================================================

-- Compare two byte sequences in constant time (timing-attack resistant).
-- Publishes true if equal, false otherwise, or halts on FFI error.
-- Delegates to proven_crypto_constant_time_eq via FFI.
def constant_time_eq(a, b) =
  with_string_ptr(a, lambda(a_ptr, a_len) =
    with_string_ptr(b, lambda(b_ptr, b_len) =
      val result = ffi_crypto_constant_time_eq(a_ptr, a_len, b_ptr, b_len)
      extract_bool(result)
    )
  )

-- ============================================================================
-- Secure Random Bytes
-- ============================================================================

import class JNAMemory = "com.sun.jna.Memory"

-- Generate cryptographically secure random bytes.
-- Publishes a byte array of the requested length, or halts on error.
-- Delegates to proven_crypto_random_bytes via FFI.
def random_bytes(len) =
  val buf = JNAMemory(len)
  val status = ffi_crypto_random_bytes(buf, len)
  if status = 0 then
    buf.getByteArray(0, len)
  else stop

-- ============================================================================
-- Hex Encoding / Decoding
-- ============================================================================

-- Hex-encode a byte string.
-- Publishes the hex-encoded string, or halts on error.
-- Delegates to proven_hex_encode via FFI.
def hex_encode(s) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_hex_encode(ptr, len, false)
    extract_string(result)
  )

-- Hex-encode a byte string in uppercase.
-- Publishes the uppercase hex-encoded string, or halts on error.
-- Delegates to proven_hex_encode via FFI.
def hex_encode_upper(s) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_hex_encode(ptr, len, true)
    extract_string(result)
  )

-- Hex-decode a hex string into raw bytes.
-- Publishes the decoded byte array, or halts on error.
-- Delegates to proven_hex_decode via FFI.
def hex_decode(s) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_hex_decode(ptr, len)
    if result /= null then
      val decoded = result.getByteArray(0, result.size())
      ffi_hex_free(result)
      >> decoded
    else stop
  )

-- ============================================================================
-- Checksum Operations
-- ============================================================================

-- Compute CRC32 checksum of a byte string.
-- Publishes the CRC32 value as an integer, or halts on error.
-- Delegates to proven_checksum_crc32 via FFI.
def crc32(s) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_checksum_crc32(ptr, len)
    extract_int(result)
  )

-- Verify CRC32 checksum of a byte string against an expected value.
-- Publishes true if the checksum matches, false otherwise.
-- Delegates to proven_checksum_verify_crc32 via FFI.
def verify_crc32(s, expected) =
  with_string_ptr(s, lambda(ptr, len) =
    val result = ffi_checksum_verify_crc32(ptr, len, expected)
    extract_bool(result)
  )
