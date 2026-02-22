/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeCrypto - Cryptographic primitives

Provides constant-time comparison, cryptographic random byte generation, hex
encoding/decoding, and CRC32 checksums. Every operation delegates to the
formally verified Idris 2 core via the Zig FFI bridge (`libproven`). No
cryptographic logic is reimplemented in Lean.

## Security note

`constantTimeEq` uses a timing-attack-resistant comparison algorithm
implemented in the verified core. Do NOT replace it with naive byte
comparison.
-/

import Proven.FFI

namespace Proven.SafeCrypto

open Proven.FFI

/-- Error type for crypto operations. -/
structure CryptoError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString CryptoError := ⟨fun e => s!"CryptoError: {e.status}"⟩

/-- Alias for crypto operation results. -/
abbrev CryptoResult (a : Type) := Except CryptoError a

-- Internal helpers
private def unwrapBoolResult (r : BoolResult) : CryptoResult Bool :=
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => .ok r.value
  | _   => .error { status := s }

private def unwrapStringResult (raw : StringResultRaw) : IO (CryptoResult String) := do
  let s := ProvenStatus.ofInt32 raw.status
  match s with
  | .ok => do
    let maybeStr <- marshalStringResult raw
    match maybeStr with
    | some str => return .ok str
    | none     => return .error { status := .errAllocationFailed }
  | _ => return .error { status := s }

private def unwrapIntResult (r : IntResult) : CryptoResult Int64 :=
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => .ok r.value
  | _   => .error { status := s }

-- ============================================================================
-- Constant-time comparison
-- ============================================================================

/-- Constant-time byte array comparison (timing-attack safe).
    Returns `true` if the two byte arrays are equal, `false` otherwise.
    Returns `false` if the arrays have different lengths.
    Delegates to `proven_crypto_constant_time_eq`. -/
def constantTimeEq (a b : ByteArray) : IO (CryptoResult Bool) := do
  let r <- provenCryptoConstantTimeEq a b
  return unwrapBoolResult r

/-- Constant-time comparison returning `Option Bool`. -/
def constantTimeEq? (a b : ByteArray) : IO (Option Bool) := do
  let r <- constantTimeEq a b
  return r.toOption

/-- Constant-time string comparison (timing-attack safe).
    Converts strings to UTF-8 bytes before comparison. -/
def constantTimeStringEq (a b : String) : IO (CryptoResult Bool) :=
  constantTimeEq a.toUTF8 b.toUTF8

-- ============================================================================
-- Random byte generation
-- ============================================================================

/-- Generate cryptographically secure random bytes.
    Delegates to `proven_crypto_random_bytes`. -/
def randomBytes (len : Nat) : IO ByteArray :=
  provenCryptoRandomBytes len.toUSize

-- ============================================================================
-- Hex encoding/decoding
-- ============================================================================

/-- Encode bytes to lowercase hex string.
    Delegates to `proven_hex_encode`. -/
def hexEncode (data : ByteArray) : IO (CryptoResult String) := do
  let raw <- provenHexEncode data false
  unwrapStringResult raw

/-- Encode bytes to uppercase hex string.
    Delegates to `proven_hex_encode`. -/
def hexEncodeUpper (data : ByteArray) : IO (CryptoResult String) := do
  let raw <- provenHexEncode data true
  unwrapStringResult raw

/-- Decode a hex string to bytes.
    Delegates to `proven_hex_decode`.
    Note: the raw result is marshaled as a string (bytes); the caller
    should convert the resulting `String` to `ByteArray` if needed. -/
def hexDecode (hexStr : String) : IO (CryptoResult String) := do
  let raw <- provenHexDecode hexStr.toUTF8
  unwrapStringResult raw

-- ============================================================================
-- CRC32 checksum
-- ============================================================================

/-- Calculate CRC32 checksum of byte data.
    Returns the checksum as `Int64`.
    Delegates to `proven_checksum_crc32`. -/
def crc32 (data : ByteArray) : IO (CryptoResult Int64) := do
  let r <- provenChecksumCrc32 data
  return unwrapIntResult r

/-- Verify that byte data matches an expected CRC32 value.
    Delegates to `proven_checksum_verify_crc32`. -/
def verifyCrc32 (data : ByteArray) (expected : UInt32) : IO (CryptoResult Bool) := do
  let r <- provenChecksumVerifyCrc32 data expected
  return unwrapBoolResult r

-- ============================================================================
-- Convenience: Option-returning variants
-- ============================================================================

/-- Hex encoding returning `Option`. -/
def hexEncode? (data : ByteArray) : IO (Option String) := do
  let r <- hexEncode data
  return r.toOption

/-- CRC32 returning `Option`. -/
def crc32? (data : ByteArray) : IO (Option Int64) := do
  let r <- crc32 data
  return r.toOption

end Proven.SafeCrypto
