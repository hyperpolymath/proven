// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_crypto.carbon - Safe cryptographic primitives via libproven FFI.
//
// Provides constant-time comparison and cryptographic random byte generation.
// All operations delegate to Idris 2 verified code.
// No cryptographic logic is implemented in Carbon.

package Proven api;

import Proven library "lib_proven";

// Safe cryptographic operations.
//
// All methods call the formally verified Idris 2 implementation via FFI.
class SafeCrypto {

  // Constant-time byte comparison (timing-attack resistant).
  //
  // Returns Optional(bool): true if both byte slices are identical.
  // The comparison takes the same amount of time regardless of where
  // a mismatch occurs. Returns false if lengths differ.
  // None indicates the FFI call itself failed.
  fn ConstantTimeEq(
      ptr1: Cpp.Ptr(u8), len1: i64,
      ptr2: Cpp.Ptr(u8), len2: i64
  ) -> Optional(bool) {
    var result: BoolResult = proven_crypto_constant_time_eq(ptr1, len1, ptr2, len2);
    if (Succeeded(result.status)) {
      return Optional(bool).Create(result.value != 0);
    }
    return Optional(bool).None();
  }

  // Fill a buffer with cryptographically secure random bytes.
  //
  // Uses the Idris 2 verified CSPRNG implementation.
  // Returns true on success, false on failure.
  fn RandomBytes(buf: Cpp.Ptr(u8), len: i64) -> bool {
    var status: i32 = proven_crypto_random_bytes(buf, len);
    return Succeeded(status);
  }
}
