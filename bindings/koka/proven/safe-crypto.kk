// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe-crypto.kk -- Safe cryptographic operations via libproven FFI.
//
// Provides constant-time comparison and secure random byte generation.
// All computation is performed by the Idris 2 verified implementation.
// No cryptographic logic is reimplemented in Koka.
module proven/safe-crypto

import std/num/int64
import proven/lib-proven

// ============================================================================
// Raw FFI declarations
// ============================================================================

// Constant-time byte comparison (timing-attack safe).
extern raw-constant-time-eq(
  ptr1: intptr_t, len1: int64,
  ptr2: intptr_t, len2: int64
): bool-result
  c inline "{ ProvenBoolResult r = proven_crypto_constant_time_eq((const uint8_t*)#1, (size_t)#2, (const uint8_t*)#3, (size_t)#4); return kk_proven_bool_result(r.status, r.value ? 1 : 0, kk_context()); }"

// Fill buffer with cryptographically secure random bytes.
// Returns status code (0 on success).
extern raw-random-bytes(ptr: intptr_t, len: int64): int32
  c inline "(int32_t)proven_crypto_random_bytes((uint8_t*)#1, (size_t)#2)"

// Hex encode bytes.
extern raw-hex-encode(ptr: intptr_t, len: int64, uppercase: bool): string
  c inline "({ ProvenStringResult r = proven_hex_encode((const uint8_t*)#1, (size_t)#2, #3); kk_string_t s = kk_string_empty(); if (r.status == 0 && r.value != NULL) { s = kk_string_alloc_len((kk_ssize_t)r.length, r.value, kk_context()); proven_free_string(r.value); } s; })"

extern raw-hex-encode-status(ptr: intptr_t, len: int64, uppercase: bool): int32
  c inline "({ ProvenStringResult r = proven_hex_encode((const uint8_t*)#1, (size_t)#2, #3); int32_t st = r.status; if (st == 0 && r.value != NULL) proven_free_string(r.value); st; })"

// ============================================================================
// Safe public API
// ============================================================================

// Constant-time comparison of two strings (timing-attack resistant).
// Returns Just(true) if equal, Just(false) if not, Nothing on error.
pub fun constant-time-eq(a: string, b: string): <ndet> maybe<bool>
  val r = raw-constant-time-eq(
    a.cptr, a.count.int64,
    b.cptr, b.count.int64
  )
  bool-result-to-maybe(r)

// Encode a string as lowercase hexadecimal.
// Returns Nothing on error.
pub fun hex-encode(input: string): <ndet> maybe<string>
  val status = raw-hex-encode-status(input.cptr, input.count.int64, False)
  if status == 0i32 then
    Just(raw-hex-encode(input.cptr, input.count.int64, False))
  else Nothing

// Encode a string as uppercase hexadecimal.
// Returns Nothing on error.
pub fun hex-encode-upper(input: string): <ndet> maybe<string>
  val status = raw-hex-encode-status(input.cptr, input.count.int64, True)
  if status == 0i32 then
    Just(raw-hex-encode(input.cptr, input.count.int64, True))
  else Nothing
