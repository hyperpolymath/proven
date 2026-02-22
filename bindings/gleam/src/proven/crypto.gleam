// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeCrypto - Cryptographic primitives that cannot crash.
////
//// Thin FFI wrapper over libproven proven_crypto_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Constant-time byte comparison (timing-safe).
/// Use this for comparing secrets to prevent timing attacks.
/// Returns False if lengths differ.
@external(erlang, "proven_nif", "crypto_constant_time_eq")
pub fn constant_time_eq(a: BitArray, b: BitArray) -> Result(Bool, String)

/// Fill buffer with cryptographically secure random bytes.
/// Uses the system CSPRNG.
@external(erlang, "proven_nif", "crypto_random_bytes")
pub fn random_bytes(size: Int) -> Result(BitArray, String)
