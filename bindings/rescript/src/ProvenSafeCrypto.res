// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeCrypto - Typed wrapper for cryptographic operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeCrypto FFI wrapper. */
module SafeCryptoJs = {
  @module("../../javascript/src/safe_crypto.js") @scope("SafeCrypto")
  external constantTimeEq: (Uint8Array.t, Uint8Array.t) => jsResult<bool> = "constantTimeEq"

  @module("../../javascript/src/safe_crypto.js") @scope("SafeCrypto")
  external randomBytes: Uint8Array.t => jsResult<Uint8Array.t> = "randomBytes"
}

/**
 * Compare two byte arrays in constant time (prevents timing attacks).
 * Delegates to proven_crypto_constant_time_eq via FFI.
 *
 * @param a First byte array.
 * @param b Second byte array.
 * @returns Ok(true/false) or Error.
 */
let constantTimeEq = (a: Uint8Array.t, b: Uint8Array.t): result<bool, string> => {
  SafeCryptoJs.constantTimeEq(a, b)->fromJs
}

/**
 * Generate cryptographically secure random bytes.
 * Delegates to proven_crypto_random_bytes via FFI.
 *
 * The buffer must be pre-allocated to the desired length.
 *
 * @param buffer The buffer to fill with random bytes.
 * @returns Ok(filled_buffer) or Error.
 */
let randomBytes = (buffer: Uint8Array.t): result<Uint8Array.t, string> => {
  SafeCryptoJs.randomBytes(buffer)->fromJs
}
