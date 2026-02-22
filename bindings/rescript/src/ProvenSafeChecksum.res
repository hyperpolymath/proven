// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeChecksum - Typed wrapper for hash and checksum verification.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

open ProvenResult

/** JavaScript bindings to the SafeChecksum FFI wrapper. */
module SafeChecksumJs = {
  @module("../../javascript/src/safe_checksum.js") @scope("SafeChecksum")
  external crc32: Uint8Array.t => jsResult<int> = "crc32"

  @module("../../javascript/src/safe_checksum.js") @scope("SafeChecksum")
  external verifyCrc32: (Uint8Array.t, int) => jsResult<bool> = "verifyCrc32"
}

/**
 * Compute CRC32 checksum of data.
 * Delegates to proven_checksum_crc32 via FFI.
 *
 * @param data The data to checksum.
 * @returns Ok(checksum) or Error.
 */
let crc32 = (data: Uint8Array.t): result<int, string> => {
  SafeChecksumJs.crc32(data)->fromJs
}

/**
 * Verify a CRC32 checksum against data.
 * Delegates to proven_checksum_verify_crc32 via FFI.
 *
 * @param data The data to verify.
 * @param expected The expected checksum value.
 * @returns Ok(true/false) or Error.
 */
let verifyCrc32 = (data: Uint8Array.t, expected: int): result<bool, string> => {
  SafeChecksumJs.verifyCrc32(data, expected)->fromJs
}
