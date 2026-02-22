// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto - Cryptographic primitives via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.

module SafeCrypto {

  public use LibProven;

  /**
   * Constant-time byte comparison (timing-attack safe).
   *
   * :arg a: First byte sequence (string).
   * :arg b: Second byte sequence (string).
   * :returns: ``none`` on error, true if equal, false otherwise.
   */
  proc constantTimeEq(a: string, b: string): bool? {
    var (ptrA, lenA) = toCBytes(a);
    var (ptrB, lenB) = toCBytes(b);
    var r = provenCryptoConstantTimeEq(ptrA, lenA, ptrB, lenB);
    if isOk(r.status) then return r.value;
    return none;
  }

  /**
   * Fill a buffer with cryptographically secure random bytes.
   *
   * :arg n: Number of bytes to generate.
   * :returns: ``none`` on error, otherwise an array of random bytes.
   */
  proc randomBytes(n: int): [0..#n] uint(8)? {
    var buf : [0..#n] uint(8);
    var status = provenCryptoRandomBytes(c_ptrTo(buf[0]), n: c_size_t);
    if isOk(status) then return buf;
    return none;
  }

  /**
   * Encode bytes to hexadecimal string.
   *
   * :arg data: Input bytes (string).
   * :arg uppercase: Use uppercase hex digits (default false).
   * :returns: ``none`` on error, otherwise the hex string.
   */
  proc hexEncode(data: string, uppercase: bool = false): string? {
    var (ptr, len) = toCBytes(data);
    var r = provenHexEncode(ptr, len, uppercase);
    if isOk(r.status) {
      var result = extractString(r);
      return result;
    }
    return none;
  }

}
