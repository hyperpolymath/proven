// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeCrypto - Cryptographic primitives via libproven FFI.
//
// Status: GATED on proven#88 (proven_crypto_* + proven_hex_encode).

module SafeCrypto {

  public use LibProven;
  use CTypes;

  /** Constant-time byte comparison (timing-attack safe). */
  proc constantTimeEq(a: string, b: string): Maybe(bool) {
    var (ptrA, lenA) = toCBytes(a);
    var (ptrB, lenB) = toCBytes(b);
    var r = provenCryptoConstantTimeEq(ptrA, lenA, ptrB, lenB);
    if isOk(r.status) then return some(r.value);
    return absent(bool);
  }

  /**
   * Fill a buffer with cryptographically secure random bytes.
   *
   * Returns ``some(true)`` on success and ``absent(bool)`` on failure;
   * the caller-provided buffer is filled in-place via the FFI.
   * Array-typed Maybe return is sidestepped because the result type
   * would carry a domain.
   */
  proc randomBytes(ref buf: [] uint(8)): Maybe(bool) {
    var status = provenCryptoRandomBytes(c_ptrTo(buf[buf.domain.low]),
                                          buf.size: c_size_t);
    if isOk(status) then return some(true);
    return absent(bool);
  }

  /** Encode bytes to hexadecimal string. */
  proc hexEncode(data: string, uppercase: bool = false): Maybe(string) {
    var (ptr, len) = toCBytes(data);
    var r = provenHexEncode(ptr, len, uppercase);
    if isOk(r.status) then return some(extractString(r));
    return absent(string);
  }

}
