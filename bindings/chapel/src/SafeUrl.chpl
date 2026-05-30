// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeUrl - URL encoding/decoding via libproven FFI.
//
// Status: GATED on proven#88.

module SafeUrl {

  public use LibProven;

  /** URL-encode a string (RFC 3986 percent encoding). */
  proc encode(s: string): Maybe(string) {
    var (ptr, len) = toCBytes(s);
    var r = provenHttpUrlEncode(ptr, len);
    if isOk(r.status) then return some(extractString(r));
    return absent(string);
  }

  /** URL-decode a percent-encoded string. */
  proc decode(s: string): Maybe(string) {
    var (ptr, len) = toCBytes(s);
    var r = provenHttpUrlDecode(ptr, len);
    if isOk(r.status) then return some(extractString(r));
    return absent(string);
  }

}
