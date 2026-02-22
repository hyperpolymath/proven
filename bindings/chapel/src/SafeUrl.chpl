// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUrl - URL encoding/decoding via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.

module SafeUrl {

  public use LibProven;

  /**
   * URL-encode a string (RFC 3986 percent encoding).
   *
   * Unreserved characters (A-Za-z0-9-._~) pass through; all others
   * become %XX.
   *
   * :arg s: Input string.
   * :returns: ``none`` on error, otherwise the encoded string.
   */
  proc encode(s: string): string? {
    var (ptr, len) = toCBytes(s);
    var r = provenHttpUrlEncode(ptr, len);
    if isOk(r.status) {
      var result = extractString(r);
      return result;
    }
    return none;
  }

  /**
   * URL-decode a percent-encoded string.
   *
   * :arg s: Percent-encoded input string.
   * :returns: ``none`` on error, otherwise the decoded string.
   */
  proc decode(s: string): string? {
    var (ptr, len) = toCBytes(s);
    var r = provenHttpUrlDecode(ptr, len);
    if isOk(r.status) {
      var result = extractString(r);
      return result;
    }
    return none;
  }

}
