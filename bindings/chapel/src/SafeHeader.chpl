// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeHeader - HTTP header safety via libproven FFI.
//
// All computation is performed in the libproven C ABI (currently a Zig
// implementation under ffi/zig/src/main.zig; the Idris2 backing for this
// primitive lands as part of the proven#88 export ramp).  This module is
// a thin wrapper; it does NOT reimplement any logic.
//
// Status: WIRED.  proven_header_has_crlf is exported by libproven 0.9.0
// and verified by chapel-symbol-audit.

module SafeHeader {

  public use LibProven;

  /**
   * Check whether a header value contains a CRLF injection sequence ("\r\n").
   *
   * CRLF injection allows attackers to inject additional HTTP headers or
   * begin a response body when user input is reflected into a header.
   *
   * :arg value: Candidate header value.
   * :returns: ``absent(bool)`` on FFI error; otherwise ``some(true)`` if
   *           CRLF found, ``some(false)`` if safe.
   */
  proc hasCrlf(value: string): Maybe(bool) {
    var (ptr, len) = toCBytes(value);
    var r = provenHeaderHasCrlf(ptr, len);
    if isOk(r.status) then return some(r.value);
    return absent(bool);
  }

}
