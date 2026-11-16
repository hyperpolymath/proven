// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeHeader - HTTP header safety via libproven FFI.
//
// The qualified libproven profile calls the Idris byte kernel through RefC.
// A library built without that backend returns -99, mapped to absent below.
// This module only marshals the byte span and status.
//
// Status: WIRED.  proven_header_has_crlf is exported by libproven 0.9.0
// and verified by chapel-symbol-audit.

module SafeHeader {

  public use LibProven;

  /**
   * Check whether a header value contains either CR or LF, including lone bytes.
   *
   * CRLF injection allows attackers to inject additional HTTP headers or
   * begin a response body when user input is reflected into a header.
   *
   * :arg value: Candidate header value.
   * :returns: ``absent(bool)`` on FFI error; otherwise ``some(true)`` if
   *           CR or LF found, ``some(false)`` if neither occurs.
   */
  proc hasCrlf(value: string): Maybe(bool) {
    var (ptr, len) = toCBytes(value);
    var r = provenHeaderHasCrlf(ptr, len);
    if isOk(r.status) then return some(r.value);
    return absent(bool);
  }

}
