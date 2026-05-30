// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeEmail - Email validation via libproven FFI.
//
// Status: GATED on proven#88.

module SafeEmail {

  public use LibProven;

  /** Validate email address (RFC 5321 simplified). */
  proc isValid(email: string): Maybe(bool) {
    var (ptr, len) = toCBytes(email);
    var r = provenEmailIsValid(ptr, len);
    if isOk(r.status) then return some(r.value);
    return absent(bool);
  }

}
