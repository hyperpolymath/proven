// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail - Email validation via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.

module SafeEmail {

  public use LibProven;

  /**
   * Validate email address (RFC 5321 simplified).
   *
   * :arg email: Email address string.
   * :returns: ``none`` on error, true if valid, false otherwise.
   */
  proc isValid(email: string): bool? {
    var (ptr, len) = toCBytes(email);
    var r = provenEmailIsValid(ptr, len);
    if isOk(r.status) then return r.value;
    return none;
  }

}
