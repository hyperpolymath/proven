// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_email.carbon - Safe email validation via libproven FFI.
//
// Validates email addresses according to RFC 5321/5322 simplified rules.
// All operations delegate to Idris 2 verified code.
// No validation logic is implemented in Carbon.

package Proven api;

import Proven library "lib_proven";

// Safe email validation.
//
// All methods call the formally verified Idris 2 implementation via FFI.
class SafeEmail {

  // Validate an email address per RFC 5321 simplified rules.
  //
  // Returns Optional(bool): true if valid, false if invalid.
  // None indicates the FFI call itself failed (e.g., null pointer).
  fn IsValid(data: Cpp.Ptr(u8), len: i64) -> Optional(bool) {
    var result: BoolResult = proven_email_is_valid(data, len);
    if (Succeeded(result.status)) {
      return Optional(bool).Create(result.value != 0);
    }
    return Optional(bool).None();
  }
}
