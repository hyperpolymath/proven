// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail - Email validation via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This class is a thin wrapper; it does NOT reimplement any logic.

primitive SafeEmail
  """
  Formally verified email address validation.

  Example:
  ```pony
  match SafeEmail.is_valid("user@example.com")
  | let v: Bool => if v then env.out.print("Valid!") end
  | None => env.out.print("Error")
  end
  ```
  """

  fun is_valid(email: String): (Bool | None) =>
    """
    Validate email address (RFC 5321 simplified).
    Returns true if valid, false if invalid, None on error.
    """
    let r = _LibProven.email_is_valid(email.cpointer(), email.size())
    if r.status == ProvenOk() then r.value else None end
