// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeEmail - Email validation that cannot crash.
////
//// Thin FFI wrapper over libproven proven_email_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Validate email address (RFC 5321 simplified).
/// Does not verify that the email address actually exists.
@external(erlang, "proven_nif", "email_is_valid")
pub fn is_valid(email: String) -> Result(Bool, String)
