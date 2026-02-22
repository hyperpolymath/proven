// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail.pike - Email validation for Pike.
//
// All operations delegate to libproven via LibProven. Returns UNDEFINED
// on error.
//
// Usage:
//   import Proven;
//   LibProven.init();
//   if (SafeEmail.is_valid("user@example.com"))
//       write("Valid email\n");
//   LibProven.deinit();

//! @class SafeEmail
//! RFC 5321 email address validation.
//!
//! Validates email address format using the formally verified Idris 2
//! implementation in libproven.

protected LibProven lib = LibProven();

//! @decl int(0..1)|zero is_valid(string email)
//! Validate an email address (RFC 5321 simplified).
//! @param email
//!   Email address to validate.
//! @returns
//!   @expr{1@} if valid, @expr{0@} if invalid, @expr{UNDEFINED@} on error.
int(0..1)|zero is_valid(string email)
{
    return lib->call_bool("email_is_valid", ({email}));
}
