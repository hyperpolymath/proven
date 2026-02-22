// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe-email.kk -- Safe email validation via libproven FFI.
//
// Validates email addresses according to RFC 5321 simplified rules.
// All computation is performed by the Idris 2 verified implementation.
// No validation logic is reimplemented in Koka.
module proven/safe-email

import std/num/int64
import proven/lib-proven

// ============================================================================
// Raw FFI declarations
// ============================================================================

// Validate email address (RFC 5321 simplified).
extern raw-is-valid(ptr: intptr_t, len: int64): bool-result
  c inline "{ ProvenBoolResult r = proven_email_is_valid((const uint8_t*)#1, (size_t)#2); return kk_proven_bool_result(r.status, r.value ? 1 : 0, kk_context()); }"

// ============================================================================
// Safe public API
// ============================================================================

// Validate an email address per RFC 5321 simplified rules.
// Returns Just(true) if valid, Just(false) if invalid, Nothing on error.
pub fun is-valid-email(email: string): <ndet> maybe<bool>
  val r = raw-is-valid(email.cptr, email.count.int64)
  bool-result-to-maybe(r)

// Convenience: assert that an email is valid, returning the email on success.
// Returns Nothing if the email is not valid.
pub fun validate-email(email: string): <ndet> maybe<string>
  match is-valid-email(email)
    Just(True)  -> Just(email)
    Just(False) -> Nothing
    Nothing     -> Nothing
