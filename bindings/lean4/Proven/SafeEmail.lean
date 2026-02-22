/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeEmail - Email address validation

Provides RFC 5321 email validation. The validation logic is implemented in
formally verified Idris 2 code and accessed via the Zig FFI bridge
(`libproven`). No email parsing logic is reimplemented in Lean.
-/

import Proven.FFI

namespace Proven.SafeEmail

open Proven.FFI

/-- Error type for email operations. -/
structure EmailError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString EmailError := ⟨fun e => s!"EmailError: {e.status}"⟩

/-- Alias for email operation results. -/
abbrev EmailResult (a : Type) := Except EmailError a

-- Internal helper
private def unwrapBoolResult (r : BoolResult) : EmailResult Bool :=
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => .ok r.value
  | _   => .error { status := s }

-- ============================================================================
-- Email validation
-- ============================================================================

/-- Validate an email address according to RFC 5321 (simplified).
    Returns `true` if the email is syntactically valid.
    Delegates to `proven_email_is_valid`. -/
def isValid (email : String) : IO (EmailResult Bool) := do
  let r <- provenEmailIsValid email.toUTF8
  return unwrapBoolResult r

/-- Validate an email address, returning `Option Bool`.
    Returns `some true` if valid, `some false` if invalid,
    `none` on internal error. -/
def isValid? (email : String) : IO (Option Bool) := do
  let r <- isValid email
  return r.toOption

/-- Validate an email address, returning a simple `Bool`.
    Returns `false` both for invalid addresses and on internal error.
    Use `isValid` for richer error reporting. -/
def validate (email : String) : IO Bool := do
  let r <- isValid? email
  return r.getD false

end Proven.SafeEmail
