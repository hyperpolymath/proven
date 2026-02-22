{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-
  safe_email.orc - Email validation for Orc via libproven

  All validation is performed by libproven's formally verified Idris 2
  core. This module provides Orc-idiomatic wrappers that leverage
  Orc's concurrency for bulk email validation.
-}

include "src/ffi.orc"
include "src/proven.orc"
include "src/safe_string.orc"

-- ============================================================================
-- Email Validation
-- ============================================================================

-- Validate an email address (RFC 5321/5322).
-- Publishes true if valid, false if invalid, or halts on FFI error.
-- Delegates to proven_email_is_valid via FFI.
def is_valid_email(email) =
  with_string_ptr(email, lambda(ptr, len) =
    val result = ffi_email_is_valid(ptr, len)
    extract_bool(result)
  )

-- ============================================================================
-- Concurrent Bulk Validation
-- ============================================================================

-- Validate a list of email addresses concurrently.
-- Publishes (email, valid) pairs for each email.
-- Orc's natural concurrency processes all emails in parallel.
def validate_emails(emails) =
  each(emails) >email>
  val valid = is_valid_email(email)
  (email, valid)

-- Filter a list to only valid emails.
-- Publishes only those emails that pass validation.
def filter_valid_emails(emails) =
  each(emails) >email>
  val valid = is_valid_email(email)
  if valid then email
  else stop

-- Filter a list to only invalid emails (for reporting).
-- Publishes only those emails that fail validation.
def filter_invalid_emails(emails) =
  each(emails) >email>
  val valid = is_valid_email(email)
  if valid then stop
  else email
