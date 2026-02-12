-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeEmail operations
|||
||| This module exports safe email operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and handle RFC 5321/5322 validation.
|||
||| Return conventions:
||| - EmailAddress → encoded as "local@domain" string
||| - Maybe EmailAddress → (status: Int, value: String)
|||   - status = 0: Success, value is "local@domain"
|||   - status = 1: Error (invalid email)
||| - Bool → Int (0 = false, 1 = true)
module Proven.FFI.SafeEmail

import Proven.SafeEmail
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int for FFI
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode Maybe EmailAddress as (status, emailString) tuple
encodeEmailResult : Maybe EmailAddress -> (Int, String)
encodeEmailResult Nothing = (1, "")
encodeEmailResult (Just email) = (0, toString email)

||| Encode Maybe String as (status, value) tuple
encodeStringResult : Maybe String -> (Int, String)
encodeStringResult Nothing = (1, "")
encodeStringResult (Just s) = (0, s)

--------------------------------------------------------------------------------
-- Email Construction and Parsing
--------------------------------------------------------------------------------

export
proven_idris_email_validate : String -> (Int, String)
proven_idris_email_validate s =
  encodeEmailResult (validateEmail s)

export
proven_idris_email_mk : String -> String -> (Int, String)
proven_idris_email_mk local domain =
  encodeEmailResult (mkEmail local domain)

--------------------------------------------------------------------------------
-- Email Validation
--------------------------------------------------------------------------------

export
proven_idris_email_is_valid : String -> Int
proven_idris_email_is_valid s =
  encodeBool (isValidEmail s)

export
proven_idris_email_is_valid_local : String -> Int
proven_idris_email_is_valid_local s =
  encodeBool (isValidLocalPart s)

export
proven_idris_email_is_valid_domain : String -> Int
proven_idris_email_is_valid_domain s =
  encodeBool (isValidDomain s)

--------------------------------------------------------------------------------
-- Email Accessors
--------------------------------------------------------------------------------

||| Parse email and get local part
export
proven_idris_email_get_local : String -> (Int, String)
proven_idris_email_get_local s =
  case validateEmail s of
    Nothing => (1, "")
    Just email => (0, getLocalPart email)

||| Parse email and get domain
export
proven_idris_email_get_domain : String -> (Int, String)
proven_idris_email_get_domain s =
  case validateEmail s of
    Nothing => (1, "")
    Just email => (0, getDomain email)

||| Parse email and get TLD
export
proven_idris_email_get_tld : String -> (Int, String)
proven_idris_email_get_tld s =
  case validateEmail s of
    Nothing => (1, "")
    Just email => encodeStringResult (getTLD email)

||| Parse email and get second-level domain
export
proven_idris_email_get_sld : String -> (Int, String)
proven_idris_email_get_sld s =
  case validateEmail s of
    Nothing => (1, "")
    Just email => encodeStringResult (getSecondLevelDomain email)

--------------------------------------------------------------------------------
-- Email Normalization
--------------------------------------------------------------------------------

||| Parse and normalize email (lowercase domain)
export
proven_idris_email_normalize : String -> (Int, String)
proven_idris_email_normalize s =
  case validateEmail s of
    Nothing => (1, "")
    Just email => (0, toString (normalize email))

||| Parse and normalize email (lowercase both parts)
export
proven_idris_email_normalize_full : String -> (Int, String)
proven_idris_email_normalize_full s =
  case validateEmail s of
    Nothing => (1, "")
    Just email => (0, toString (normalizeFull email))

||| Parse email and get base (remove +suffix)
export
proven_idris_email_get_base : String -> (Int, String)
proven_idris_email_get_base s =
  case validateEmail s of
    Nothing => (1, "")
    Just email => (0, toString (getBaseEmail email))

--------------------------------------------------------------------------------
-- Email Security
--------------------------------------------------------------------------------

export
proven_idris_email_sanitize_header : String -> String
proven_idris_email_sanitize_header s =
  sanitizeForHeader s

export
proven_idris_email_escape_html : String -> (Int, String)
proven_idris_email_escape_html s =
  case validateEmail s of
    Nothing => (1, "")
    Just email => (0, escapeForHTML email)

export
proven_idris_email_is_disposable_domain : String -> Int
proven_idris_email_is_disposable_domain domain =
  encodeBool (isDisposableDomain domain)

export
proven_idris_email_is_disposable : String -> Int
proven_idris_email_is_disposable s =
  case validateEmail s of
    Nothing => 0  -- Invalid emails aren't disposable (just invalid)
    Just email => encodeBool (isDisposableEmail email)

--------------------------------------------------------------------------------
-- Email Domain Operations
--------------------------------------------------------------------------------

export
proven_idris_email_domain_matches : String -> String -> Int
proven_idris_email_domain_matches pattern emailStr =
  case validateEmail emailStr of
    Nothing => 0  -- Invalid emails don't match
    Just email => encodeBool (domainMatches pattern email)

--------------------------------------------------------------------------------
-- Email Comparison
--------------------------------------------------------------------------------

||| Compare two emails (case-insensitive)
export
proven_idris_email_equals : String -> String -> Int
proven_idris_email_equals s1 s2 =
  case (validateEmail s1, validateEmail s2) of
    (Just e1, Just e2) => encodeBool (e1 == e2)
    _ => 0  -- If either is invalid, they're not equal
