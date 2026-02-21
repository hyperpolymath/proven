-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafePhone operations
|||
||| This module exports E.164 phone number operations to the C ABI
||| via Idris2's RefC backend.
|||
||| Return conventions:
||| - Phone parsing -> (status: Int, value: String)
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafePhone

import Proven.SafePhone

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

export
proven_idris_phone_is_valid_e164 : String -> Int
proven_idris_phone_is_valid_e164 = encodeBool . isValidE164

export
proven_idris_phone_is_digits_only : String -> Int
proven_idris_phone_is_digits_only = encodeBool . isDigitsOnly

--------------------------------------------------------------------------------
-- Parsing and Formatting
--------------------------------------------------------------------------------

export
proven_idris_phone_parse_e164 : String -> (Int, String)
proven_idris_phone_parse_e164 s = case parseE164 s of
  Nothing => (1, "Invalid E.164 phone number")
  Just phone => (0, formatE164 phone)

export
proven_idris_phone_format_display : String -> (Int, String)
proven_idris_phone_format_display s = case parseE164 s of
  Nothing => (1, "Invalid phone number")
  Just phone => (0, formatDisplay phone)

export
proven_idris_phone_mask : String -> (Int, String)
proven_idris_phone_mask s = case parseE164 s of
  Nothing => (1, "Invalid phone number")
  Just phone => (0, maskPhone phone)

--------------------------------------------------------------------------------
-- Phone Components
--------------------------------------------------------------------------------

export
proven_idris_phone_get_country_code : String -> (Int, String)
proven_idris_phone_get_country_code s = case parseE164 s of
  Nothing => (1, "Invalid phone number")
  Just phone => (0, show (phoneCountry phone))

export
proven_idris_phone_get_subscriber : String -> (Int, String)
proven_idris_phone_get_subscriber s = case parseE164 s of
  Nothing => (1, "Invalid phone number")
  Just phone => (0, phoneSubscriber phone)

--------------------------------------------------------------------------------
-- Formatting Utilities
--------------------------------------------------------------------------------

export
proven_idris_phone_strip_formatting : String -> String
proven_idris_phone_strip_formatting = stripFormatting

export
proven_idris_phone_max_e164_length : Int
proven_idris_phone_max_e164_length = cast maxE164Length
