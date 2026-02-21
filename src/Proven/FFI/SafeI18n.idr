-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeI18n operations
|||
||| This module exports internationalization safety to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent format string injection.
|||
||| Return conventions:
||| - Language tag parsing -> (status: Int, value: String)
||| - Bool checks -> Int (0 = false, 1 = true)
||| - Text direction -> Int (0 = LTR, 1 = RTL)
module Proven.FFI.SafeI18n

import Proven.SafeI18n

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Language Tag Operations
--------------------------------------------------------------------------------

export
proven_idris_i18n_is_valid_language : String -> Int
proven_idris_i18n_is_valid_language = encodeBool . isValidLanguage

export
proven_idris_i18n_is_valid_region : String -> Int
proven_idris_i18n_is_valid_region = encodeBool . isValidRegion

export
proven_idris_i18n_parse_language_tag : String -> (Int, String)
proven_idris_i18n_parse_language_tag s = case parseLanguageTag s of
  Nothing => (1, "Invalid BCP 47 language tag")
  Just tag => (0, show tag)

export
proven_idris_i18n_get_language : String -> (Int, String)
proven_idris_i18n_get_language s = case parseLanguageTag s of
  Nothing => (1, "Invalid language tag")
  Just tag => (0, language tag)

export
proven_idris_i18n_get_region : String -> (Int, String)
proven_idris_i18n_get_region s = case parseLanguageTag s of
  Nothing => (1, "Invalid language tag")
  Just tag => case region tag of
    Nothing => (1, "No region")
    Just r => (0, r)

--------------------------------------------------------------------------------
-- Text Direction
--------------------------------------------------------------------------------

export
proven_idris_i18n_text_direction : String -> Int
proven_idris_i18n_text_direction s = case parseLanguageTag s of
  Nothing => 0  -- Default LTR
  Just tag => case textDirection tag of
    LTR => 0
    RTL => 1

--------------------------------------------------------------------------------
-- Message Key Validation
--------------------------------------------------------------------------------

export
proven_idris_i18n_is_valid_message_key : String -> Int
proven_idris_i18n_is_valid_message_key = encodeBool . isValidMessageKey

export
proven_idris_i18n_mk_message_key : String -> (Int, String)
proven_idris_i18n_mk_message_key s = case mkMessageKey s of
  Nothing => (1, "Invalid message key")
  Just (MkMessageKey k) => (0, k)

--------------------------------------------------------------------------------
-- Template Interpolation
--------------------------------------------------------------------------------

export
proven_idris_i18n_parse_template_params : String -> String
proven_idris_i18n_parse_template_params s =
  let tmpl = parseTemplate s
  in show (parameterNames tmpl)

export
proven_idris_i18n_interpolate_single : String -> String -> String -> String
proven_idris_i18n_interpolate_single template key value =
  let tmpl = parseTemplate template
  in interpolate tmpl [(key, StringParam value)]
