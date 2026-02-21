-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeArgs operations
|||
||| This module exports CLI argument parsing utilities to the C ABI via Idris2's RefC backend.
||| All functions are proven total and protect against injection and DoS via argument limits.
|||
||| Return conventions:
||| - Type parsers → (status: Int, result: String)
|||   - status = 0: Success, result contains parsed value
|||   - status = 1: Error, result contains error message
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Use strict defaults for untrusted input. Enforce argument length and count limits.
|||
||| NOTE: Full argument parsing (parseArgs) is not exposed as it requires building
||| complex Idris2 data structures (List ArgSpec). This module exports utility functions
||| for type conversion, validation, and option constant access.
module Proven.FFI.SafeArgs

import Proven.SafeArgs
import Proven.SafeArgs.Types
import Proven.SafeArgs.Parser
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode Maybe as (status, value)
encodeMaybe : Show a => Maybe a -> (Int, String)
encodeMaybe Nothing = (1, "parse failed")
encodeMaybe (Just x) = (0, show x)

||| Encode ArgType as Int
encodeArgType : ArgType -> Int
encodeArgType Flag = 0
encodeArgType Option = 1
encodeArgType Positional = 2
encodeArgType Rest = 3

--------------------------------------------------------------------------------
-- Type Parsers
--------------------------------------------------------------------------------

export
proven_idris_args_parse_bool : String -> (Int, String)
proven_idris_args_parse_bool s =
  case parseBoolArg s of
    Nothing => (1, "invalid boolean value")
    Just True => (0, "true")
    Just False => (0, "false")

export
proven_idris_args_parse_int : String -> (Int, String)
proven_idris_args_parse_int s = encodeMaybe (parseIntArg s)

export
proven_idris_args_parse_nat : String -> (Int, String)
proven_idris_args_parse_nat s = encodeMaybe (parseNatArg s)

export
proven_idris_args_parse_double : String -> (Int, String)
proven_idris_args_parse_double s = encodeMaybe (parseDoubleArg s)

--------------------------------------------------------------------------------
-- Argument Classification
--------------------------------------------------------------------------------

export
proven_idris_args_is_long_opt : String -> Int
proven_idris_args_is_long_opt arg =
  case unpack arg of
    ('-' :: '-' :: _ :: _) => 1
    _ => 0

export
proven_idris_args_is_short_opt : String -> Int
proven_idris_args_is_short_opt arg =
  case unpack arg of
    ('-' :: c :: _) =>
      if c /= '-' then 1 else 0
    _ => 0

export
proven_idris_args_is_positional : String -> Int
proven_idris_args_is_positional arg =
  case unpack arg of
    ('-' :: _) => 0
    _ => 1

export
proven_idris_args_is_end_of_opts : String -> Int
proven_idris_args_is_end_of_opts arg =
  if arg == "--" then 1 else 0

export
proven_idris_args_has_equals : String -> Int
proven_idris_args_has_equals arg =
  encodeBool (isInfixOf "=" arg)

--------------------------------------------------------------------------------
-- Parser Options Constants
--------------------------------------------------------------------------------

export
proven_idris_args_default_allow_unknown : Int
proven_idris_args_default_allow_unknown = encodeBool defaultParserOptions.allowUnknown

export
proven_idris_args_default_stop_at_non_option : Int
proven_idris_args_default_stop_at_non_option = encodeBool defaultParserOptions.stopAtNonOption

export
proven_idris_args_default_allow_bundling : Int
proven_idris_args_default_allow_bundling = encodeBool defaultParserOptions.allowBundling

export
proven_idris_args_default_allow_equals : Int
proven_idris_args_default_allow_equals = encodeBool defaultParserOptions.allowEquals

export
proven_idris_args_default_case_sensitive : Int
proven_idris_args_default_case_sensitive = encodeBool defaultParserOptions.caseSensitive

export
proven_idris_args_default_max_arg_length : Int
proven_idris_args_default_max_arg_length = cast defaultParserOptions.maxArgLength

export
proven_idris_args_default_max_arg_count : Int
proven_idris_args_default_max_arg_count = cast defaultParserOptions.maxArgCount

export
proven_idris_args_strict_allow_unknown : Int
proven_idris_args_strict_allow_unknown = encodeBool strictParserOptions.allowUnknown

export
proven_idris_args_strict_allow_bundling : Int
proven_idris_args_strict_allow_bundling = encodeBool strictParserOptions.allowBundling

export
proven_idris_args_strict_max_arg_length : Int
proven_idris_args_strict_max_arg_length = cast strictParserOptions.maxArgLength

export
proven_idris_args_strict_max_arg_count : Int
proven_idris_args_strict_max_arg_count = cast strictParserOptions.maxArgCount

export
proven_idris_args_permissive_allow_unknown : Int
proven_idris_args_permissive_allow_unknown = encodeBool permissiveParserOptions.allowUnknown

export
proven_idris_args_permissive_stop_at_non_option : Int
proven_idris_args_permissive_stop_at_non_option = encodeBool permissiveParserOptions.stopAtNonOption

export
proven_idris_args_permissive_max_arg_length : Int
proven_idris_args_permissive_max_arg_length = cast permissiveParserOptions.maxArgLength

export
proven_idris_args_permissive_max_arg_count : Int
proven_idris_args_permissive_max_arg_count = cast permissiveParserOptions.maxArgCount

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

export
proven_idris_args_check_arg_length : Int -> String -> Int
proven_idris_args_check_arg_length maxLen arg =
  if length (unpack arg) <= (cast maxLen)
    then 1  -- Valid
    else 0  -- Too long

export
proven_idris_args_check_arg_count : Int -> Int -> Int
proven_idris_args_check_arg_count maxCount currentCount =
  if currentCount <= maxCount
    then 1  -- Valid
    else 0  -- Too many

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

export
proven_idris_args_is_unknown_option_error : String -> Int
proven_idris_args_is_unknown_option_error errorMsg =
  if isInfixOf "unknown option" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_args_is_missing_required_error : String -> Int
proven_idris_args_is_missing_required_error errorMsg =
  if isInfixOf "missing required" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_args_is_missing_value_error : String -> Int
proven_idris_args_is_missing_value_error errorMsg =
  if isInfixOf "requires a value" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_args_is_invalid_value_error : String -> Int
proven_idris_args_is_invalid_value_error errorMsg =
  if isInfixOf "invalid value" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_args_is_not_allowed_error : String -> Int
proven_idris_args_is_not_allowed_error errorMsg =
  if isInfixOf "not allowed" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_args_is_too_many_positional_error : String -> Int
proven_idris_args_is_too_many_positional_error errorMsg =
  if isInfixOf "too many positional" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_args_is_ambiguous_short_error : String -> Int
proven_idris_args_is_ambiguous_short_error errorMsg =
  if isInfixOf "ambiguous" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_args_is_type_conversion_error : String -> Int
proven_idris_args_is_type_conversion_error errorMsg =
  if isInfixOf "cannot convert" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_args_is_invalid_format_error : String -> Int
proven_idris_args_is_invalid_format_error errorMsg =
  if isInfixOf "invalid argument format" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Friendly Error Messages
--------------------------------------------------------------------------------

export
proven_idris_args_friendly_error : String -> String
proven_idris_args_friendly_error errorMsg =
  if isInfixOf "unknown option" (toLower errorMsg)
    then "Unrecognized command-line option"
  else if isInfixOf "missing required" (toLower errorMsg)
    then "Required argument not provided"
  else if isInfixOf "requires a value" (toLower errorMsg)
    then "Option requires a value but none was provided"
  else if isInfixOf "invalid value" (toLower errorMsg)
    then "Invalid value provided for option"
  else if isInfixOf "not allowed" (toLower errorMsg)
    then "Value not in allowed list for this option"
  else if isInfixOf "too many positional" (toLower errorMsg)
    then "Too many positional arguments provided"
  else if isInfixOf "too long" (toLower errorMsg)
    then "Argument exceeds maximum length (possible DoS attack)"
  else if isInfixOf "ambiguous" (toLower errorMsg)
    then "Ambiguous short option matches multiple arguments"
  else if isInfixOf "cannot convert" (toLower errorMsg)
    then "Cannot convert argument to requested type"
  else if isInfixOf "only flags can be bundled" (toLower errorMsg)
    then "Invalid bundled options (only boolean flags can be bundled)"
  else
    "Argument parsing error"

--------------------------------------------------------------------------------
-- String Utilities
--------------------------------------------------------------------------------

export
proven_idris_args_extract_long_name : String -> (Int, String)
proven_idris_args_extract_long_name arg =
  case unpack arg of
    ('-' :: '-' :: rest) =>
      case break (== '=') (pack rest) of
        (name, _) => (0, name)
    _ => (1, "not a long option")

export
proven_idris_args_extract_short_name : String -> (Int, String)
proven_idris_args_extract_short_name arg =
  case unpack arg of
    ('-' :: c :: _) =>
      if c /= '-' then (0, singleton c) else (1, "not a short option")
    _ => (1, "not a short option")

export
proven_idris_args_extract_equals_value : String -> (Int, String)
proven_idris_args_extract_equals_value arg =
  case break (== '=') arg of
    (_, val) =>
      if null (unpack val)
        then (1, "no equals sign found")
        else (0, drop 1 val)
