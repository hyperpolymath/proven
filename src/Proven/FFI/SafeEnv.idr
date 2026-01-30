-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeEnv operations
|||
||| This module exports environment variable utilities to the C ABI via Idris2's RefC backend.
||| All functions are proven total and protect against information disclosure and memory exhaustion.
|||
||| Return conventions:
||| - Name validation → Int (0 = invalid, 1 = valid)
||| - Type parsers → (status: Int, result: String)
|||   - status = 0: Success, result contains parsed value
|||   - status = 1: Error, result contains error message
||| - Security classification → Int (0 = public, 1 = sensitive, 2 = secret)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Use strict defaults for untrusted environments. Sensitive variables blocked by default.
module Proven.FFI.SafeEnv

import Proven.SafeEnv
import Proven.SafeEnv.Types
import Proven.SafeEnv.Access
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

||| Encode Maybe String as (status, value)
encodeMaybeString : Maybe String -> (Int, String)
encodeMaybeString Nothing = (1, "")
encodeMaybeString (Just s) = (0, s)

||| Encode Maybe Int as (status, value)
encodeMaybeInt : Show a => Maybe a -> (Int, String)
encodeMaybeInt Nothing = (1, "parse failed")
encodeMaybeInt (Just x) = (0, show x)

||| Encode EnvSecurity as Int
encodeEnvSecurity : EnvSecurity -> Int
encodeEnvSecurity Public = 0
encodeEnvSecurity Sensitive = 1
encodeEnvSecurity Secret = 2

||| Encode EnvResult as (status, result/error)
encodeEnvResult : Show a => EnvResult a -> (Int, String)
encodeEnvResult (Err err) = (1, show err)
encodeEnvResult (Ok val) = (0, show val)

--------------------------------------------------------------------------------
-- Name Validation
--------------------------------------------------------------------------------

%export
proven_idris_env_is_valid_name : String -> Int
proven_idris_env_is_valid_name name = encodeBool (isValidEnvName name)

%export
proven_idris_env_is_well_known : String -> Int
proven_idris_env_is_well_known name = encodeBool (isWellKnown name)

%export
proven_idris_env_is_sensitive_name : String -> Int
proven_idris_env_is_sensitive_name name = encodeBool (isSensitiveName name)

%export
proven_idris_env_classify_by_name : String -> Int
proven_idris_env_classify_by_name name = encodeEnvSecurity (classifyByName name)

--------------------------------------------------------------------------------
-- Type Parsers
--------------------------------------------------------------------------------

%export
proven_idris_env_parse_bool : String -> (Int, String)
proven_idris_env_parse_bool s =
  case parseBool s of
    Nothing => (1, "invalid boolean value")
    Just True => (0, "true")
    Just False => (0, "false")

%export
proven_idris_env_parse_int : String -> (Int, String)
proven_idris_env_parse_int s = encodeMaybeInt (parseInt s)

%export
proven_idris_env_parse_nat : String -> (Int, String)
proven_idris_env_parse_nat s = encodeMaybeInt (parseNat s)

%export
proven_idris_env_parse_port : String -> (Int, String)
proven_idris_env_parse_port s = encodeMaybeInt (parsePort s)

%export
proven_idris_env_parse_list : String -> String
proven_idris_env_parse_list s = joinWith "," (parseList s)
  where
    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

%export
proven_idris_env_parse_key_value : String -> (Int, String)
proven_idris_env_parse_key_value s =
  case parseKeyValue s of
    Nothing => (1, "invalid key=value format")
    Just (key, value) => (0, key ++ "=" ++ value)

--------------------------------------------------------------------------------
-- Options Constants
--------------------------------------------------------------------------------

%export
proven_idris_env_default_max_value_length : Int
proven_idris_env_default_max_value_length = cast defaultMaxValueLength

%export
proven_idris_env_default_allow_sensitive : Int
proven_idris_env_default_allow_sensitive = encodeBool defaultOptions.allowSensitive

%export
proven_idris_env_default_require_uppercase : Int
proven_idris_env_default_require_uppercase = encodeBool defaultOptions.requireUppercase

%export
proven_idris_env_strict_max_value_length : Int
proven_idris_env_strict_max_value_length = cast strictOptions.maxValueLength

%export
proven_idris_env_strict_allow_sensitive : Int
proven_idris_env_strict_allow_sensitive = encodeBool strictOptions.allowSensitive

%export
proven_idris_env_strict_require_uppercase : Int
proven_idris_env_strict_require_uppercase = encodeBool strictOptions.requireUppercase

%export
proven_idris_env_permissive_max_value_length : Int
proven_idris_env_permissive_max_value_length = cast permissiveOptions.maxValueLength

%export
proven_idris_env_permissive_allow_sensitive : Int
proven_idris_env_permissive_allow_sensitive = encodeBool permissiveOptions.allowSensitive

--------------------------------------------------------------------------------
-- Security/Redaction
--------------------------------------------------------------------------------

%export
proven_idris_env_redact_value : Int -> String -> String
proven_idris_env_redact_value secInt value =
  case secInt of
    0 => redactValue Public value      -- Public
    1 => redactValue Sensitive value   -- Sensitive
    2 => redactValue Secret value      -- Secret
    _ => "***INVALID_SECURITY_LEVEL***"

%export
proven_idris_env_mask_value : String -> String
proven_idris_env_mask_value value = maskValue value

%export
proven_idris_env_to_loggable : String -> String -> String
proven_idris_env_to_loggable name value = toLoggable name value

--------------------------------------------------------------------------------
-- Well-Known Variables Count
--------------------------------------------------------------------------------

%export
proven_idris_env_well_known_count : Int
proven_idris_env_well_known_count = cast (length wellKnownVars)

%export
proven_idris_env_sensitive_patterns_count : Int
proven_idris_env_sensitive_patterns_count = cast (length sensitivePatterns)

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

%export
proven_idris_env_is_not_found_error : String -> Int
proven_idris_env_is_not_found_error errorMsg =
  if isInfixOf "not found" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_env_is_invalid_name_error : String -> Int
proven_idris_env_is_invalid_name_error errorMsg =
  if isInfixOf "invalid" (toLower errorMsg) && isInfixOf "name" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_env_is_value_too_long_error : String -> Int
proven_idris_env_is_value_too_long_error errorMsg =
  if isInfixOf "too long" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_env_is_type_conversion_error : String -> Int
proven_idris_env_is_type_conversion_error errorMsg =
  if isInfixOf "cannot convert" (toLower errorMsg) || isInfixOf "conversion" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_env_is_sensitive_variable_error : String -> Int
proven_idris_env_is_sensitive_variable_error errorMsg =
  if isInfixOf "sensitive" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_env_is_access_denied_error : String -> Int
proven_idris_env_is_access_denied_error errorMsg =
  if isInfixOf "access denied" (toLower errorMsg) || isInfixOf "blocked" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Friendly Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_env_friendly_error : String -> String
proven_idris_env_friendly_error errorMsg =
  if isInfixOf "not found" (toLower errorMsg)
    then "Environment variable not set"
  else if isInfixOf "sensitive" (toLower errorMsg)
    then "Variable contains sensitive data (access denied by policy)"
  else if isInfixOf "too long" (toLower errorMsg)
    then "Environment variable value exceeds maximum length (possible memory exhaustion)"
  else if isInfixOf "invalid" (toLower errorMsg) && isInfixOf "name" (toLower errorMsg)
    then "Environment variable name contains invalid characters"
  else if isInfixOf "cannot convert" (toLower errorMsg)
    then "Cannot convert environment variable to requested type"
  else if isInfixOf "access denied" (toLower errorMsg)
    then "Access to environment variable denied by security policy"
  else if isInfixOf "blocked pattern" (toLower errorMsg)
    then "Environment variable matches blocked pattern"
  else
    "Environment variable access error"
