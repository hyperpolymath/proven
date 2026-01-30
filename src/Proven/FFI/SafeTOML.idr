-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeTOML operations
|||
||| This module exports TOML processing with resource limits to the C ABI via Idris2's RefC backend.
||| All functions are proven total and enforce depth/size limits.
|||
||| Return conventions:
||| - TOML parsing → (status: Int, result/error: String)
|||   - status = 0: Success, result is JSON-like representation
|||   - status = 1: Error, result contains error message
||| - Type coercion → (status: Int, value/error: String)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Use secure defaults for untrusted TOML. Enforce depth and size limits.
module Proven.FFI.SafeTOML

import Proven.SafeTOML
import Proven.SafeTOML.Types
import Proven.SafeTOML.Parser
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode TOMLResult as (status, result/error)
encodeTOMLResult : {a : Type} -> Show a => TOMLResult a -> (Int, String)
encodeTOMLResult (Err err) = (1, show err)
encodeTOMLResult (Ok val) = (0, show val)

||| Encode Maybe String as (status, value)
encodeMaybeString : Maybe String -> (Int, String)
encodeMaybeString Nothing = (1, "")
encodeMaybeString (Just s) = (0, s)

--------------------------------------------------------------------------------
-- TOML Parsing
--------------------------------------------------------------------------------

%export
proven_idris_toml_parse : String -> (Int, String)
proven_idris_toml_parse s = encodeTOMLResult (parse s)

%export
proven_idris_toml_render : String -> (Int, String)
proven_idris_toml_render s =
  case parse s of
    Err err => (1, show err)
    Ok doc => (0, render doc)

--------------------------------------------------------------------------------
-- Type Coercion
--------------------------------------------------------------------------------

%export
proven_idris_toml_as_string : String -> String -> (Int, String)
proven_idris_toml_as_string tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getField key doc of
      Err err => (1, show err)
      Ok val => case asString val of
        Err err => (1, show err)
        Ok s => (0, s)

%export
proven_idris_toml_as_int : String -> String -> (Int, String)
proven_idris_toml_as_int tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getField key doc of
      Err err => (1, show err)
      Ok val => case asInt val of
        Err err => (1, show err)
        Ok i => (0, show i)

%export
proven_idris_toml_as_float : String -> String -> (Int, String)
proven_idris_toml_as_float tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getField key doc of
      Err err => (1, show err)
      Ok val => case asFloat val of
        Err err => (1, show err)
        Ok f => (0, show f)

%export
proven_idris_toml_as_bool : String -> String -> (Int, String)
proven_idris_toml_as_bool tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getField key doc of
      Err err => (1, show err)
      Ok val => case asBool val of
        Err err => (1, show err)
        Ok True => (0, "true")
        Ok False => (0, "false")

--------------------------------------------------------------------------------
-- Document/Table Access
--------------------------------------------------------------------------------

%export
proven_idris_toml_get_field : String -> String -> (Int, String)
proven_idris_toml_get_field tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getField key doc of
      Err err => (1, show err)
      Ok val => (0, show val)

%export
proven_idris_toml_has_field : String -> String -> Int
proven_idris_toml_has_field tomlStr key =
  case parse tomlStr of
    Err _ => 0
    Ok doc => encodeBool (hasField key doc)

%export
proven_idris_toml_get_path : String -> String -> (Int, String)
proven_idris_toml_get_path tomlStr path =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getPath path doc of
      Err err => (1, show err)
      Ok val => (0, show val)

%export
proven_idris_toml_keys : String -> (Int, String)
proven_idris_toml_keys tomlStr =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => (0, joinWith "," (keys doc))
  where
    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

--------------------------------------------------------------------------------
-- Typed Field Access
--------------------------------------------------------------------------------

%export
proven_idris_toml_get_string : String -> String -> (Int, String)
proven_idris_toml_get_string tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getString key doc of
      Err err => (1, show err)
      Ok s => (0, s)

%export
proven_idris_toml_get_int : String -> String -> (Int, String)
proven_idris_toml_get_int tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getInt key doc of
      Err err => (1, show err)
      Ok i => (0, show i)

%export
proven_idris_toml_get_float : String -> String -> (Int, String)
proven_idris_toml_get_float tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getFloat key doc of
      Err err => (1, show err)
      Ok f => (0, show f)

%export
proven_idris_toml_get_bool : String -> String -> (Int, String)
proven_idris_toml_get_bool tomlStr key =
  case parse tomlStr of
    Err err => (1, show err)
    Ok doc => case getBool key doc of
      Err err => (1, show err)
      Ok True => (0, "true")
      Ok False => (0, "false")

--------------------------------------------------------------------------------
-- Security Options Constants
--------------------------------------------------------------------------------

%export
proven_idris_toml_secure_max_depth : Int
proven_idris_toml_secure_max_depth = cast secureDefaults.maxDepth

%export
proven_idris_toml_secure_max_key_length : Int
proven_idris_toml_secure_max_key_length = cast secureDefaults.maxKeyLength

%export
proven_idris_toml_secure_max_value_size : Int
proven_idris_toml_secure_max_value_size = cast secureDefaults.maxValueSize

%export
proven_idris_toml_secure_max_total_keys : Int
proven_idris_toml_secure_max_total_keys = cast secureDefaults.maxTotalKeys

%export
proven_idris_toml_secure_max_array_length : Int
proven_idris_toml_secure_max_array_length = cast secureDefaults.maxArrayLength

%export
proven_idris_toml_secure_allow_heterogeneous : Int
proven_idris_toml_secure_allow_heterogeneous = encodeBool secureDefaults.allowHeterogeneousArrays

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

%export
proven_idris_toml_is_syntax_error : String -> Int
proven_idris_toml_is_syntax_error errorMsg =
  if isInfixOf "syntax" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_toml_is_duplicate_key_error : String -> Int
proven_idris_toml_is_duplicate_key_error errorMsg =
  if isInfixOf "duplicate" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_toml_is_nesting_error : String -> Int
proven_idris_toml_is_nesting_error errorMsg =
  if isInfixOf "nesting" (toLower errorMsg) || isInfixOf "depth" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_toml_is_limit_error : String -> Int
proven_idris_toml_is_limit_error errorMsg =
  if isInfixOf "too long" (toLower errorMsg) || isInfixOf "too large" (toLower errorMsg) || isInfixOf "too many" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_toml_is_type_error : String -> Int
proven_idris_toml_is_type_error errorMsg =
  if isInfixOf "type" (toLower errorMsg) || isInfixOf "mismatch" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_toml_is_datetime_error : String -> Int
proven_idris_toml_is_datetime_error errorMsg =
  if isInfixOf "datetime" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Friendly Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_toml_friendly_error : String -> String
proven_idris_toml_friendly_error errorMsg =
  if isInfixOf "nesting" (toLower errorMsg) || isInfixOf "depth" (toLower errorMsg)
    then "TOML nesting too deep (possible resource exhaustion attack)"
  else if isInfixOf "too many keys" (toLower errorMsg)
    then "TOML contains too many keys (possible resource exhaustion)"
  else if isInfixOf "duplicate" (toLower errorMsg)
    then "TOML contains duplicate keys"
  else if isInfixOf "heterogeneous" (toLower errorMsg)
    then "TOML array contains mixed types (TOML 1.0 requires homogeneous arrays)"
  else
    "TOML processing error"
