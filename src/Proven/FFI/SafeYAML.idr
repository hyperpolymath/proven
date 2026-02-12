-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeYAML operations
|||
||| This module exports YAML processing with deserialization attack prevention to the C ABI via Idris2's RefC backend.
||| All functions are proven total and protect against alias bombs and dangerous tags.
|||
||| Return conventions:
||| - YAML parsing → (status: Int, result/error: String)
|||   - status = 0: Success, result is JSON-like representation
|||   - status = 1: Error, result contains error message
||| - Type coercion → (status: Int, value/error: String)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Use secure defaults for untrusted YAML. Anchors disabled, dangerous tags blocked.
module Proven.FFI.SafeYAML

import Proven.SafeYAML
import Proven.SafeYAML.Types
import Proven.SafeYAML.Parser
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode YAMLResult as (status, result/error)
encodeYAMLResult : {a : Type} -> Show a => YAMLResult a -> (Int, String)
encodeYAMLResult (Err err) = (1, show err)
encodeYAMLResult (Ok val) = (0, show val)

||| Encode Maybe String as (status, value)
encodeMaybeString : Maybe String -> (Int, String)
encodeMaybeString Nothing = (1, "")
encodeMaybeString (Just s) = (0, s)

--------------------------------------------------------------------------------
-- YAML Parsing
--------------------------------------------------------------------------------

export
proven_idris_yaml_parse : String -> (Int, String)
proven_idris_yaml_parse s = encodeYAMLResult (parse s)

export
proven_idris_yaml_is_valid : String -> Int
proven_idris_yaml_is_valid s = encodeBool (isSafe s)

export
proven_idris_yaml_validate : String -> (Int, String)
proven_idris_yaml_validate s =
  case validate s of
    Err err => (1, show err)
    Ok () => (0, "")

export
proven_idris_yaml_has_dangerous_patterns : String -> Int
proven_idris_yaml_has_dangerous_patterns s = encodeBool (hasDangerousPatterns s)

export
proven_idris_yaml_has_anchors : String -> Int
proven_idris_yaml_has_anchors s = encodeBool (hasAnchors s)

--------------------------------------------------------------------------------
-- Type Coercion
--------------------------------------------------------------------------------

export
proven_idris_yaml_as_string : String -> (Int, String)
proven_idris_yaml_as_string s =
  case parse s of
    Err err => (1, show err)
    Ok val => case asString val of
      Err err => (1, show err)
      Ok str => (0, str)

export
proven_idris_yaml_as_int : String -> (Int, String)
proven_idris_yaml_as_int s =
  case parse s of
    Err err => (1, show err)
    Ok val => case asInt val of
      Err err => (1, show err)
      Ok i => (0, show i)

export
proven_idris_yaml_as_float : String -> (Int, String)
proven_idris_yaml_as_float s =
  case parse s of
    Err err => (1, show err)
    Ok val => case asFloat val of
      Err err => (1, show err)
      Ok f => (0, show f)

export
proven_idris_yaml_as_bool : String -> (Int, String)
proven_idris_yaml_as_bool s =
  case parse s of
    Err err => (1, show err)
    Ok val => case asBool val of
      Err err => (1, show err)
      Ok True => (0, "true")
      Ok False => (0, "false")

--------------------------------------------------------------------------------
-- Object Access
--------------------------------------------------------------------------------

export
proven_idris_yaml_get_field : String -> String -> (Int, String)
proven_idris_yaml_get_field yamlStr fieldName =
  case parse yamlStr of
    Err err => (1, show err)
    Ok val => case getField fieldName val of
      Err err => (1, show err)
      Ok fieldVal => (0, show fieldVal)

export
proven_idris_yaml_has_field : String -> String -> Int
proven_idris_yaml_has_field yamlStr fieldName =
  case parse yamlStr of
    Err _ => 0
    Ok val => encodeBool (hasField fieldName val)

export
proven_idris_yaml_get_path : String -> String -> (Int, String)
proven_idris_yaml_get_path yamlStr path =
  case parse yamlStr of
    Err err => (1, show err)
    Ok val => case getPath path val of
      Err err => (1, show err)
      Ok result => (0, show result)

export
proven_idris_yaml_keys : String -> (Int, String)
proven_idris_yaml_keys yamlStr =
  case parse yamlStr of
    Err err => (1, show err)
    Ok val => (0, joinWith "," (keys val))
  where
    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

--------------------------------------------------------------------------------
-- Array Access
--------------------------------------------------------------------------------

export
proven_idris_yaml_get_index : String -> Int -> (Int, String)
proven_idris_yaml_get_index yamlStr idx =
  case parse yamlStr of
    Err err => (1, show err)
    Ok val => case getIndex (cast idx) val of
      Err err => (1, show err)
      Ok result => (0, show result)

export
proven_idris_yaml_array_length : String -> (Int, String)
proven_idris_yaml_array_length yamlStr =
  case parse yamlStr of
    Err err => (1, show err)
    Ok val => (0, show (arrayLength val))

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

export
proven_idris_yaml_render : String -> (Int, String)
proven_idris_yaml_render yamlStr =
  case parse yamlStr of
    Err err => (1, show err)
    Ok val => (0, render val)

--------------------------------------------------------------------------------
-- Security Options Constants
--------------------------------------------------------------------------------

export
proven_idris_yaml_secure_max_documents : Int
proven_idris_yaml_secure_max_documents = cast secureDefaults.maxDocuments

export
proven_idris_yaml_secure_max_depth : Int
proven_idris_yaml_secure_max_depth = cast secureDefaults.maxDepth

export
proven_idris_yaml_secure_max_key_length : Int
proven_idris_yaml_secure_max_key_length = cast secureDefaults.maxKeyLength

export
proven_idris_yaml_secure_max_value_size : Int
proven_idris_yaml_secure_max_value_size = cast secureDefaults.maxValueSize

export
proven_idris_yaml_secure_max_alias_depth : Int
proven_idris_yaml_secure_max_alias_depth = cast secureDefaults.maxAliasDepth

export
proven_idris_yaml_secure_allow_anchors : Int
proven_idris_yaml_secure_allow_anchors = encodeBool secureDefaults.allowAnchors

export
proven_idris_yaml_secure_allow_binary : Int
proven_idris_yaml_secure_allow_binary = encodeBool secureDefaults.allowBinary

export
proven_idris_yaml_secure_allow_custom_tags : Int
proven_idris_yaml_secure_allow_custom_tags = encodeBool secureDefaults.allowCustomTags

--------------------------------------------------------------------------------
-- Dangerous Tag Detection
--------------------------------------------------------------------------------

export
proven_idris_yaml_is_dangerous_tag : String -> Int
proven_idris_yaml_is_dangerous_tag tag = encodeBool (isDangerousTag tag)

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

export
proven_idris_yaml_is_syntax_error : String -> Int
proven_idris_yaml_is_syntax_error errorMsg =
  if isInfixOf "syntax" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_yaml_is_alias_error : String -> Int
proven_idris_yaml_is_alias_error errorMsg =
  if isInfixOf "alias" (toLower errorMsg) || isInfixOf "anchor" (toLower errorMsg) || isInfixOf "circular" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_yaml_is_dangerous_tag_error : String -> Int
proven_idris_yaml_is_dangerous_tag_error errorMsg =
  if isInfixOf "dangerous" (toLower errorMsg) || isInfixOf "tag" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_yaml_is_limit_error : String -> Int
proven_idris_yaml_is_limit_error errorMsg =
  if isInfixOf "exceeded" (toLower errorMsg) || isInfixOf "too" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_yaml_is_type_error : String -> Int
proven_idris_yaml_is_type_error errorMsg =
  if isInfixOf "type" (toLower errorMsg) || isInfixOf "mismatch" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Friendly Error Messages
--------------------------------------------------------------------------------

export
proven_idris_yaml_friendly_error : String -> String
proven_idris_yaml_friendly_error errorMsg =
  if isInfixOf "dangerous" (toLower errorMsg)
    then "YAML contains dangerous tags (deserialization attack detected)"
  else if isInfixOf "alias" (toLower errorMsg) || isInfixOf "circular" (toLower errorMsg)
    then "YAML contains circular references or alias bombs"
  else if isInfixOf "depth" (toLower errorMsg)
    then "YAML nesting too deep"
  else if isInfixOf "too many documents" (toLower errorMsg)
    then "YAML stream contains too many documents"
  else
    "YAML processing error"
