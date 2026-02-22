-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Safe JSON validation and type detection via libproven FFI.
|||
||| Provides JSON syntax validation and root-level type detection.
||| All logic is performed by the formally verified Idris 2 core
||| through the precompiled shared library.
|||
||| This module does NOT parse JSON into a tree structure. It validates
||| that a string is syntactically correct JSON and reports the top-level
||| value type (null, boolean, number, string, array, object).
module Proven.SafeJson

import Proven.FFI

%default total

-- ============================================================================
-- Re-export JsonType from FFI
-- ============================================================================

-- JsonType and intToJsonType are defined in Proven.FFI and re-exported
-- here for convenience.

-- ============================================================================
-- Helpers
-- ============================================================================

||| Interpret a C boolean result tuple as a Maybe Bool.
boolResultToMaybe : (Int, Int) -> Maybe Bool
boolResultToMaybe (s, v) =
  if isOK s then Just (v /= 0)
  else Nothing

-- ============================================================================
-- JSON validation
-- ============================================================================

||| Check whether a string is valid JSON.
|||
||| Returns `Just True` if the string is valid JSON, `Just False` if not,
||| or `Nothing` on error (e.g. null pointer).
||| @ str The JSON string to validate
public export
isValidJson : HasIO io => (str : String) -> io (Maybe Bool)
isValidJson str = do
  let len = cast {to=Int} (length str)
  result <- primIO $ prim__proven_json_is_valid str len
  pure (boolResultToMaybe result)

||| Check whether a string is valid JSON, returning a typed error.
public export
isValidJsonE : HasIO io => (str : String) -> io (Either ProvenError Bool)
isValidJsonE str = do
  let len = cast {to=Int} (length str)
  (status, boolVal) <- primIO $ prim__proven_json_is_valid str len
  case statusToError status of
    Nothing  => pure (Right (boolVal /= 0))
    Just err => pure (Left err)

-- ============================================================================
-- JSON type detection
-- ============================================================================

||| Get the root-level JSON value type.
|||
||| Inspects the first significant character of the JSON string to
||| determine whether the root value is null, boolean, number, string,
||| array, or object. Returns `JsonInvalid` if the string is not valid JSON.
||| @ str The JSON string to inspect
public export
getJsonType : HasIO io => (str : String) -> io JsonType
getJsonType str = do
  let len = cast {to=Int} (length str)
  typeCode <- primIO $ prim__proven_json_get_type str len
  pure (intToJsonType typeCode)

-- ============================================================================
-- Convenience predicates
-- ============================================================================

||| Check whether a JSON string contains an object at the root level.
public export
isJsonObject : HasIO io => (str : String) -> io Bool
isJsonObject str = do
  jt <- getJsonType str
  pure $ case jt of
    JsonObject => True
    _          => False

||| Check whether a JSON string contains an array at the root level.
public export
isJsonArray : HasIO io => (str : String) -> io Bool
isJsonArray str = do
  jt <- getJsonType str
  pure $ case jt of
    JsonArray => True
    _         => False
