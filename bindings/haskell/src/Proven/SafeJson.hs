{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe JSON operations via libproven FFI.
--
-- JSON validation is performed by the Idris 2 verified core.
module Proven.SafeJson
  ( JsonValueType(..)
  , isValidJson
  , getJsonType
  ) where

import Foreign.C.Types (CInt)
import Proven.FFI (c_proven_json_is_valid, c_proven_json_get_type)
import Proven.FFI.Types (JsonType(..))
import Proven.Core (withCStringLen', boolResultToBool)

-- | JSON value type at root level.
data JsonValueType
  = JNull
  | JBool
  | JNumber
  | JString
  | JArray
  | JObject
  | JInvalid
  deriving (Eq, Show)

-- | Check if a string is valid JSON.
-- Delegates to @proven_json_is_valid@ in libproven.
isValidJson :: String -> IO (Maybe Bool)
isValidJson str = withCStringLen' str $ \ptr len ->
  boolResultToBool <$> c_proven_json_is_valid ptr len

-- | Get the JSON value type at the root level.
-- Delegates to @proven_json_get_type@ in libproven.
getJsonType :: String -> IO JsonValueType
getJsonType str = withCStringLen' str $ \ptr len -> do
  jt <- c_proven_json_get_type ptr len
  return (fromJsonType jt)

-- | Convert FFI JsonType to Haskell JsonValueType.
fromJsonType :: JsonType -> JsonValueType
fromJsonType (JsonType 0)    = JNull
fromJsonType (JsonType 1)    = JBool
fromJsonType (JsonType 2)    = JNumber
fromJsonType (JsonType 3)    = JString
fromJsonType (JsonType 4)    = JArray
fromJsonType (JsonType 5)    = JObject
fromJsonType (JsonType (-1)) = JInvalid
fromJsonType _               = JInvalid
