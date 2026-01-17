-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe JSON parsing and generation.
-- |
-- | Provides safe JSON operations with proper error handling and
-- | protection against malformed input.

module Proven.SafeJson
  ( SafeJson
  , JsonValue(..)
  , parse
  , stringify
  , isValidJson
  , getField
  , getArrayItem
  , asString
  , asNumber
  , asBoolean
  , asArray
  , asObject
  ) where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Proven.Result (Result(..), ProvenError(..))

-- | SafeJson namespace marker (not instantiated).
data SafeJson

-- | JSON value representation.
data JsonValue
  = JsonNull
  | JsonBool Boolean
  | JsonNumber Number
  | JsonString String
  | JsonArray (Array JsonValue)
  | JsonObject (Object JsonValue)

derive instance eqJsonValue :: Eq JsonValue

instance showJsonValue :: Show JsonValue where
  show JsonNull = "null"
  show (JsonBool b) = show b
  show (JsonNumber n) = show n
  show (JsonString s) = show s
  show (JsonArray arr) = show arr
  show (JsonObject obj) = show obj

-- | Parse a JSON string safely.
parse :: String -> Result JsonValue ProvenError
parse s =
  let result = parseJsonImpl s
  in if result.valid
     then Ok (fromForeign result.value)
     else Err (InvalidJson result.error)

foreign import parseJsonImpl :: String ->
  { valid :: Boolean
  , value :: Foreign
  , error :: String
  }

foreign import data Foreign :: Type

foreign import fromForeign :: Foreign -> JsonValue

-- | Convert a JSON value to a string.
stringify :: JsonValue -> String
stringify = stringifyImpl

foreign import stringifyImpl :: JsonValue -> String

-- | Check if a string is valid JSON.
isValidJson :: String -> Boolean
isValidJson s = (parseJsonImpl s).valid

-- | Get a field from a JSON object.
getField :: String -> JsonValue -> Maybe JsonValue
getField key (JsonObject obj) = Object.lookup key obj
getField _ _ = Nothing

-- | Get an item from a JSON array by index.
getArrayItem :: Int -> JsonValue -> Maybe JsonValue
getArrayItem idx (JsonArray arr) = index arr idx
getArrayItem _ _ = Nothing

-- | Try to extract a string value.
asString :: JsonValue -> Maybe String
asString (JsonString s) = Just s
asString _ = Nothing

-- | Try to extract a number value.
asNumber :: JsonValue -> Maybe Number
asNumber (JsonNumber n) = Just n
asNumber _ = Nothing

-- | Try to extract a boolean value.
asBoolean :: JsonValue -> Maybe Boolean
asBoolean (JsonBool b) = Just b
asBoolean _ = Nothing

-- | Try to extract an array value.
asArray :: JsonValue -> Maybe (Array JsonValue)
asArray (JsonArray arr) = Just arr
asArray _ = Nothing

-- | Try to extract an object value.
asObject :: JsonValue -> Maybe (Object JsonValue)
asObject (JsonObject obj) = Just obj
asObject _ = Nothing
