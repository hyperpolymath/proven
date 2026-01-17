-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe hexadecimal encoding and decoding.
-- |
-- | Provides validated hex string operations with proper error handling
-- | for invalid input.

module Proven.SafeHex
  ( SafeHex
  , encode
  , decode
  , isValidHex
  , encodeBytes
  , decodeBytes
  , requireValidHex
  ) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.String (toUpper, toLower)
import Data.String as S
import Proven.Result (Result(..), ProvenError(..))

-- | SafeHex namespace marker (not instantiated).
data SafeHex

-- | Check if a string contains only valid hex characters.
-- | Valid characters: 0-9, a-f, A-F
isValidHex :: String -> Boolean
isValidHex hex =
  S.length hex `mod` 2 == 0 && isValidHexImpl hex

foreign import isValidHexImpl :: String -> Boolean

-- | Encode a string to hexadecimal.
-- | Each character is converted to its hex representation.
encode :: String -> String
encode s = encodeImpl s

foreign import encodeImpl :: String -> String

-- | Decode a hexadecimal string.
-- | Returns error for invalid hex input.
decode :: String -> Result String ProvenError
decode hex
  | not (isValidHex hex) = Err InvalidHex
  | otherwise =
      let result = decodeImpl hex
      in if result.valid
         then Ok result.value
         else Err InvalidHex

foreign import decodeImpl :: String -> { valid :: Boolean, value :: String }

-- | Encode bytes (array of ints 0-255) to hex string.
encodeBytes :: Array Int -> String
encodeBytes bytes = encodeBytesImpl bytes

foreign import encodeBytesImpl :: Array Int -> String

-- | Decode hex string to bytes (array of ints 0-255).
decodeBytes :: String -> Result (Array Int) ProvenError
decodeBytes hex
  | not (isValidHex hex) = Err InvalidHex
  | otherwise =
      let result = decodeBytesImpl hex
      in if result.valid
         then Ok result.bytes
         else Err InvalidHex

foreign import decodeBytesImpl :: String -> { valid :: Boolean, bytes :: Array Int }

-- | Require a valid hex string or return error.
requireValidHex :: String -> Result String ProvenError
requireValidHex hex
  | isValidHex hex = Ok (toLower hex)
  | otherwise = Err InvalidHex
