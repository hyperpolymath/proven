-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeHex operations
|||
||| This module exports hexadecimal encoding/decoding to the C ABI via Idris2's RefC backend.
||| All functions are proven total and validate hex format.
|||
||| Return conventions:
||| - Hex decoding → (status: Int, result: String)
|||   - status = 0: Success, result contains comma-separated bytes
|||   - status = 1: Invalid hex format
||| - Hex encoding → String (always succeeds)
||| - Integer conversion → (status: Int, result: String)
||| - Validation → Int (0 = invalid, 1 = valid)
|||
||| CRITICAL: Hex strings must have even length. Use padHex for odd-length inputs.
module Proven.FFI.SafeHex

import Proven.SafeHex
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

||| Encode Maybe (List Nat) as (status, csv)
encodeMaybeBytes : Maybe (List Nat) -> (Int, String)
encodeMaybeBytes Nothing = (1, "invalid hex format")
encodeMaybeBytes (Just bytes) = (0, joinWith "," (map show bytes))
  where
    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

||| Decode comma-separated bytes to List Nat
decodeBytes : String -> Maybe (List Nat)
decodeBytes s =
  if s == "" then Just []
  else traverse parseNat (split (== ',') s)
  where
    parseNat : String -> Maybe Nat
    parseNat str =
      case parseInteger str of
        Just i => if i >= 0 then Just (cast i) else Nothing
        Nothing => Nothing

--------------------------------------------------------------------------------
-- Hex Validation
--------------------------------------------------------------------------------

%export
proven_idris_hex_is_valid : String -> Int
proven_idris_hex_is_valid s = encodeBool (isValidHex s)

%export
proven_idris_hex_is_even_length : String -> Int
proven_idris_hex_is_even_length s =
  encodeBool (length (unpack s) `mod` 2 == 0)

--------------------------------------------------------------------------------
-- Hex Encoding
--------------------------------------------------------------------------------

%export
proven_idris_hex_encode : String -> String
proven_idris_hex_encode bytesStr =
  case decodeBytes bytesStr of
    Nothing => ""
    Just bytes => encode bytes

%export
proven_idris_hex_encode_upper : String -> String
proven_idris_hex_encode_upper bytesStr =
  case decodeBytes bytesStr of
    Nothing => ""
    Just bytes => encodeUpper bytes

%export
proven_idris_hex_byte_to_hex : Int -> String
proven_idris_hex_byte_to_hex n = byteToHex (cast n)

%export
proven_idris_hex_nibble_to_hex : Int -> String
proven_idris_hex_nibble_to_hex n = singleton (nibbleToHex (cast n))

--------------------------------------------------------------------------------
-- Hex Decoding
--------------------------------------------------------------------------------

%export
proven_idris_hex_decode : String -> (Int, String)
proven_idris_hex_decode s = encodeMaybeBytes (decode s)

%export
proven_idris_hex_hex_to_nibble : String -> (Int, String)
proven_idris_hex_hex_to_nibble s =
  case unpack s of
    (c :: _) =>
      case hexToNibble c of
        Nothing => (1, "invalid hex character")
        Just n => (0, show n)
    [] => (1, "empty string")

--------------------------------------------------------------------------------
-- Integer Conversion
--------------------------------------------------------------------------------

%export
proven_idris_hex_int_to_hex : Int -> String
proven_idris_hex_int_to_hex n = intToHex (cast n)

%export
proven_idris_hex_int_to_hex_upper : Int -> String
proven_idris_hex_int_to_hex_upper n = toUpper (intToHex (cast n))

%export
proven_idris_hex_hex_to_int : String -> (Int, String)
proven_idris_hex_hex_to_int s =
  case hexToInt s of
    Nothing => (1, "invalid hex format")
    Just i => (0, show i)

--------------------------------------------------------------------------------
-- Hex Utilities
--------------------------------------------------------------------------------

%export
proven_idris_hex_pad : Int -> String -> String
proven_idris_hex_pad len s = padHex (cast len) s

%export
proven_idris_hex_has_prefix : String -> Int
proven_idris_hex_has_prefix s =
  encodeBool (isPrefixOf "0x" s || isPrefixOf "0X" s)

%export
proven_idris_hex_strip_prefix : String -> String
proven_idris_hex_strip_prefix s =
  if isPrefixOf "0x" s || isPrefixOf "0X" s
    then drop 2 s
    else s

%export
proven_idris_hex_add_prefix : String -> String
proven_idris_hex_add_prefix s =
  if isPrefixOf "0x" s || isPrefixOf "0X" s
    then s
    else "0x" ++ s

%export
proven_idris_hex_to_lower : String -> String
proven_idris_hex_to_lower s = toLower s

%export
proven_idris_hex_to_upper : String -> String
proven_idris_hex_to_upper s = toUpper s

--------------------------------------------------------------------------------
-- Format Detection
--------------------------------------------------------------------------------

%export
proven_idris_hex_is_lowercase : String -> Int
proven_idris_hex_is_lowercase s =
  encodeBool (s == toLower s)

%export
proven_idris_hex_is_uppercase : String -> Int
proven_idris_hex_is_uppercase s =
  encodeBool (s == toUpper s)

%export
proven_idris_hex_byte_count : String -> (Int, String)
proven_idris_hex_byte_count s =
  if isValidHex s
    then (0, show (length (unpack s) `div` 2))
    else (1, "invalid hex")

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_hex_friendly_error : String -> String
proven_idris_hex_friendly_error errorMsg =
  if isInfixOf "invalid hex" (toLower errorMsg)
    then "Invalid hexadecimal format (must contain only 0-9, a-f, A-F)"
  else if isInfixOf "odd" (toLower errorMsg) || isInfixOf "length" (toLower errorMsg)
    then "Hex string must have even length (each byte = 2 hex digits)"
  else if isInfixOf "empty" (toLower errorMsg)
    then "Empty hex string"
  else
    "Hexadecimal processing error"
