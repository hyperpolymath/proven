-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeString operations
|||
||| This module exports safe string operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and handle edge cases (out of bounds, encoding errors).
|||
||| Return conventions:
||| - Maybe Char → (status: Int, charCode: Int)
|||   - status = 0: Success, charCode is valid
|||   - status = 1: Error (out of bounds, empty string)
||| - Maybe String → (status: Int, value: String)
|||   - status = 0: Success, value is valid
|||   - status = 1: Error (invalid indices, etc.)
||| - Bool → Int (0 = false, 1 = true)
module Proven.FFI.SafeString

import Proven.SafeString
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Maybe Char as (status, charCode) tuple for FFI
encodeCharResult : Maybe Char -> (Int, Int)
encodeCharResult Nothing = (1, 0)
encodeCharResult (Just c) = (0, ord c)

||| Encode Maybe String as (status, value) tuple for FFI
encodeStringResult : Maybe String -> (Int, String)
encodeStringResult Nothing = (1, "")
encodeStringResult (Just s) = (0, s)

||| Encode Bool as Int for FFI
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Decode Int to Char for FFI input
decodeChar : Int -> Char
decodeChar code = chr code

||| Decode Int to Nat for FFI input (clamp negative to 0)
decodeNat : Int -> Nat
decodeNat n = if n < 0 then Z else fromInteger (cast n)

--------------------------------------------------------------------------------
-- Safe String Access
--------------------------------------------------------------------------------

export
proven_idris_string_char_at : String -> Int -> (Int, Int)
proven_idris_string_char_at s idx =
  encodeCharResult (charAt s (decodeNat idx))

export
proven_idris_string_substring : Int -> Int -> String -> (Int, String)
proven_idris_string_substring start len s =
  encodeStringResult (substring (decodeNat start) (decodeNat len) s)

export
proven_idris_string_head_char : String -> (Int, Int)
proven_idris_string_head_char s =
  encodeCharResult (headChar s)

export
proven_idris_string_tail : String -> (Int, String)
proven_idris_string_tail s =
  encodeStringResult (tailStr s)

export
proven_idris_string_last_char : String -> (Int, Int)
proven_idris_string_last_char s =
  encodeCharResult (lastChar s)

--------------------------------------------------------------------------------
-- String Predicates
--------------------------------------------------------------------------------

export
proven_idris_string_is_empty : String -> Int
proven_idris_string_is_empty s =
  encodeBool (isEmpty s)

export
proven_idris_string_is_not_empty : String -> Int
proven_idris_string_is_not_empty s =
  encodeBool (isNotEmpty s)

export
proven_idris_string_is_blank : String -> Int
proven_idris_string_is_blank s =
  encodeBool (isBlank s)

export
proven_idris_string_contains : String -> String -> Int
proven_idris_string_contains needle haystack =
  encodeBool (contains needle haystack)

export
proven_idris_string_starts_with : String -> String -> Int
proven_idris_string_starts_with prefix s =
  encodeBool (startsWith prefix s)

export
proven_idris_string_ends_with : String -> String -> Int
proven_idris_string_ends_with suffix s =
  encodeBool (endsWith suffix s)

--------------------------------------------------------------------------------
-- String Transformation
--------------------------------------------------------------------------------

export
proven_idris_string_trim : String -> String
proven_idris_string_trim s = trim s

export
proven_idris_string_trim_left : String -> String
proven_idris_string_trim_left s = trimLeft s

export
proven_idris_string_trim_right : String -> String
proven_idris_string_trim_right s = trimRight s

export
proven_idris_string_pad_left : Int -> Int -> String -> String
proven_idris_string_pad_left targetLen padCharCode s =
  padLeft (decodeNat targetLen) (decodeChar padCharCode) s

export
proven_idris_string_pad_right : Int -> Int -> String -> String
proven_idris_string_pad_right targetLen padCharCode s =
  padRight (decodeNat targetLen) (decodeChar padCharCode) s

export
proven_idris_string_truncate : Int -> String -> String
proven_idris_string_truncate maxLen s =
  truncate (decodeNat maxLen) s

export
proven_idris_string_truncate_ellipsis : Int -> String -> String
proven_idris_string_truncate_ellipsis maxLen s =
  truncateWithEllipsis (decodeNat maxLen) s

--------------------------------------------------------------------------------
-- Case Operations
--------------------------------------------------------------------------------

export
proven_idris_string_to_lower : String -> String
proven_idris_string_to_lower s = toLowerAscii s

export
proven_idris_string_to_upper : String -> String
proven_idris_string_to_upper s = toUpperAscii s

export
proven_idris_string_capitalize : String -> String
proven_idris_string_capitalize s = capitalize s

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

export
proven_idris_string_is_ascii : String -> Int
proven_idris_string_is_ascii s =
  encodeBool (isAscii s)

export
proven_idris_string_is_alphanum : String -> Int
proven_idris_string_is_alphanum s =
  encodeBool (isAlphaNum s)

export
proven_idris_string_is_digits : String -> Int
proven_idris_string_is_digits s =
  encodeBool (isDigits s)

export
proven_idris_string_is_identifier : String -> Int
proven_idris_string_is_identifier s =
  encodeBool (isIdentifier s)

--------------------------------------------------------------------------------
-- Safe Conversion
--------------------------------------------------------------------------------

||| Parse string to Nat, return (status, value)
||| status = 0: success, value is valid
||| status = 1: parse error, value is 0
export
proven_idris_string_parse_nat : String -> (Int, Integer)
proven_idris_string_parse_nat s =
  case parseNat s of
    Nothing => (1, 0)
    Just n => (0, cast n)

export
proven_idris_string_parse_int : String -> (Int, Integer)
proven_idris_string_parse_int s =
  case parseInt s of
    Nothing => (1, 0)
    Just n => (0, n)

--------------------------------------------------------------------------------
-- String Length
--------------------------------------------------------------------------------

export
proven_idris_string_length : String -> Int
proven_idris_string_length s =
  cast (length s)
