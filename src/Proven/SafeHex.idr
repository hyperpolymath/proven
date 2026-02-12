-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeHex - Safe hexadecimal encoding and decoding
|||
||| This module provides safe hexadecimal conversion operations
||| that handle invalid input gracefully.
module Proven.SafeHex

import public Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- Hex Encoding
--------------------------------------------------------------------------------

||| Hexadecimal alphabet (lowercase)
hexChars : String
hexChars = "0123456789abcdef"

||| Convert a nibble (0-15) to a hex character
public export
nibbleToHex : Nat -> Char
nibbleToHex n = case strIndex hexChars (cast (n `mod` 16)) of
  Just c => c
  Nothing => '0'

||| Convert a byte to two hex characters
public export
byteToHex : Nat -> String
byteToHex b =
  let hi = (b `mod` 256) `div` 16
      lo = (b `mod` 256) `mod` 16
  in singleton (nibbleToHex hi) ++ singleton (nibbleToHex lo)

||| Encode a list of bytes to hexadecimal string
public export
encode : List Nat -> String
encode [] = ""
encode (b :: bs) = byteToHex b ++ encode bs

||| Encode with uppercase letters
public export
encodeUpper : List Nat -> String
encodeUpper = toUpper . encode

--------------------------------------------------------------------------------
-- Hex Decoding
--------------------------------------------------------------------------------

||| Convert a hex character to a nibble
public export
hexToNibble : Char -> Maybe Nat
hexToNibble c =
  if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
  else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
  else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
  else Nothing

||| Decode two hex characters to a byte
public export
hexPairToByte : Char -> Char -> Maybe Nat
hexPairToByte hi lo = do
  h <- hexToNibble hi
  l <- hexToNibble lo
  Just (h * 16 + l)

||| Decode a hexadecimal string to bytes
public export
decode : String -> Maybe (List Nat)
decode s = decodeChars (unpack s)
  where
    decodeChars : List Char -> Maybe (List Nat)
    decodeChars [] = Just []
    decodeChars [_] = Nothing  -- Odd number of characters
    decodeChars (hi :: lo :: rest) = do
      b <- hexPairToByte hi lo
      bs <- decodeChars rest
      Just (b :: bs)

||| Check if a string is valid hexadecimal
public export
isValidHex : String -> Bool
isValidHex s =
  let chars = unpack s
  in (length chars `mod` 2 == 0) && all isHexChar chars
  where
    isHexChar : Char -> Bool
    isHexChar c = (c >= '0' && c <= '9')
               || (c >= 'a' && c <= 'f')
               || (c >= 'A' && c <= 'F')

--------------------------------------------------------------------------------
-- Hex Utilities
--------------------------------------------------------------------------------

||| Convert an integer to hex string
public export
intToHex : Integer -> String
intToHex n =
  if n < 0 then "-" ++ intToHex (-n)
  else if n == 0 then "0"
  else toHex' n ""
  where
    toHex' : Integer -> String -> String
    toHex' 0 acc = acc
    toHex' i acc =
      let digit = i `mod` 16
          c = nibbleToHex (cast digit)
      in toHex' (i `div` 16) (singleton c ++ acc)

||| Parse a hex string to integer
public export
hexToInt : String -> Maybe Integer
hexToInt s =
  let s' = if isPrefixOf "0x" s || isPrefixOf "0X" s then drop 2 s else s
  in if s' == "" then Nothing else parseHex' (unpack s') 0
  where
    parseHex' : List Char -> Integer -> Maybe Integer
    parseHex' [] acc = Just acc
    parseHex' (c :: cs) acc = do
      n <- hexToNibble c
      parseHex' cs (acc * 16 + cast n)

||| Pad a hex string to specified length with leading zeros
public export
padHex : (len : Nat) -> String -> String
padHex len s =
  let padding = minus len (length s)
  in replicate padding '0' ++ s
