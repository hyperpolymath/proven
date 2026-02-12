-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeChecksum operations
|||
||| This module exports checksum calculation and validation to the C ABI
||| via Idris2's RefC backend. All functions are proven total and prevent checksum bypass.
|||
||| Return conventions:
||| - Checksums → Int (32-bit checksums as signed int)
||| - Luhn digit → Int (0-9)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: Checksums detect transmission errors and data corruption.
|||           Never skip checksum validation in security-critical contexts.
|||           CRC32 and Adler-32 are NOT cryptographic hashes.
|||
||| Algorithms:
||| - CRC32: IEEE 802.3 polynomial (0xEDB88320)
||| - Adler-32: Modulus 65521
||| - Luhn: Credit card/ID validation
||| - ISBN-10/13: Book identifier validation
module Proven.FFI.SafeChecksum

import Proven.SafeChecksum
import Proven.Core
import Data.String
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Convert string to byte list
stringToBytes : String -> List Bits8
stringToBytes s = map (cast . ord) (unpack s)

||| Convert string to nat list (for Adler-32)
stringToNats : String -> List Nat
stringToNats s = map (cast . ord) (unpack s)

||| Bits32 to Int (signed)
bits32ToInt : Bits32 -> Int
bits32ToInt b = cast {to = Int} b

||| Int to Bits32
intToBits32 : Int -> Bits32
intToBits32 i = cast {to = Bits32} i

--------------------------------------------------------------------------------
-- CRC32
--------------------------------------------------------------------------------

export
proven_idris_checksum_crc32 : String -> Int
proven_idris_checksum_crc32 s =
  bits32ToInt (crc32 (stringToBytes s))

export
proven_idris_checksum_crc32_bytes : String -> Int
proven_idris_checksum_crc32_bytes s =
  bits32ToInt (crc32 (stringToBytes s))

export
proven_idris_checksum_verify_crc32 : String -> Int -> Int
proven_idris_checksum_verify_crc32 s expected =
  encodeBool (verifyCRC32 (stringToBytes s) (intToBits32 expected))

export
proven_idris_checksum_crc32_polynomial : Int
proven_idris_checksum_crc32_polynomial = bits32ToInt crc32Polynomial

--------------------------------------------------------------------------------
-- Adler-32
--------------------------------------------------------------------------------

export
proven_idris_checksum_adler32 : String -> Int
proven_idris_checksum_adler32 s =
  bits32ToInt (adler32 (stringToNats s))

export
proven_idris_checksum_verify_adler32 : String -> Int -> Int
proven_idris_checksum_verify_adler32 s expected =
  encodeBool (verifyAdler32 (stringToNats s) (intToBits32 expected))

export
proven_idris_checksum_adler32_mod : Int
proven_idris_checksum_adler32_mod = cast adler32Mod

--------------------------------------------------------------------------------
-- XOR and Sum Checksums
--------------------------------------------------------------------------------

export
proven_idris_checksum_xor : String -> Int
proven_idris_checksum_xor s =
  cast {to = Int} (xorChecksum (stringToBytes s))

export
proven_idris_checksum_sum : String -> Int
proven_idris_checksum_sum s =
  cast (sumChecksum (stringToNats s))

export
proven_idris_checksum_twos_complement : String -> Int
proven_idris_checksum_twos_complement s =
  cast (twosComplement (stringToNats s))

export
proven_idris_checksum_verify_xor : String -> Int -> Int
proven_idris_checksum_verify_xor s expected =
  encodeBool (cast {to = Int} (xorChecksum (stringToBytes s)) == expected)

export
proven_idris_checksum_verify_sum : String -> Int -> Int
proven_idris_checksum_verify_sum s expected =
  encodeBool (cast (sumChecksum (stringToNats s)) == expected)

--------------------------------------------------------------------------------
-- Luhn Algorithm
--------------------------------------------------------------------------------

export
proven_idris_checksum_luhn_digit : String -> Int
proven_idris_checksum_luhn_digit s =
  case luhnDigit s of
    Nothing => (-1)
    Just digit => cast digit

export
proven_idris_checksum_validate_luhn : String -> Int
proven_idris_checksum_validate_luhn s =
  encodeBool (validateLuhn s)

export
proven_idris_checksum_luhn_with_check_digit : String -> Int -> String
proven_idris_checksum_luhn_with_check_digit s checkDigit =
  s ++ show checkDigit

export
proven_idris_checksum_generate_luhn : String -> String
proven_idris_checksum_generate_luhn s =
  case luhnDigit s of
    Nothing => s
    Just digit => s ++ show digit

--------------------------------------------------------------------------------
-- ISBN Validation
--------------------------------------------------------------------------------

export
proven_idris_checksum_validate_isbn10 : String -> Int
proven_idris_checksum_validate_isbn10 s =
  encodeBool (validateISBN10 s)

export
proven_idris_checksum_validate_isbn13 : String -> Int
proven_idris_checksum_validate_isbn13 s =
  encodeBool (validateISBN13 s)

export
proven_idris_checksum_validate_isbn : String -> Int
proven_idris_checksum_validate_isbn s =
  let digits = filter isDigit (unpack s)
  in case length digits of
       10 => encodeBool (validateISBN10 s)
       13 => encodeBool (validateISBN13 s)
       _ => 0

export
proven_idris_checksum_is_isbn10_format : String -> Int
proven_idris_checksum_is_isbn10_format s =
  let chars = filter (\c => isDigit c || c == 'X' || c == 'x') (unpack s)
  in encodeBool (length chars == 10)

export
proven_idris_checksum_is_isbn13_format : String -> Int
proven_idris_checksum_is_isbn13_format s =
  let digits = filter isDigit (unpack s)
  in encodeBool (length digits == 13)

--------------------------------------------------------------------------------
-- Checksum Comparison
--------------------------------------------------------------------------------

export
proven_idris_checksum_checksums_equal : Int -> Int -> Int
proven_idris_checksum_checksums_equal csum1 csum2 =
  encodeBool (csum1 == csum2)

export
proven_idris_checksum_verify_checksum : Int -> Int -> Int
proven_idris_checksum_verify_checksum calculated expected =
  encodeBool (calculated == expected)

--------------------------------------------------------------------------------
-- Data Validation Helpers
--------------------------------------------------------------------------------

export
proven_idris_checksum_has_digits : String -> Int
proven_idris_checksum_has_digits s =
  encodeBool (any isDigit (unpack s))

export
proven_idris_checksum_all_digits : String -> Int
proven_idris_checksum_all_digits s =
  let nonEmpty = not (null s)
  in encodeBool (nonEmpty && all isDigit (unpack s))

export
proven_idris_checksum_digit_count : String -> Int
proven_idris_checksum_digit_count s =
  cast (length (filter isDigit (unpack s)))

export
proven_idris_checksum_is_valid_length_for_luhn : String -> Int
proven_idris_checksum_is_valid_length_for_luhn s =
  let digitCount = length (filter isDigit (unpack s))
  in encodeBool (digitCount >= 2)

--------------------------------------------------------------------------------
-- Checksum Format Helpers
--------------------------------------------------------------------------------

export
proven_idris_checksum_to_hex : Int -> String
proven_idris_checksum_to_hex csum =
  let hex = toHex (cast {to = Bits32} csum)
  in if length hex < 8
       then pack (replicate (8 - length hex) '0') ++ hex
       else hex
  where
    toHex : Bits32 -> String
    toHex 0 = "0"
    toHex n = go n ""
      where
        go : Bits32 -> String -> String
        go 0 acc = acc
        go n acc =
          let digit = n `mod` 16
              char = if digit < 10
                     then chr (ord '0' + cast digit)
                     else chr (ord 'a' + cast (digit - 10))
          in go (n `div` 16) (strCons char acc)

export
proven_idris_checksum_from_hex : String -> Int
proven_idris_checksum_from_hex s =
  cast {to = Int} (parseHex s 0)
  where
    parseHex : String -> Bits32 -> Bits32
    parseHex str acc = go (unpack str) acc
      where
        digitValue : Char -> Bits32
        digitValue c =
          if c >= '0' && c <= '9' then cast (ord c - ord '0')
          else if c >= 'a' && c <= 'f' then cast (ord c - ord 'a' + 10)
          else if c >= 'A' && c <= 'F' then cast (ord c - ord 'A' + 10)
          else 0

        go : List Char -> Bits32 -> Bits32
        go [] acc = acc
        go (c :: cs) acc = go cs (acc * 16 + digitValue c)

--------------------------------------------------------------------------------
-- Constant Values
--------------------------------------------------------------------------------

export
proven_idris_checksum_luhn_mod : Int
proven_idris_checksum_luhn_mod = 10

export
proven_idris_checksum_isbn10_length : Int
proven_idris_checksum_isbn10_length = 10

export
proven_idris_checksum_isbn13_length : Int
proven_idris_checksum_isbn13_length = 13

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_checksum_friendly_error : String -> String
proven_idris_checksum_friendly_error errorMsg =
  if isInfixOf "crc" (toLower errorMsg) || isInfixOf "crc32" (toLower errorMsg)
    then "CRC32 checksum mismatch (data may be corrupted)"
  else if isInfixOf "adler" (toLower errorMsg)
    then "Adler-32 checksum mismatch (data may be corrupted)"
  else if isInfixOf "luhn" (toLower errorMsg)
    then "Luhn checksum validation failed (invalid card/ID number)"
  else if isInfixOf "isbn" (toLower errorMsg)
    then "ISBN validation failed (invalid book identifier)"
  else if isInfixOf "xor" (toLower errorMsg) || isInfixOf "sum" (toLower errorMsg)
    then "Checksum validation failed"
  else if isInfixOf "digits" (toLower errorMsg)
    then "Invalid input (expected digits only)"
  else
    "Checksum error"
