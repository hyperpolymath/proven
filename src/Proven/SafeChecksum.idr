-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeChecksum - Safe checksum calculations
|||
||| This module provides safe checksum calculations including
||| CRC32, Adler-32, and Luhn algorithm validation.
module Proven.SafeChecksum

import public Proven.Core
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- CRC32
--------------------------------------------------------------------------------

||| CRC32 polynomial (IEEE 802.3)
crc32Polynomial : Bits32
crc32Polynomial = 0xEDB88320

||| Calculate CRC32 step for a single byte
crc32Step : Bits32 -> Bits8 -> Bits32
crc32Step crc byte =
  let idx = xor crc (cast byte)
  in go 8 idx
  where
    go : Nat -> Bits32 -> Bits32
    go Z acc = acc
    go (S n) acc =
      let newAcc = if testBit acc 0
                   then xor (shiftR acc 1) crc32Polynomial
                   else shiftR acc 1
      in go n newAcc

||| Calculate CRC32 checksum
public export
crc32 : List Bits8 -> Bits32
crc32 bytes = complement (foldl crc32Step 0xFFFFFFFF bytes)

||| Verify CRC32 checksum
public export
verifyCRC32 : List Bits8 -> Bits32 -> Bool
verifyCRC32 bytes expected = crc32 bytes == expected

--------------------------------------------------------------------------------
-- Adler-32
--------------------------------------------------------------------------------

||| Adler-32 modulus
adler32Mod : Nat
adler32Mod = 65521

||| Calculate Adler-32 checksum
public export
adler32 : List Nat -> Bits32
adler32 bytes = go 1 0 bytes
  where
    go : Nat -> Nat -> List Nat -> Bits32
    go a b [] = cast ((b `shiftL` 16) .|. a)
    go a b (x :: xs) =
      let a' = (a + x) `mod` adler32Mod
          b' = (b + a') `mod` adler32Mod
      in go a' b' xs

||| Verify Adler-32 checksum
public export
verifyAdler32 : List Nat -> Bits32 -> Bool
verifyAdler32 bytes expected = adler32 bytes == expected

--------------------------------------------------------------------------------
-- XOR and Sum Checksums
--------------------------------------------------------------------------------

||| Calculate XOR checksum
public export
xorChecksum : List Bits8 -> Bits8
xorChecksum = foldl xor 0

||| Calculate sum checksum (mod 256)
public export
sumChecksum : List Nat -> Nat
sumChecksum bytes = (sum bytes) `mod` 256

||| Calculate two's complement checksum
public export
twosComplement : List Nat -> Nat
twosComplement bytes =
  let s = sumChecksum bytes
  in (256 - s) `mod` 256

--------------------------------------------------------------------------------
-- Luhn Algorithm
--------------------------------------------------------------------------------

||| Extract digits from a string
extractDigits : String -> List Nat
extractDigits s = map (\c => cast (ord c - ord '0')) (filter isDigit (unpack s))

||| Calculate Luhn checksum digit
public export
luhnDigit : String -> Maybe Nat
luhnDigit s =
  let digits = extractDigits s
  in if isNil digits then Nothing
     else Just (computeLuhn (reverse digits) 0 0)
  where
    computeLuhn : List Nat -> Nat -> Nat -> Nat
    computeLuhn [] _ acc = (10 - (acc `mod` 10)) `mod` 10
    computeLuhn (d :: ds) idx acc =
      let n = if idx `mod` 2 == 0
              then let doubled = d * 2
                   in if doubled > 9 then doubled - 9 else doubled
              else d
      in computeLuhn ds (S idx) (acc + n)

||| Validate Luhn checksum
public export
validateLuhn : String -> Bool
validateLuhn s =
  let digits = extractDigits s
  in if length digits < 2 then False
     else computeLuhn (reverse digits) 0 0 `mod` 10 == 0
  where
    computeLuhn : List Nat -> Nat -> Nat -> Nat
    computeLuhn [] _ acc = acc
    computeLuhn (d :: ds) idx acc =
      let n = if idx `mod` 2 == 1
              then let doubled = d * 2
                   in if doubled > 9 then doubled - 9 else doubled
              else d
      in computeLuhn ds (S idx) (acc + n)

--------------------------------------------------------------------------------
-- ISBN Validation
--------------------------------------------------------------------------------

||| Validate ISBN-10
public export
validateISBN10 : String -> Bool
validateISBN10 s =
  let chars = filter (\c => isDigit c || c == 'X' || c == 'x') (unpack s)
  in if length chars /= 10 then False
     else computeISBN10 chars 10 0 `mod` 11 == 0
  where
    charValue : Char -> Nat -> Nat
    charValue 'X' _ = 10
    charValue 'x' _ = 10
    charValue c _ = cast (ord c - ord '0')

    computeISBN10 : List Char -> Nat -> Nat -> Nat
    computeISBN10 [] _ acc = acc
    computeISBN10 (c :: cs) weight acc =
      computeISBN10 cs (minus weight 1) (acc + charValue c weight * weight)

||| Validate ISBN-13
public export
validateISBN13 : String -> Bool
validateISBN13 s =
  let digits = extractDigits s
  in if length digits /= 13 then False
     else computeISBN13 digits 0 0 `mod` 10 == 0
  where
    computeISBN13 : List Nat -> Nat -> Nat -> Nat
    computeISBN13 [] _ acc = acc
    computeISBN13 (d :: ds) idx acc =
      let weight = if idx `mod` 2 == 0 then 1 else 3
      in computeISBN13 ds (S idx) (acc + d * weight)
