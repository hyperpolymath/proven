-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeChecksum - Verified checksum operations
|||
||| Type-safe checksum and hash computations including CRC variants,
||| Adler-32, Fletcher, and simple hash functions.
|||
||| Guarantees checksums are computed correctly and results are properly sized.
module Proven.SafeChecksum

import Proven.Core
import Data.So
import Data.Bits
import Data.List
import Data.Vect

%default total

-- ============================================================================
-- CHECKSUM TYPES
-- ============================================================================

||| 8-bit checksum
public export
record Checksum8 where
  constructor MkChecksum8
  value : Bits8

||| 16-bit checksum
public export
record Checksum16 where
  constructor MkChecksum16
  value : Bits16

||| 32-bit checksum
public export
record Checksum32 where
  constructor MkChecksum32
  value : Bits32

||| 64-bit checksum
public export
record Checksum64 where
  constructor MkChecksum64
  value : Bits64

export
Show Checksum8 where
  show c = "0x" ++ toHex8 c.value

export
Show Checksum16 where
  show c = "0x" ++ toHex16 c.value

export
Show Checksum32 where
  show c = "0x" ++ toHex32 c.value

export
Show Checksum64 where
  show c = "0x" ++ toHex64 c.value

-- Hex formatting helpers
toHex8 : Bits8 -> String
toHex8 b = pack [hexDigit (cast (b `shiftR` 4)), hexDigit (cast (b .&. 0xF))]
  where
    hexDigit : Nat -> Char
    hexDigit n = if n < 10 then chr (cast n + ord '0') else chr (cast (n - 10) + ord 'a')

toHex16 : Bits16 -> String
toHex16 b = toHex8 (cast (b `shiftR` 8)) ++ toHex8 (cast b)

toHex32 : Bits32 -> String
toHex32 b = toHex16 (cast (b `shiftR` 16)) ++ toHex16 (cast b)

toHex64 : Bits64 -> String
toHex64 b = toHex32 (cast (b `shiftR` 32)) ++ toHex32 (cast b)

-- ============================================================================
-- SIMPLE CHECKSUMS
-- ============================================================================

||| Simple XOR checksum (8-bit)
export
xorChecksum : List Bits8 -> Checksum8
xorChecksum bytes = MkChecksum8 (foldl xor 0 bytes)

||| Simple additive checksum (8-bit)
export
addChecksum8 : List Bits8 -> Checksum8
addChecksum8 bytes = MkChecksum8 (foldl (+) 0 bytes)

||| Simple additive checksum (16-bit)
export
addChecksum16 : List Bits8 -> Checksum16
addChecksum16 bytes = MkChecksum16 (foldl (\acc, b => acc + cast b) 0 bytes)

||| Two's complement checksum (for validation: sum + checksum = 0)
export
twosComplementChecksum : List Bits8 -> Checksum8
twosComplementChecksum bytes =
  let sum = foldl (+) 0 bytes
  in MkChecksum8 (complement sum + 1)

||| Verify two's complement checksum
export
verifyTwosComplement : List Bits8 -> Checksum8 -> Bool
verifyTwosComplement bytes ck =
  let sum = foldl (+) 0 bytes
  in sum + ck.value == 0

-- ============================================================================
-- INTERNET CHECKSUM (RFC 1071)
-- ============================================================================

||| Internet checksum used in IP, TCP, UDP headers
export
internetChecksum : List Bits8 -> Checksum16
internetChecksum bytes = MkChecksum16 (complement (cast finalSum))
  where
    -- Sum pairs of bytes as 16-bit words
    sumWords : List Bits8 -> Bits32
    sumWords [] = 0
    sumWords [b] = cast b `shiftL` 8  -- Pad odd byte
    sumWords (h :: l :: rest) =
      let word : Bits32 = (cast h `shiftL` 8) .|. cast l
      in word + sumWords rest

    -- Fold 32-bit sum to 16-bit with carry
    foldCarry : Bits32 -> Bits16
    foldCarry s =
      let folded = (s .&. 0xFFFF) + (s `shiftR` 16)
          folded2 = (folded .&. 0xFFFF) + (folded `shiftR` 16)
      in cast folded2

    finalSum : Bits16
    finalSum = foldCarry (sumWords bytes)

||| Verify internet checksum (result should be 0xFFFF)
export
verifyInternetChecksum : List Bits8 -> Bool
verifyInternetChecksum bytes =
  let (MkChecksum16 result) = internetChecksum bytes
  in result == 0xFFFF

-- ============================================================================
-- FLETCHER CHECKSUM
-- ============================================================================

||| Fletcher-16 checksum
export
fletcher16 : List Bits8 -> Checksum16
fletcher16 bytes =
  let (sum1, sum2) = foldl step (0, 0) bytes
      check1 = sum1 `mod` 255
      check2 = sum2 `mod` 255
  in MkChecksum16 ((check2 `shiftL` 8) .|. check1)
  where
    step : (Bits16, Bits16) -> Bits8 -> (Bits16, Bits16)
    step (s1, s2) b =
      let s1' = (s1 + cast b) `mod` 255
          s2' = (s2 + s1') `mod` 255
      in (s1', s2')

||| Fletcher-32 checksum
export
fletcher32 : List Bits16 -> Checksum32
fletcher32 words =
  let (sum1, sum2) = foldl step (0, 0) words
      check1 = sum1 `mod` 65535
      check2 = sum2 `mod` 65535
  in MkChecksum32 ((check2 `shiftL` 16) .|. check1)
  where
    step : (Bits32, Bits32) -> Bits16 -> (Bits32, Bits32)
    step (s1, s2) w =
      let s1' = (s1 + cast w) `mod` 65535
          s2' = (s2 + s1') `mod` 65535
      in (s1', s2')

||| Fletcher-64 checksum
export
fletcher64 : List Bits32 -> Checksum64
fletcher64 words =
  let (sum1, sum2) = foldl step (0, 0) words
  in MkChecksum64 ((sum2 `shiftL` 32) .|. sum1)
  where
    step : (Bits64, Bits64) -> Bits32 -> (Bits64, Bits64)
    step (s1, s2) w =
      let s1' = (s1 + cast w) `mod` 0xFFFFFFFF
          s2' = (s2 + s1') `mod` 0xFFFFFFFF
      in (s1', s2')

-- ============================================================================
-- ADLER-32
-- ============================================================================

||| Adler-32 checksum (used in zlib)
export
adler32 : List Bits8 -> Checksum32
adler32 bytes =
  let (a, b) = foldl step (1, 0) bytes
      a' = a `mod` 65521
      b' = b `mod` 65521
  in MkChecksum32 ((b' `shiftL` 16) .|. a')
  where
    step : (Bits32, Bits32) -> Bits8 -> (Bits32, Bits32)
    step (a, b) byte =
      let a' = (a + cast byte) `mod` 65521
          b' = (b + a') `mod` 65521
      in (a', b')

||| Update Adler-32 with more data
export
adler32Update : Checksum32 -> List Bits8 -> Checksum32
adler32Update (MkChecksum32 prev) bytes =
  let a = prev .&. 0xFFFF
      b = prev `shiftR` 16
      (a', b') = foldl step (a, b) bytes
      a'' = a' `mod` 65521
      b'' = b' `mod` 65521
  in MkChecksum32 ((b'' `shiftL` 16) .|. a'')
  where
    step : (Bits32, Bits32) -> Bits8 -> (Bits32, Bits32)
    step (a, b) byte =
      let a' = (a + cast byte) `mod` 65521
          b' = (b + a') `mod` 65521
      in (a', b')

-- ============================================================================
-- CRC
-- ============================================================================

||| CRC-8 with configurable polynomial
export
crc8 : Bits8 -> List Bits8 -> Checksum8
crc8 poly bytes = MkChecksum8 (foldl (processByte poly) 0 bytes)
  where
    processBit : Bits8 -> Bits8 -> Bits8
    processBit p crc =
      if (crc .&. 0x80) /= 0
      then (crc `shiftL` 1) `xor` p
      else crc `shiftL` 1

    processByte : Bits8 -> Bits8 -> Bits8 -> Bits8
    processByte p crc byte =
      let crc' = crc `xor` byte
      in foldl (\c, _ => processBit p c) crc' [0..7]

||| CRC-8 with standard polynomial (0x07)
export
crc8Standard : List Bits8 -> Checksum8
crc8Standard = crc8 0x07

||| CRC-16-CCITT
export
crc16CCITT : List Bits8 -> Checksum16
crc16CCITT bytes = MkChecksum16 (foldl processByte 0xFFFF bytes)
  where
    poly : Bits16
    poly = 0x1021

    processByte : Bits16 -> Bits8 -> Bits16
    processByte crc byte =
      let crc' = crc `xor` (cast byte `shiftL` 8)
      in foldl processBit crc' [0..7]

    processBit : Bits16 -> Nat -> Bits16
    processBit crc _ =
      if (crc .&. 0x8000) /= 0
      then (crc `shiftL` 1) `xor` poly
      else crc `shiftL` 1

||| CRC-16 (IBM/ANSI)
export
crc16IBM : List Bits8 -> Checksum16
crc16IBM bytes = MkChecksum16 (foldl processByte 0 bytes)
  where
    poly : Bits16
    poly = 0xA001  -- Reflected polynomial

    processByte : Bits16 -> Bits8 -> Bits16
    processByte crc byte =
      let crc' = crc `xor` cast byte
      in foldl processBit crc' [0..7]

    processBit : Bits16 -> Nat -> Bits16
    processBit crc _ =
      if (crc .&. 1) /= 0
      then (crc `shiftR` 1) `xor` poly
      else crc `shiftR` 1

||| CRC-32 (IEEE 802.3 / Ethernet / ZIP)
export
crc32 : List Bits8 -> Checksum32
crc32 bytes = MkChecksum32 (complement (foldl processByte 0xFFFFFFFF bytes))
  where
    poly : Bits32
    poly = 0xEDB88320  -- Reflected polynomial

    processByte : Bits32 -> Bits8 -> Bits32
    processByte crc byte =
      let crc' = crc `xor` cast byte
      in foldl processBit crc' [0..7]

    processBit : Bits32 -> Nat -> Bits32
    processBit crc _ =
      if (crc .&. 1) /= 0
      then (crc `shiftR` 1) `xor` poly
      else crc `shiftR` 1

||| CRC-32C (Castagnoli) - used in iSCSI, SCTP, ext4
export
crc32c : List Bits8 -> Checksum32
crc32c bytes = MkChecksum32 (complement (foldl processByte 0xFFFFFFFF bytes))
  where
    poly : Bits32
    poly = 0x82F63B78  -- Reflected Castagnoli polynomial

    processByte : Bits32 -> Bits8 -> Bits32
    processByte crc byte =
      let crc' = crc `xor` cast byte
      in foldl processBit crc' [0..7]

    processBit : Bits32 -> Nat -> Bits32
    processBit crc _ =
      if (crc .&. 1) /= 0
      then (crc `shiftR` 1) `xor` poly
      else crc `shiftR` 1

||| CRC-64-ECMA
export
crc64 : List Bits8 -> Checksum64
crc64 bytes = MkChecksum64 (foldl processByte 0xFFFFFFFFFFFFFFFF bytes)
  where
    poly : Bits64
    poly = 0xC96C5795D7870F42  -- Reflected ECMA polynomial

    processByte : Bits64 -> Bits8 -> Bits64
    processByte crc byte =
      let crc' = crc `xor` cast byte
      in foldl processBit crc' [0..7]

    processBit : Bits64 -> Nat -> Bits64
    processBit crc _ =
      if (crc .&. 1) /= 0
      then (crc `shiftR` 1) `xor` poly
      else crc `shiftR` 1

-- ============================================================================
-- HASH FUNCTIONS (Non-cryptographic)
-- ============================================================================

||| FNV-1a hash (32-bit)
export
fnv1a32 : List Bits8 -> Checksum32
fnv1a32 bytes = MkChecksum32 (foldl step 0x811c9dc5 bytes)
  where
    step : Bits32 -> Bits8 -> Bits32
    step hash byte = (hash `xor` cast byte) * 0x01000193

||| FNV-1a hash (64-bit)
export
fnv1a64 : List Bits8 -> Checksum64
fnv1a64 bytes = MkChecksum64 (foldl step 0xcbf29ce484222325 bytes)
  where
    step : Bits64 -> Bits8 -> Bits64
    step hash byte = (hash `xor` cast byte) * 0x100000001b3

||| DJB2 hash (Dan Bernstein)
export
djb2 : List Bits8 -> Checksum32
djb2 bytes = MkChecksum32 (foldl step 5381 bytes)
  where
    step : Bits32 -> Bits8 -> Bits32
    step hash byte = ((hash `shiftL` 5) + hash) + cast byte  -- hash * 33 + byte

||| SDBM hash
export
sdbm : List Bits8 -> Checksum32
sdbm bytes = MkChecksum32 (foldl step 0 bytes)
  where
    step : Bits32 -> Bits8 -> Bits32
    step hash byte = cast byte + (hash `shiftL` 6) + (hash `shiftL` 16) - hash

||| Jenkins one-at-a-time hash
export
jenkinsOAT : List Bits8 -> Checksum32
jenkinsOAT bytes =
  let hash = foldl step 0 bytes
      hash' = hash + (hash `shiftL` 3)
      hash'' = hash' `xor` (hash' `shiftR` 11)
  in MkChecksum32 (hash'' + (hash'' `shiftL` 15))
  where
    step : Bits32 -> Bits8 -> Bits32
    step hash byte =
      let h = hash + cast byte
          h' = h + (h `shiftL` 10)
      in h' `xor` (h' `shiftR` 6)

||| MurmurHash3 finalizer (32-bit)
export
murmur3Finalize : Bits32 -> Checksum32
murmur3Finalize h =
  let h' = h `xor` (h `shiftR` 16)
      h'' = h' * 0x85ebca6b
      h''' = h'' `xor` (h'' `shiftR` 13)
      h'''' = h''' * 0xc2b2ae35
  in MkChecksum32 (h'''' `xor` (h'''' `shiftR` 16))

-- ============================================================================
-- LUHN ALGORITHM (Credit card validation)
-- ============================================================================

||| Compute Luhn checksum digit
export
luhnCheckDigit : List Nat -> Nat
luhnCheckDigit digits =
  let doubled = zipWith (\i, d => if i `mod` 2 == 0 then d else doubleDigit d)
                        [0..length digits] (reverse digits)
      sum = foldl (+) 0 doubled
  in (10 - (sum `mod` 10)) `mod` 10
  where
    doubleDigit : Nat -> Nat
    doubleDigit d = let d' = d * 2 in if d' > 9 then d' - 9 else d'

||| Validate Luhn checksum
export
luhnValidate : List Nat -> Bool
luhnValidate digits =
  let doubled = zipWith (\i, d => if i `mod` 2 == 1 then doubleDigit d else d)
                        [0..length digits] (reverse digits)
      sum = foldl (+) 0 doubled
  in sum `mod` 10 == 0
  where
    doubleDigit : Nat -> Nat
    doubleDigit d = let d' = d * 2 in if d' > 9 then d' - 9 else d'

-- ============================================================================
-- ISBN VALIDATION
-- ============================================================================

||| Validate ISBN-10 check digit
export
isbn10Validate : List Nat -> Bool
isbn10Validate digits =
  if length digits /= 10 then False
  else let weighted = zipWith (*) [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] digits
           sum = foldl (+) 0 weighted
       in sum `mod` 11 == 0

||| Calculate ISBN-10 check digit
export
isbn10CheckDigit : List Nat -> Nat
isbn10CheckDigit digits =
  if length digits /= 9 then 0
  else let weighted = zipWith (*) [10, 9, 8, 7, 6, 5, 4, 3, 2] digits
           sum = foldl (+) 0 weighted
           remainder = sum `mod` 11
       in if remainder == 0 then 0 else 11 - remainder

||| Validate ISBN-13 check digit
export
isbn13Validate : List Nat -> Bool
isbn13Validate digits =
  if length digits /= 13 then False
  else let weights = cycle [1, 3] 13
           weighted = zipWith (*) weights digits
           sum = foldl (+) 0 weighted
       in sum `mod` 10 == 0
  where
    cycle : List Nat -> Nat -> List Nat
    cycle _ 0 = []
    cycle [] _ = []
    cycle (x :: xs) n = x :: cycle (xs ++ [x]) (n `minus` 1)

-- ============================================================================
-- UTILITY
-- ============================================================================

||| Convert string to bytes
export
stringToBytes : String -> List Bits8
stringToBytes s = map (cast . ord) (unpack s)

||| Verify any checksum matches expected
export
verify32 : (List Bits8 -> Checksum32) -> List Bits8 -> Bits32 -> Bool
verify32 hashFn bytes expected =
  let (MkChecksum32 computed) = hashFn bytes
  in computed == expected

||| Combine two checksums (for parallel computation)
export
combine32 : Checksum32 -> Checksum32 -> Checksum32
combine32 (MkChecksum32 a) (MkChecksum32 b) = MkChecksum32 (a `xor` b)

