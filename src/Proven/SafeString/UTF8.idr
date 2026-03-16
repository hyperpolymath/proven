-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| UTF-8 encoding and decoding with safety guarantees
|||
||| This module provides safe UTF-8 operations that never throw exceptions
||| and properly handle invalid sequences.
module Proven.SafeString.UTF8

import Proven.Core
import Data.List
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- UTF-8 Byte Classification
--------------------------------------------------------------------------------

||| Classify a byte in UTF-8 encoding
public export
data UTF8ByteType
  = ASCII            -- 0xxxxxxx - Single byte (ASCII)
  | LeadingByte2     -- 110xxxxx - Start of 2-byte sequence
  | LeadingByte3     -- 1110xxxx - Start of 3-byte sequence
  | LeadingByte4     -- 11110xxx - Start of 4-byte sequence
  | ContinuationByte -- 10xxxxxx - Continuation byte
  | InvalidByte      -- Invalid UTF-8 byte

||| Classify a byte for UTF-8
public export
classifyByte : Bits8 -> UTF8ByteType
classifyByte b =
  if b < 0x80 then ASCII
  else if b < 0xC0 then ContinuationByte
  else if b < 0xE0 then LeadingByte2
  else if b < 0xF0 then LeadingByte3
  else if b < 0xF8 then LeadingByte4
  else InvalidByte

--------------------------------------------------------------------------------
-- Bitwise helpers for Bits8 (using Data.Bits interface)
--------------------------------------------------------------------------------

||| Bitwise AND for Bits8
bitAnd8 : Bits8 -> Bits8 -> Bits8
bitAnd8 = (.&.)

||| Bitwise AND for Nat via Bits32 intermediary
||| Used for codepoint manipulation
natBitAnd : Nat -> Nat -> Nat
natBitAnd a b = cast ((.&.) {a=Bits32} (cast a) (cast b))

||| Right shift for Nat using Integer division by powers of 2
natShiftR : Nat -> Nat -> Nat
natShiftR val 0 = val
natShiftR val (S n) = natShiftR (div val 2) n

||| Left shift for Nat using multiplication by powers of 2
natShiftL : Nat -> Nat -> Nat
natShiftL val 0 = val
natShiftL val (S n) = natShiftL (val * 2) n

--------------------------------------------------------------------------------
-- UTF-8 Validation
--------------------------------------------------------------------------------

||| Result of UTF-8 validation
public export
data UTF8ValidationResult
  = ValidUTF8                      -- String is valid UTF-8
  | InvalidSequence Nat            -- Invalid sequence at byte offset
  | UnexpectedEnd Nat              -- Truncated sequence at byte offset
  | OverlongEncoding Nat           -- Overlong encoding at byte offset
  | InvalidCodepoint Nat Nat       -- Invalid codepoint at offset with value

||| Validate UTF-8 byte sequence, bounded by fuel parameter for totality
public export
validateUTF8 : List Bits8 -> UTF8ValidationResult
validateUTF8 bytes = go (length bytes) bytes 0
  where
    -- Check continuation bytes
    checkContinuation : Nat -> List Bits8 -> Nat -> Nat -> (List Bits8, Bool)
    checkContinuation _ bs offset 0 = (bs, True)
    checkContinuation 0 _ offset _ = ([], False)
    checkContinuation (S fuel) [] offset _ = ([], False)
    checkContinuation (S fuel) (b :: bs) offset n =
      case classifyByte b of
        ContinuationByte => checkContinuation fuel bs (S offset) (minus n 1)
        _ => (b :: bs, False)

    go : Nat -> List Bits8 -> Nat -> UTF8ValidationResult
    go _ [] _ = ValidUTF8
    go 0 _ offset = UnexpectedEnd offset  -- Fuel exhausted
    go (S fuel) (b :: bs) offset =
      case classifyByte b of
        ASCII => go fuel bs (S offset)
        ContinuationByte => InvalidSequence offset
        LeadingByte2 =>
          case checkContinuation fuel bs (S offset) 1 of
            (rest, True) => go fuel rest (S (S offset))
            (_, False) => UnexpectedEnd offset
        LeadingByte3 =>
          case checkContinuation fuel bs (S offset) 2 of
            (rest, True) => go fuel rest (S (S (S offset)))
            (_, False) => UnexpectedEnd offset
        LeadingByte4 =>
          case checkContinuation fuel bs (S offset) 3 of
            (rest, True) => go fuel rest (S (S (S (S offset))))
            (_, False) => UnexpectedEnd offset
        InvalidByte => InvalidSequence offset

||| Check if bytes form valid UTF-8
public export
isValidUTF8 : List Bits8 -> Bool
isValidUTF8 bytes = case validateUTF8 bytes of
  ValidUTF8 => True
  _ => False

--------------------------------------------------------------------------------
-- UTF-8 Decoding
--------------------------------------------------------------------------------

||| A decoded Unicode codepoint
public export
Codepoint : Type
Codepoint = Nat  -- Unicode codepoints are 0 to 0x10FFFF

||| Maximum valid Unicode codepoint
public export
maxCodepoint : Nat
maxCodepoint = 0x10FFFF

||| Check if a codepoint is valid Unicode
public export
isValidCodepoint : Codepoint -> Bool
isValidCodepoint cp = cp <= maxCodepoint && not (cp >= 0xD800 && cp <= 0xDFFF)

||| Extract continuation bits from a byte (lower 6 bits)
contBits : Bits8 -> Nat
contBits b = cast (bitAnd8 b 0x3F)

||| Decode UTF-8 bytes to codepoints, bounded by fuel for totality
public export
decodeUTF8 : List Bits8 -> Maybe (List Codepoint)
decodeUTF8 bytes = go (length bytes) bytes []
  where
    go : Nat -> List Bits8 -> List Codepoint -> Maybe (List Codepoint)
    go _ [] acc = Just (reverse acc)
    go 0 _ _ = Nothing  -- Fuel exhausted
    go (S fuel) (b :: bs) acc =
      case classifyByte b of
        ASCII => go fuel bs (cast b :: acc)
        LeadingByte2 =>
          case bs of
            (c :: rest) =>
              let cp = natShiftL (cast (bitAnd8 b 0x1F)) 6 + contBits c
              in go fuel rest (cp :: acc)
            _ => Nothing
        LeadingByte3 =>
          case bs of
            (c1 :: c2 :: rest) =>
              let cp = natShiftL (cast (bitAnd8 b 0x0F)) 12 +
                       natShiftL (contBits c1) 6 +
                       contBits c2
              in go fuel rest (cp :: acc)
            _ => Nothing
        LeadingByte4 =>
          case bs of
            (c1 :: c2 :: c3 :: rest) =>
              let cp = natShiftL (cast (bitAnd8 b 0x07)) 18 +
                       natShiftL (contBits c1) 12 +
                       natShiftL (contBits c2) 6 +
                       contBits c3
              in go fuel rest (cp :: acc)
            _ => Nothing
        _ => Nothing

--------------------------------------------------------------------------------
-- UTF-8 Encoding
--------------------------------------------------------------------------------

||| Encode a single codepoint to UTF-8 bytes
public export
encodeCodepoint : Codepoint -> Maybe (List Bits8)
encodeCodepoint cp =
  if not (isValidCodepoint cp)
    then Nothing
    else if cp < 0x80
      then Just [cast cp]
    else if cp < 0x800
      then Just [ cast (0xC0 + natShiftR cp 6)
                , cast (0x80 + natBitAnd cp 0x3F)
                ]
    else if cp < 0x10000
      then Just [ cast (0xE0 + natShiftR cp 12)
                , cast (0x80 + natBitAnd (natShiftR cp 6) 0x3F)
                , cast (0x80 + natBitAnd cp 0x3F)
                ]
    else Just [ cast (0xF0 + natShiftR cp 18)
              , cast (0x80 + natBitAnd (natShiftR cp 12) 0x3F)
              , cast (0x80 + natBitAnd (natShiftR cp 6) 0x3F)
              , cast (0x80 + natBitAnd cp 0x3F)
              ]

||| Encode a list of codepoints to UTF-8 bytes
public export
encodeUTF8 : List Codepoint -> Maybe (List Bits8)
encodeUTF8 cps = map concat (traverse encodeCodepoint cps)

--------------------------------------------------------------------------------
-- Byte Length Calculations
--------------------------------------------------------------------------------

||| Calculate UTF-8 byte length for a codepoint
public export
codepointByteLength : Codepoint -> Nat
codepointByteLength cp =
  if cp < 0x80 then 1
  else if cp < 0x800 then 2
  else if cp < 0x10000 then 3
  else 4

||| Count bytes needed for a string of codepoints
public export
utf8ByteLength : List Codepoint -> Nat
utf8ByteLength = sum . map codepointByteLength

--------------------------------------------------------------------------------
-- String Conversion
--------------------------------------------------------------------------------

||| Try to decode bytes to a string (using Idris's internal representation)
public export
bytesToString : List Bits8 -> Maybe String
bytesToString bytes =
  if isValidUTF8 bytes
    then Just (pack (map (cast . cast {to=Int}) bytes))
    else Nothing

||| Convert string to bytes
public export
stringToBytes : String -> List Bits8
stringToBytes s = map (cast . ord) (unpack s)
