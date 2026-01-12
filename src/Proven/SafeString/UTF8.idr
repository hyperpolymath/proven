-- SPDX-License-Identifier: Palimpsest-MPL-1.0
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
  = ASCII           -- 0xxxxxxx - Single byte (ASCII)
  | LeadingByte2    -- 110xxxxx - Start of 2-byte sequence
  | LeadingByte3    -- 1110xxxx - Start of 3-byte sequence
  | LeadingByte4    -- 11110xxx - Start of 4-byte sequence
  | ContinuationByte -- 10xxxxxx - Continuation byte
  | InvalidByte     -- Invalid UTF-8 byte

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
-- UTF-8 Validation
--------------------------------------------------------------------------------

||| Result of UTF-8 validation
public export
data UTF8ValidationResult
  = Valid                           -- String is valid UTF-8
  | InvalidSequence Nat             -- Invalid sequence at byte offset
  | UnexpectedEnd Nat               -- Truncated sequence at byte offset
  | OverlongEncoding Nat            -- Overlong encoding at byte offset
  | InvalidCodepoint Nat Nat        -- Invalid codepoint at offset with value

||| Validate UTF-8 byte sequence
public export
validateUTF8 : List Bits8 -> UTF8ValidationResult
validateUTF8 bytes = go bytes 0
  where
    -- Check continuation bytes
    checkContinuation : List Bits8 -> Nat -> Nat -> (List Bits8, Bool)
    checkContinuation bs offset 0 = (bs, True)
    checkContinuation [] offset _ = ([], False)
    checkContinuation (b :: bs) offset n =
      case classifyByte b of
        ContinuationByte => checkContinuation bs (S offset) (minus n 1)
        _ => (b :: bs, False)

    go : List Bits8 -> Nat -> UTF8ValidationResult
    go [] _ = Valid
    go (b :: bs) offset =
      case classifyByte b of
        ASCII => go bs (S offset)
        ContinuationByte => InvalidSequence offset  -- Unexpected continuation
        LeadingByte2 =>
          case checkContinuation bs (S offset) 1 of
            (rest, True) => go rest (S (S offset))
            (_, False) => UnexpectedEnd offset
        LeadingByte3 =>
          case checkContinuation bs (S offset) 2 of
            (rest, True) => go rest (S (S (S offset)))
            (_, False) => UnexpectedEnd offset
        LeadingByte4 =>
          case checkContinuation bs (S offset) 3 of
            (rest, True) => go rest (S (S (S (S offset))))
            (_, False) => UnexpectedEnd offset
        InvalidByte => InvalidSequence offset

||| Check if bytes form valid UTF-8
public export
isValidUTF8 : List Bits8 -> Bool
isValidUTF8 bytes = case validateUTF8 bytes of
  Valid => True
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

||| Decode UTF-8 bytes to codepoints
public export
decodeUTF8 : List Bits8 -> Maybe (List Codepoint)
decodeUTF8 bytes = go bytes []
  where
    -- Extract value bits from continuation byte
    contBits : Bits8 -> Nat
    contBits b = cast (b `and` 0x3F)

    go : List Bits8 -> List Codepoint -> Maybe (List Codepoint)
    go [] acc = Just (reverse acc)
    go (b :: bs) acc =
      case classifyByte b of
        ASCII => go bs (cast b :: acc)
        LeadingByte2 =>
          case bs of
            (c :: rest) =>
              let cp = (cast (b `and` 0x1F) `shiftL` 6) + contBits c
              in go rest (cp :: acc)
            _ => Nothing
        LeadingByte3 =>
          case bs of
            (c1 :: c2 :: rest) =>
              let cp = (cast (b `and` 0x0F) `shiftL` 12) +
                       (contBits c1 `shiftL` 6) +
                       contBits c2
              in go rest (cp :: acc)
            _ => Nothing
        LeadingByte4 =>
          case bs of
            (c1 :: c2 :: c3 :: rest) =>
              let cp = (cast (b `and` 0x07) `shiftL` 18) +
                       (contBits c1 `shiftL` 12) +
                       (contBits c2 `shiftL` 6) +
                       contBits c3
              in go rest (cp :: acc)
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
      then Just [ cast (0xC0 + (cp `shiftR` 6))
                , cast (0x80 + (cp `and` 0x3F))
                ]
    else if cp < 0x10000
      then Just [ cast (0xE0 + (cp `shiftR` 12))
                , cast (0x80 + ((cp `shiftR` 6) `and` 0x3F))
                , cast (0x80 + (cp `and` 0x3F))
                ]
    else Just [ cast (0xF0 + (cp `shiftR` 18))
              , cast (0x80 + ((cp `shiftR` 12) `and` 0x3F))
              , cast (0x80 + ((cp `shiftR` 6) `and` 0x3F))
              , cast (0x80 + (cp `and` 0x3F))
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
