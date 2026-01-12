-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe Base64 encoding
|||
||| This module provides safe Base64 encoding operations that:
||| - Handle all input correctly without exceptions
||| - Support multiple Base64 variants
||| - Calculate correct output lengths
||| - Insert line breaks for MIME encoding
module Proven.SafeBase64.Encode

import Proven.Core
import Proven.SafeBase64.Types
import Data.List
import Data.String
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- Core Encoding
--------------------------------------------------------------------------------

||| Encode a single group of 3 bytes to 4 Base64 characters
encodeGroup : Base64Variant -> Bits8 -> Bits8 -> Bits8 -> List Char
encodeGroup variant b0 b1 b2 =
  let -- Split 24 bits into 4 6-bit values
      v0 = b0 `shiftR` 2
      v1 = ((b0 .&. 0x03) `shiftL` 4) .|. (b1 `shiftR` 4)
      v2 = ((b1 .&. 0x0F) `shiftL` 2) .|. (b2 `shiftR` 6)
      v3 = b2 .&. 0x3F
  in [ valueToChar variant v0
     , valueToChar variant v1
     , valueToChar variant v2
     , valueToChar variant v3
     ]

||| Encode remaining 1 byte with padding
encodeRemainder1 : Base64Variant -> Bits8 -> List Char
encodeRemainder1 variant b0 =
  let v0 = b0 `shiftR` 2
      v1 = (b0 .&. 0x03) `shiftL` 4
  in if usesPadding variant
       then [valueToChar variant v0, valueToChar variant v1, paddingChar, paddingChar]
       else [valueToChar variant v0, valueToChar variant v1]

||| Encode remaining 2 bytes with padding
encodeRemainder2 : Base64Variant -> Bits8 -> Bits8 -> List Char
encodeRemainder2 variant b0 b1 =
  let v0 = b0 `shiftR` 2
      v1 = ((b0 .&. 0x03) `shiftL` 4) .|. (b1 `shiftR` 4)
      v2 = (b1 .&. 0x0F) `shiftL` 2
  in if usesPadding variant
       then [valueToChar variant v0, valueToChar variant v1, valueToChar variant v2, paddingChar]
       else [valueToChar variant v0, valueToChar variant v1, valueToChar variant v2]

||| Core encoding loop
encodeLoop : Base64Variant -> List Bits8 -> List Char
encodeLoop variant [] = []
encodeLoop variant [b0] = encodeRemainder1 variant b0
encodeLoop variant [b0, b1] = encodeRemainder2 variant b0 b1
encodeLoop variant (b0 :: b1 :: b2 :: rest) =
  encodeGroup variant b0 b1 b2 ++ encodeLoop variant rest

--------------------------------------------------------------------------------
-- Line Breaking for MIME
--------------------------------------------------------------------------------

||| Insert line breaks every n characters
insertLineBreaks : Nat -> List Char -> List Char
insertLineBreaks _ [] = []
insertLineBreaks Z cs = cs
insertLineBreaks n cs = go n cs
  where
    go : Nat -> List Char -> List Char
    go _ [] = []
    go Z remaining = '\r' :: '\n' :: go n remaining
    go (S k) (c :: rest) = c :: go k rest

||| Insert CRLF line breaks for MIME encoding
addMIMELineBreaks : String -> String
addMIMELineBreaks s =
  pack (insertLineBreaks mimeLineLength (unpack s))

--------------------------------------------------------------------------------
-- Main Encoding Functions
--------------------------------------------------------------------------------

||| Encode bytes to Base64 with the given variant
|||
||| This is the primary encoding function.
public export
encodeBytes : Base64Variant -> List Bits8 -> Base64Encoded
encodeBytes variant bytes =
  let encoded = pack (encodeLoop variant bytes)
      withBreaks = case variant of
                     MIME => addMIMELineBreaks encoded
                     _ => encoded
  in MkBase64Encoded variant withBreaks (length bytes)

||| Encode bytes to Base64 string
public export
encodeBytesToString : Base64Variant -> List Bits8 -> String
encodeBytesToString variant bytes = (encodeBytes variant bytes).encoded

||| Encode a string to Base64 (UTF-8 bytes)
public export
encodeString : Base64Variant -> String -> Base64Encoded
encodeString variant s =
  let bytes = map (cast . ord) (unpack s)
  in encodeBytes variant bytes

||| Encode a string to Base64 string
public export
encodeStringToString : Base64Variant -> String -> String
encodeStringToString variant s = (encodeString variant s).encoded

--------------------------------------------------------------------------------
-- Variant-Specific Encoding
--------------------------------------------------------------------------------

||| Encode to standard Base64
public export
encodeStandard : List Bits8 -> Base64Encoded
encodeStandard = encodeBytes Standard

||| Encode string to standard Base64
public export
encodeStandardString : String -> String
encodeStandardString = encodeStringToString Standard

||| Encode to URL-safe Base64
public export
encodeURLSafe : List Bits8 -> Base64Encoded
encodeURLSafe = encodeBytes URLSafe

||| Encode string to URL-safe Base64
public export
encodeURLSafeString : String -> String
encodeURLSafeString = encodeStringToString URLSafe

||| Encode to URL-safe Base64 without padding
public export
encodeURLSafeNoPad : List Bits8 -> Base64Encoded
encodeURLSafeNoPad = encodeBytes URLSafeNoPad

||| Encode string to URL-safe Base64 without padding
public export
encodeURLSafeNoPadString : String -> String
encodeURLSafeNoPadString = encodeStringToString URLSafeNoPad

||| Encode to MIME Base64 (with line breaks)
public export
encodeMIME : List Bits8 -> Base64Encoded
encodeMIME = encodeBytes MIME

||| Encode string to MIME Base64
public export
encodeMIMEString : String -> String
encodeMIMEString = encodeStringToString MIME

--------------------------------------------------------------------------------
-- With Options
--------------------------------------------------------------------------------

||| Encode with explicit options
public export
encodeWithOptions : EncodeOptions -> List Bits8 -> Base64Encoded
encodeWithOptions opts = encodeBytes opts.variant

||| Encode string with explicit options
public export
encodeStringWithOptions : EncodeOptions -> String -> Base64Encoded
encodeStringWithOptions opts = encodeString opts.variant

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Convert integer to bytes (big-endian)
public export
intToBytes : Integer -> List Bits8
intToBytes 0 = [0]
intToBytes n =
  let unsigned = if n < 0 then cast {to=Integer} (the Bits64 (cast n)) else n
  in reverse (go unsigned)
  where
    go : Integer -> List Bits8
    go 0 = []
    go x = cast (x `mod` 256) :: go (x `div` 256)

||| Convert Nat to bytes (big-endian)
public export
natToBytes : Nat -> List Bits8
natToBytes = intToBytes . cast

||| Encode an integer as Base64
public export
encodeInteger : Base64Variant -> Integer -> String
encodeInteger variant n = encodeBytesToString variant (intToBytes n)

||| Encode a Nat as Base64
public export
encodeNat : Base64Variant -> Nat -> String
encodeNat variant n = encodeBytesToString variant (natToBytes n)

--------------------------------------------------------------------------------
-- Chunked Encoding
--------------------------------------------------------------------------------

||| Encode in chunks (for large data)
public export
encodeChunked : Base64Variant -> Nat -> List Bits8 -> List String
encodeChunked variant chunkSize bytes =
  if chunkSize == 0
    then [encodeBytesToString variant bytes]
    else go bytes
  where
    takeChunk : Nat -> List a -> (List a, List a)
    takeChunk Z xs = ([], xs)
    takeChunk (S k) [] = ([], [])
    takeChunk (S k) (x :: xs) =
      let (taken, rest) = takeChunk k xs
      in (x :: taken, rest)

    go : List Bits8 -> List String
    go [] = []
    go remaining =
      let (chunk, rest) = takeChunk chunkSize remaining
      in encodeBytesToString variant chunk :: go rest

--------------------------------------------------------------------------------
-- Verification Helpers
--------------------------------------------------------------------------------

||| Check if output has correct length
public export
hasCorrectLength : Base64Encoded -> Bool
hasCorrectLength b =
  let expected = encodedLength b.variant b.originalLength
      -- For MIME, account for line breaks
      actualClean = case b.variant of
                      MIME => length (filter (\c => c /= '\r' && c /= '\n') (unpack b.encoded))
                      _ => length (unpack b.encoded)
  in actualClean == expected

||| Get the encoded string without line breaks
public export
getEncodedClean : Base64Encoded -> String
getEncodedClean b = case b.variant of
  MIME => pack (filter (\c => c /= '\r' && c /= '\n') (unpack b.encoded))
  _ => b.encoded

--------------------------------------------------------------------------------
-- Streaming Interface (For Future Use)
--------------------------------------------------------------------------------

||| State for streaming encoder
public export
record EncoderState where
  constructor MkEncoderState
  variant : Base64Variant
  buffer : List Bits8
  output : List Char
  bytesProcessed : Nat

||| Initialize encoder state
public export
initEncoder : Base64Variant -> EncoderState
initEncoder variant = MkEncoderState variant [] [] 0

||| Feed bytes to encoder
public export
feedEncoder : EncoderState -> List Bits8 -> EncoderState
feedEncoder state newBytes =
  let allBytes = state.buffer ++ newBytes
      (complete, remaining) = splitAtMultiple 3 allBytes
      encoded = encodeLoop state.variant complete
  in { buffer := remaining
     , output := state.output ++ encoded
     , bytesProcessed := state.bytesProcessed + length complete
     } state
  where
    splitAtMultiple : Nat -> List a -> (List a, List a)
    splitAtMultiple n xs =
      let fullGroups = (length xs `div` n) * n
      in splitAt fullGroups xs

||| Finalize encoder (flush remaining bytes)
public export
finalizeEncoder : EncoderState -> Base64Encoded
finalizeEncoder state =
  let final = encodeLoop state.variant state.buffer
      fullOutput = state.output ++ final
      encoded = pack fullOutput
      withBreaks = case state.variant of
                     MIME => addMIMELineBreaks encoded
                     _ => encoded
  in MkBase64Encoded state.variant withBreaks (state.bytesProcessed + length state.buffer)
