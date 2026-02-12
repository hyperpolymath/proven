-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe Base64 decoding
|||
||| This module provides safe Base64 decoding operations that:
||| - Return Result types instead of throwing exceptions
||| - Validate input before decoding
||| - Support multiple Base64 variants
||| - Handle whitespace and padding correctly
module Proven.SafeBase64.Decode

import Proven.Core
import Proven.SafeBase64.Types
import Data.List
import Data.String
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- Input Preprocessing
--------------------------------------------------------------------------------

||| Remove whitespace from input (for MIME and lenient decoding)
stripWhitespace : String -> String
stripWhitespace s = pack (filter (not . isBase64Whitespace) (unpack s))

||| Count padding characters at end
countPadding : String -> Nat
countPadding s = go (reverse (unpack s))
  where
    go : List Char -> Nat
    go [] = 0
    go ('=' :: rest) = S (go rest)
    go _ = 0

||| Remove padding from end
stripPadding : String -> String
stripPadding s = pack (reverse (dropWhile (== '=') (reverse (unpack s))))

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Validate that all characters are valid for the variant
validateChars : Base64Variant -> String -> Base64Result ()
validateChars variant s = go 0 (unpack s)
  where
    go : Nat -> List Char -> Base64Result ()
    go _ [] = Ok ()
    go pos (c :: cs) =
      if isValidBase64Char variant c || isPaddingChar c
        then go (S pos) cs
        else Err (InvalidCharacter c pos)

||| Validate padding is correct
validatePaddingCount : Base64Variant -> String -> Base64Result Nat
validatePaddingCount variant s =
  let padding = countPadding s
  in if padding > 2
       then Err InvalidPadding
       else if variant == URLSafeNoPad && padding > 0
         then Err UnexpectedPadding
         else Ok padding

||| Validate length (must be multiple of 4 for padded variants, after adding implicit padding)
validateLength : Base64Variant -> String -> Base64Result ()
validateLength variant s =
  let len = length (unpack s)
  in if usesPadding variant
       then if len `mod` 4 == 0
              then Ok ()
              else Err (InvalidLength len)
       else Ok ()  -- Non-padded can be any valid length

--------------------------------------------------------------------------------
-- Core Decoding
--------------------------------------------------------------------------------

||| Decode a group of 4 characters to 3 bytes
decodeGroup : Base64Variant -> Char -> Char -> Char -> Char -> Maybe (List Bits8)
decodeGroup variant c0 c1 c2 c3 = do
  v0 <- charToValue variant c0
  v1 <- charToValue variant c1
  -- Handle padding
  case (c2 == '=', c3 == '=') of
    (True, True) => do
      -- 2 padding chars = 1 output byte
      let byte0 = (v0 `shiftL` 2) .|. (v1 `shiftR` 4)
      -- Check trailing bits are zero
      if (v1 .&. 0x0F) /= 0
        then Nothing
        else Just [byte0]
    (False, True) => do
      -- 1 padding char = 2 output bytes
      v2 <- charToValue variant c2
      let byte0 = (v0 `shiftL` 2) .|. (v1 `shiftR` 4)
          byte1 = (v1 `shiftL` 4) .|. (v2 `shiftR` 2)
      -- Check trailing bits are zero
      if (v2 .&. 0x03) /= 0
        then Nothing
        else Just [byte0, byte1]
    (False, False) => do
      -- No padding = 3 output bytes
      v2 <- charToValue variant c2
      v3 <- charToValue variant c3
      let byte0 = (v0 `shiftL` 2) .|. (v1 `shiftR` 4)
          byte1 = (v1 `shiftL` 4) .|. (v2 `shiftR` 2)
          byte2 = (v2 `shiftL` 6) .|. v3
      Just [byte0, byte1, byte2]
    (True, False) =>
      -- Invalid: padding in wrong position
      Nothing

||| Decode without padding (add implicit padding)
decodeNoPadding : Base64Variant -> List Char -> Base64Result (List Bits8)
decodeNoPadding variant chars =
  let remainder = length chars `mod` 4
      paddedChars = case remainder of
                      0 => chars
                      2 => chars ++ ['=', '=']
                      3 => chars ++ ['=']
                      _ => chars  -- 1 is invalid
  in if remainder == 1
       then Err (InvalidLength (length chars))
       else decodeLoop variant paddedChars

||| Main decoding loop
decodeLoop : Base64Variant -> List Char -> Base64Result (List Bits8)
decodeLoop variant [] = Ok []
decodeLoop variant [_] = Err (InvalidLength 1)
decodeLoop variant [_, _] = Err (InvalidLength 2)
decodeLoop variant [_, _, _] = Err (InvalidLength 3)
decodeLoop variant (c0 :: c1 :: c2 :: c3 :: rest) =
  case decodeGroup variant c0 c1 c2 c3 of
    Nothing => Err NonZeroTrailingBits
    Just bytes =>
      if c3 == '=' || c2 == '='
        then
          -- Padding means end of data
          if null rest
            then Ok bytes
            else Err InvalidPadding  -- Padding in middle
        else do
          restBytes <- decodeLoop variant rest
          Ok (bytes ++ restBytes)

--------------------------------------------------------------------------------
-- Main Decoding Functions
--------------------------------------------------------------------------------

||| Decode Base64 to bytes
|||
||| This is the primary decoding function.
public export
decodeToBytes : Base64Variant -> String -> Base64Result DecodedBytes
decodeToBytes variant input = do
  -- Preprocess based on variant
  let clean = case variant of
                MIME => stripWhitespace input
                _ => input
  -- Validate characters
  validateChars variant clean
  -- Validate padding
  padding <- validatePaddingCount variant clean
  -- For non-padded variants, we don't require padding
  let processedInput = if usesPadding variant
                         then clean
                         else stripPadding clean
  -- Validate length for padded variants
  when (usesPadding variant) $ validateLength variant processedInput
  -- Decode
  let chars = unpack processedInput
  bytes <- if usesPadding variant
             then decodeLoop variant chars
             else decodeNoPadding variant chars
  Ok (mkDecodedBytes bytes)

||| Decode Base64 to byte list
public export
decode : Base64Variant -> String -> Base64Result (List Bits8)
decode variant input = map (.bytes) (decodeToBytes variant input)

||| Decode Base64 to string (interpreting bytes as UTF-8)
public export
decodeToString : Base64Variant -> String -> Base64Result String
decodeToString variant input = do
  bytes <- decode variant input
  Ok (pack (map (chr . cast) bytes))

--------------------------------------------------------------------------------
-- Variant-Specific Decoding
--------------------------------------------------------------------------------

||| Decode standard Base64
public export
decodeStandard : String -> Base64Result (List Bits8)
decodeStandard = decode Standard

||| Decode standard Base64 to string
public export
decodeStandardToString : String -> Base64Result String
decodeStandardToString = decodeToString Standard

||| Decode URL-safe Base64
public export
decodeURLSafe : String -> Base64Result (List Bits8)
decodeURLSafe = decode URLSafe

||| Decode URL-safe Base64 to string
public export
decodeURLSafeToString : String -> Base64Result String
decodeURLSafeToString = decodeToString URLSafe

||| Decode URL-safe Base64 without padding
public export
decodeURLSafeNoPad : String -> Base64Result (List Bits8)
decodeURLSafeNoPad = decode URLSafeNoPad

||| Decode URL-safe Base64 without padding to string
public export
decodeURLSafeNoPadToString : String -> Base64Result String
decodeURLSafeNoPadToString = decodeToString URLSafeNoPad

||| Decode MIME Base64 (ignores whitespace)
public export
decodeMIME : String -> Base64Result (List Bits8)
decodeMIME = decode MIME

||| Decode MIME Base64 to string
public export
decodeMIMEToString : String -> Base64Result String
decodeMIMEToString = decodeToString MIME

--------------------------------------------------------------------------------
-- With Options
--------------------------------------------------------------------------------

||| Decode with explicit options
public export
decodeWithOptions : DecodeOptions -> String -> Base64Result DecodedBytes
decodeWithOptions opts input =
  let preprocessed = if opts.ignoreWhitespace
                       then stripWhitespace input
                       else input
  in decodeToBytes opts.variant preprocessed

--------------------------------------------------------------------------------
-- Lenient Decoding
--------------------------------------------------------------------------------

||| Try to decode, auto-detecting variant
public export
decodeLenient : String -> Base64Result (List Bits8)
decodeLenient input =
  -- Try standard first
  case decode Standard input of
    Ok bytes => Ok bytes
    Err _ =>
      -- Try URL-safe
      case decode URLSafe input of
        Ok bytes => Ok bytes
        Err _ =>
          -- Try URL-safe no padding
          case decode URLSafeNoPad input of
            Ok bytes => Ok bytes
            Err e =>
              -- Try MIME (with whitespace)
              case decode MIME input of
                Ok bytes => Ok bytes
                Err _ => Err e  -- Return original error

||| Decode ignoring invalid characters (best effort)
public export
decodeBestEffort : Base64Variant -> String -> DecodedBytes
decodeBestEffort variant input =
  let clean = pack (filter (isValidBase64Char variant) (unpack input))
      padded = addPaddingIfNeeded clean
  in case decodeToBytes variant padded of
       Ok bytes => bytes
       Err _ => mkDecodedBytes []
  where
    addPaddingIfNeeded : String -> String
    addPaddingIfNeeded s =
      let len = length (unpack s)
          remainder = len `mod` 4
      in case remainder of
           0 => s
           2 => s ++ "=="
           3 => s ++ "="
           _ => s

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Decode Base64 to integer (big-endian)
public export
decodeToInteger : Base64Variant -> String -> Base64Result Integer
decodeToInteger variant input = do
  bytes <- decode variant input
  Ok (bytesToInteger bytes)
  where
    bytesToInteger : List Bits8 -> Integer
    bytesToInteger [] = 0
    bytesToInteger bs = foldl (\acc, b => acc * 256 + cast b) 0 bs

||| Decode Base64 to Nat (big-endian)
public export
decodeToNat : Base64Variant -> String -> Base64Result Nat
decodeToNat variant input = do
  n <- decodeToInteger variant input
  if n < 0
    then Ok 0
    else Ok (cast n)

||| Check if a string is valid Base64
public export
isValidBase64 : Base64Variant -> String -> Bool
isValidBase64 variant input = isOk (decode variant input)

||| Check if a string looks like Base64 (quick check)
public export
looksLikeBase64 : String -> Bool
looksLikeBase64 s =
  let len = length (unpack s)
  in len > 0 && all isValidChar (unpack s)
  where
    isValidChar : Char -> Bool
    isValidChar c = isStandardBase64Char c || isURLSafeBase64Char c || isBase64Whitespace c

--------------------------------------------------------------------------------
-- Streaming Interface (For Future Use)
--------------------------------------------------------------------------------

||| State for streaming decoder
public export
record DecoderState where
  constructor MkDecoderState
  variant : Base64Variant
  buffer : List Char
  output : List Bits8
  hasError : Maybe Base64Error

||| Initialize decoder state
public export
initDecoder : Base64Variant -> DecoderState
initDecoder variant = MkDecoderState variant [] [] Nothing

||| Feed characters to decoder
public export
feedDecoder : DecoderState -> String -> DecoderState
feedDecoder state input =
  case state.hasError of
    Just _ => state  -- Already errored
    Nothing =>
      let allChars = state.buffer ++ unpack input
          (complete, remaining) = splitAtMultiple 4 allChars
      in case decodeLoop state.variant complete of
           Err e => { hasError := Just e } state
           Ok bytes =>
             { buffer := remaining
             , output := state.output ++ bytes
             } state
  where
    splitAtMultiple : Nat -> List a -> (List a, List a)
    splitAtMultiple n xs =
      let fullGroups = (length xs `div` n) * n
      in splitAt fullGroups xs

||| Finalize decoder
public export
finalizeDecoder : DecoderState -> Base64Result DecodedBytes
finalizeDecoder state =
  case state.hasError of
    Just e => Err e
    Nothing =>
      if null state.buffer
        then Ok (mkDecodedBytes state.output)
        else
          -- Process remaining with padding
          case decodeNoPadding state.variant state.buffer of
            Err e => Err e
            Ok bytes => Ok (mkDecodedBytes (state.output ++ bytes))
