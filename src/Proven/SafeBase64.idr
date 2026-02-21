-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeBase64 - Base64 encoding/decoding that cannot crash
|||
||| This module provides safe Base64 operations including:
||| - Standard Base64 (RFC 4648)
||| - URL-safe Base64 (RFC 4648 Section 5)
||| - MIME Base64 (RFC 2045)
||| - Formal proofs of correctness
|||
||| Example usage:
||| ```idris
||| -- Encode a string
||| let encoded = encode "Hello, World!"
||| -- "SGVsbG8sIFdvcmxkIQ=="
|||
||| -- Decode back to string
||| result <- decodeString encoded
||| -- Ok "Hello, World!"
|||
||| -- URL-safe encoding
||| let urlSafe = encodeURLSafe myBytes
|||
||| -- MIME encoding with line breaks
||| let mime = encodeMIME largeData
||| ```
module Proven.SafeBase64

import public Proven.Core
import public Proven.SafeBase64.Types
import public Proven.SafeBase64.Encode
import public Proven.SafeBase64.Decode
import public Proven.SafeBase64.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Encode a string to Base64 (standard)
|||
||| This is the simplest encoding function for common use cases.
public export
encode : String -> String
encode = encodeStandardString

||| Decode Base64 to string (standard)
|||
||| This is the simplest decoding function for common use cases.
public export
decodeString : String -> Base64Result String
decodeString = decodeStandardToString

||| Encode bytes to Base64 (standard)
public export
encodeRaw : List Bits8 -> String
encodeRaw = encodeBytesToString Standard

||| Decode Base64 to bytes (standard)
public export
decodeRaw : String -> Base64Result (List Bits8)
decodeRaw = decodeStandard

--------------------------------------------------------------------------------
-- URL-Safe Shortcuts
--------------------------------------------------------------------------------

||| Encode string for URL use (no padding)
public export
encodeForURL : String -> String
encodeForURL = encodeURLSafeNoPadString

||| Decode URL-safe Base64 string
public export
decodeFromURL : String -> Base64Result String
decodeFromURL = decodeURLSafeNoPadToString

||| Encode bytes for URL use (no padding)
public export
encodeForURLRaw : List Bits8 -> String
encodeForURLRaw = encodeBytesToString URLSafeNoPad

||| Decode URL-safe Base64 to bytes
public export
decodeFromURLRaw : String -> Base64Result (List Bits8)
decodeFromURLRaw = decodeURLSafeNoPad

--------------------------------------------------------------------------------
-- Data URI Helpers
--------------------------------------------------------------------------------

||| Create a data URI from MIME type and bytes
public export
toDataURI : String -> List Bits8 -> String
toDataURI mimeType bytes =
  "data:" ++ mimeType ++ ";base64," ++ encodeRaw bytes

||| Create a data URI from MIME type and string content
public export
toDataURIString : String -> String -> String
toDataURIString mimeType content =
  "data:" ++ mimeType ++ ";base64," ++ encode content

||| Parse a data URI and decode its content
public export
fromDataURI : String -> Base64Result (String, List Bits8)
fromDataURI uri =
  if not (isPrefixOf "data:" uri)
    then Err (InvalidCharacter 'd' 0)
    else
      let rest = strSubstr 5 (cast (length uri - 5)) uri
      in case break (== ',') (unpack rest) of
           (metaPart, ',' :: base64Part) =>
             let meta = pack metaPart
                 (mimeType, _) = break (== ';') (unpack meta)
             in do
               bytes <- decodeRaw (pack base64Part)
               Ok (pack mimeType, bytes)
           _ => Err (InvalidCharacter ',' 0)

--------------------------------------------------------------------------------
-- JWT/Token Helpers
--------------------------------------------------------------------------------

||| Encode JSON for JWT (URL-safe, no padding)
public export
encodeJWTSegment : String -> String
encodeJWTSegment = encodeForURL

||| Decode JWT segment
public export
decodeJWTSegment : String -> Base64Result String
decodeJWTSegment = decodeFromURL

--------------------------------------------------------------------------------
-- Hex Conversion
--------------------------------------------------------------------------------

||| Convert hex string to Base64
public export
hexToBase64 : String -> Base64Result String
hexToBase64 hex = do
  bytes <- hexToBytes hex
  Ok (encodeRaw bytes)
  where
    hexCharToNibble : Char -> Maybe Bits8
    hexCharToNibble c =
      if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
      else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
      else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
      else Nothing

    hexToBytes : String -> Base64Result (List Bits8)
    hexToBytes s = go 0 (unpack s)
      where
        go : Nat -> List Char -> Base64Result (List Bits8)
        go _ [] = Ok []
        go pos [c] = Err (InvalidLength (S pos))  -- Odd length
        go pos (c1 :: c2 :: rest) =
          case (hexCharToNibble c1, hexCharToNibble c2) of
            (Just h, Just l) => do
              restBytes <- go (pos + 2) rest
              Ok ((h `shiftL` 4 .|. l) :: restBytes)
            (Nothing, _) => Err (InvalidCharacter c1 pos)
            (_, Nothing) => Err (InvalidCharacter c2 (pos + 1))

||| Convert Base64 to hex string
public export
base64ToHex : String -> Base64Result String
base64ToHex b64 = do
  bytes <- decodeRaw b64
  Ok (bytesToHex bytes)
  where
    nibbleToHex : Bits8 -> Char
    nibbleToHex n =
      if n < 10 then chr (ord '0' + cast n)
      else chr (ord 'a' + cast n - 10)

    bytesToHex : List Bits8 -> String
    bytesToHex bs = pack (concatMap byteToHex bs)
      where
        byteToHex : Bits8 -> List Char
        byteToHex b = [nibbleToHex (b `shiftR` 4), nibbleToHex (b .&. 0x0F)]

--------------------------------------------------------------------------------
-- Comparison Functions
--------------------------------------------------------------------------------

||| Compare two Base64 strings for equality (handles different variants)
public export
base64Equal : String -> String -> Bool
base64Equal a b =
  case (decodeLenient a, decodeLenient b) of
    (Ok bytesA, Ok bytesB) => bytesA == bytesB
    _ => False

||| Check if a string is valid Base64 (any variant)
public export
isBase64 : String -> Bool
isBase64 s = isOk (decodeLenient s)

||| Get the detected variant of a Base64 string
public export
detectVariant : String -> Maybe Base64Variant
detectVariant s =
  let hasPlus = '+' `elem` unpack s
      hasSlash = '/' `elem` unpack s
      hasDash = '-' `elem` unpack s
      hasUnderscore = '_' `elem` unpack s
      hasPadding = '=' `elem` unpack s
      hasNewline = '\n' `elem` unpack s || '\r' `elem` unpack s
  in if hasNewline then Just MIME
     else if hasPlus || hasSlash then Just Standard
     else if hasDash || hasUnderscore then
       if hasPadding then Just URLSafe else Just URLSafeNoPad
     else if hasPadding then Just Standard
     else Just URLSafeNoPad  -- Ambiguous, default to no-pad

--------------------------------------------------------------------------------
-- Streaming/Chunked Operations
--------------------------------------------------------------------------------

||| Encode large data in chunks
public export
encodeStreaming : Base64Variant -> List (List Bits8) -> String
encodeStreaming variant chunks =
  let state = foldl feedEncoder (initEncoder variant) chunks
  in (finalizeEncoder state).encoded

||| Decode large data in chunks
public export
decodeStreaming : Base64Variant -> List String -> Base64Result (List Bits8)
decodeStreaming variant chunks =
  let state = foldl feedDecoder (initDecoder variant) chunks
  in map (.bytes) (finalizeDecoder state)

--------------------------------------------------------------------------------
-- Safe Conversion Utilities
--------------------------------------------------------------------------------

||| Safely convert between Base64 variants
public export
convertVariant : Base64Variant -> Base64Variant -> String -> Base64Result String
convertVariant fromVariant toVariant input = do
  bytes <- decode fromVariant input
  Ok (encodeBytesToString toVariant bytes)

||| Convert standard to URL-safe
public export
standardToURLSafe : String -> Base64Result String
standardToURLSafe = convertVariant Standard URLSafe

||| Convert URL-safe to standard
public export
urlSafeToStandard : String -> Base64Result String
urlSafeToStandard = convertVariant URLSafe Standard

||| Add padding to unpadded Base64
public export
addPadding : String -> String
addPadding s =
  let len = length (unpack s)
      remainder = len `mod` 4
  in case remainder of
       0 => s
       2 => s ++ "=="
       3 => s ++ "="
       _ => s

||| Remove padding from Base64
public export
removePadding : String -> String
removePadding s = pack (reverse (dropWhile (== '=') (reverse (unpack s))))

--------------------------------------------------------------------------------
-- Error Recovery
--------------------------------------------------------------------------------

||| Try to decode with automatic variant detection
public export
decodeAuto : String -> Base64Result (List Bits8)
decodeAuto = decodeLenient

||| Decode with fallback value on error
public export
decodeWithDefault : List Bits8 -> String -> List Bits8
decodeWithDefault def input =
  case decodeAuto input of
    Ok bytes => bytes
    Err _ => def

||| Decode string with fallback
public export
decodeStringWithDefault : String -> String -> String
decodeStringWithDefault def input =
  case decodeString input of
    Ok s => s
    Err _ => def

--------------------------------------------------------------------------------
-- Validation Functions
--------------------------------------------------------------------------------

||| Validate Base64 format without decoding
public export
validateFormat : Base64Variant -> String -> Base64Result ()
validateFormat variant input = do
  let clean = case variant of
                MIME => pack (filter (not . isBase64Whitespace) (unpack input))
                _ => input
  -- Check all characters are valid
  _ <- traverse_ checkChar (zip [0..] (unpack clean))
  -- Check length
  let len = length (unpack clean)
  if usesPadding variant && len `mod` 4 /= 0
    then Err (InvalidLength len)
    else Ok ()
  where
    checkChar : (Nat, Char) -> Base64Result ()
    checkChar (pos, c) =
      if isValidBase64Char variant c || isPaddingChar c
        then Ok ()
        else Err (InvalidCharacter c pos)

||| Get detailed validation result
public export
data ValidationResult : Type where
  Valid : Base64Variant -> Nat -> ValidationResult
  Invalid : Base64Error -> ValidationResult

public export
Show ValidationResult where
  show (Valid v len) = "Valid(" ++ show v ++ ", " ++ show len ++ " bytes)"
  show (Invalid err) = "Invalid: " ++ show err

||| Validate and report result
public export
validate : String -> ValidationResult
validate input =
  case detectVariant input of
    Nothing => Invalid (InvalidCharacter ' ' 0)
    Just variant =>
      case decodeToBytes variant input of
        Err e => Invalid e
        Ok decoded => Valid variant decoded.length

--------------------------------------------------------------------------------
-- Size Utilities
--------------------------------------------------------------------------------

||| Calculate encoded size without encoding
public export
encodedSize : Nat -> Nat
encodedSize = encodedLength Standard

||| Calculate decoded size (estimate)
public export
decodedSize : Nat -> Nat
decodedSize = decodedLength

||| Calculate exact decoded size from Base64 string
public export
exactDecodedSize : String -> Nat
exactDecodedSize s =
  let len = length (unpack s)
      padding = length (filter (== '=') (unpack s))
  in exactDecodedLength len padding

--------------------------------------------------------------------------------
-- Debugging Helpers
--------------------------------------------------------------------------------

||| Pretty-print Base64 info
public export
base64Info : String -> String
base64Info input =
  let variant = detectVariant input
      len = length (unpack input)
      validation = validate input
  in "Base64 Info:\n" ++
     "  Input length: " ++ show len ++ " chars\n" ++
     "  Detected variant: " ++ show variant ++ "\n" ++
     "  Validation: " ++ show validation

||| Show first N bytes of decoded content
public export
peekDecoded : Nat -> String -> String
peekDecoded n input =
  case decodeAuto input of
    Err e => "Error: " ++ show e
    Ok bytes =>
      let preview = take n bytes
      in "Decoded " ++ show (length bytes) ++ " bytes: " ++
         show (map cast preview : List Int)
