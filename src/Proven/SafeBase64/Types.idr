-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Base64 types and encoding variants
|||
||| This module defines types for safe Base64 encoding/decoding including:
||| - Standard Base64 (RFC 4648)
||| - URL-safe Base64 (RFC 4648 Section 5)
||| - MIME Base64 with line breaks
||| - Length-correct encoded/decoded values
module Proven.SafeBase64.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Base64 Variants
--------------------------------------------------------------------------------

||| Base64 encoding variants
public export
data Base64Variant : Type where
  ||| Standard Base64 (RFC 4648)
  ||| Uses: A-Z, a-z, 0-9, +, /
  ||| Padding: = (required)
  Standard : Base64Variant

  ||| URL-safe Base64 (RFC 4648 Section 5)
  ||| Uses: A-Z, a-z, 0-9, -, _
  ||| Padding: = (optional)
  URLSafe : Base64Variant

  ||| URL-safe without padding
  ||| Uses: A-Z, a-z, 0-9, -, _
  ||| Padding: none
  URLSafeNoPad : Base64Variant

  ||| MIME Base64 (RFC 2045)
  ||| Uses: A-Z, a-z, 0-9, +, /
  ||| Padding: = (required)
  ||| Line breaks every 76 characters
  MIME : Base64Variant

public export
Eq Base64Variant where
  Standard == Standard = True
  URLSafe == URLSafe = True
  URLSafeNoPad == URLSafeNoPad = True
  MIME == MIME = True
  _ == _ = False

public export
Show Base64Variant where
  show Standard = "Standard"
  show URLSafe = "URLSafe"
  show URLSafeNoPad = "URLSafeNoPad"
  show MIME = "MIME"

--------------------------------------------------------------------------------
-- Alphabets
--------------------------------------------------------------------------------

||| Standard Base64 alphabet
public export
standardAlphabet : String
standardAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

||| URL-safe Base64 alphabet
public export
urlSafeAlphabet : String
urlSafeAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

||| Get alphabet for variant
public export
getAlphabet : Base64Variant -> String
getAlphabet Standard = standardAlphabet
getAlphabet URLSafe = urlSafeAlphabet
getAlphabet URLSafeNoPad = urlSafeAlphabet
getAlphabet MIME = standardAlphabet

||| Whether variant uses padding
public export
usesPadding : Base64Variant -> Bool
usesPadding Standard = True
usesPadding URLSafe = True
usesPadding URLSafeNoPad = False
usesPadding MIME = True

||| MIME line length
public export
mimeLineLength : Nat
mimeLineLength = 76

--------------------------------------------------------------------------------
-- Encoded/Decoded Types
--------------------------------------------------------------------------------

||| A validated Base64 encoded string
|||
||| Invariant: Contains only valid Base64 characters for the variant
public export
record Base64Encoded where
  constructor MkBase64Encoded
  variant : Base64Variant
  encoded : String
  ||| Original byte length (before encoding)
  originalLength : Nat

public export
Show Base64Encoded where
  show b = "Base64Encoded(" ++ show b.variant ++ ", \"" ++ b.encoded ++ "\")"

public export
Eq Base64Encoded where
  b1 == b2 = b1.variant == b2.variant && b1.encoded == b2.encoded

||| Raw bytes (decoded data)
public export
record DecodedBytes where
  constructor MkDecodedBytes
  bytes : List Bits8
  length : Nat

public export
Show DecodedBytes where
  show d = "DecodedBytes(" ++ show d.length ++ " bytes)"

public export
Eq DecodedBytes where
  d1 == d2 = d1.bytes == d2.bytes

||| Create DecodedBytes with correct length
public export
mkDecodedBytes : List Bits8 -> DecodedBytes
mkDecodedBytes bs = MkDecodedBytes bs (length bs)

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Base64 decoding errors
public export
data Base64Error : Type where
  ||| Invalid character in input
  InvalidCharacter : (char : Char) -> (position : Nat) -> Base64Error

  ||| Invalid padding
  InvalidPadding : Base64Error

  ||| Invalid length (not multiple of 4 for padded variants)
  InvalidLength : (len : Nat) -> Base64Error

  ||| Unexpected padding for no-pad variant
  UnexpectedPadding : Base64Error

  ||| Non-zero trailing bits (invalid encoding)
  NonZeroTrailingBits : Base64Error

public export
Show Base64Error where
  show (InvalidCharacter c pos) = "InvalidCharacter: '" ++ singleton c ++ "' at position " ++ show pos
  show InvalidPadding = "InvalidPadding: incorrect padding characters"
  show (InvalidLength len) = "InvalidLength: " ++ show len ++ " is not valid"
  show UnexpectedPadding = "UnexpectedPadding: padding not allowed in this variant"
  show NonZeroTrailingBits = "NonZeroTrailingBits: trailing bits must be zero"

public export
Eq Base64Error where
  InvalidCharacter c1 p1 == InvalidCharacter c2 p2 = c1 == c2 && p1 == p2
  InvalidPadding == InvalidPadding = True
  InvalidLength l1 == InvalidLength l2 = l1 == l2
  UnexpectedPadding == UnexpectedPadding = True
  NonZeroTrailingBits == NonZeroTrailingBits = True
  _ == _ = False

--------------------------------------------------------------------------------
-- Validation Predicates
--------------------------------------------------------------------------------

||| Check if a character is valid for standard Base64
public export
isStandardBase64Char : Char -> Bool
isStandardBase64Char c =
  (c >= 'A' && c <= 'Z') ||
  (c >= 'a' && c <= 'z') ||
  (c >= '0' && c <= '9') ||
  c == '+' || c == '/' || c == '='

||| Check if a character is valid for URL-safe Base64
public export
isURLSafeBase64Char : Char -> Bool
isURLSafeBase64Char c =
  (c >= 'A' && c <= 'Z') ||
  (c >= 'a' && c <= 'z') ||
  (c >= '0' && c <= '9') ||
  c == '-' || c == '_' || c == '='

||| Check if a character is valid for a variant (excluding padding)
public export
isValidBase64Char : Base64Variant -> Char -> Bool
isValidBase64Char Standard c = isStandardBase64Char c && c /= '='
isValidBase64Char URLSafe c = isURLSafeBase64Char c && c /= '='
isValidBase64Char URLSafeNoPad c = isURLSafeBase64Char c && c /= '='
isValidBase64Char MIME c = isStandardBase64Char c && c /= '=' || c == '\n' || c == '\r'

||| Check if a character is whitespace (for MIME)
public export
isBase64Whitespace : Char -> Bool
isBase64Whitespace c = c == '\n' || c == '\r' || c == ' ' || c == '\t'

--------------------------------------------------------------------------------
-- Length Calculations
--------------------------------------------------------------------------------

||| Calculate encoded length from input byte length
|||
||| For padded variants: ceil(n/3) * 4
||| For non-padded: ceil(n * 4 / 3)
public export
encodedLength : Base64Variant -> (inputLength : Nat) -> Nat
encodedLength variant n =
  if usesPadding variant
    then ((n + 2) `div` 3) * 4
    else
      let base = (n * 4) `div` 3
          remainder = (n * 4) `mod` 3
      in if remainder == 0 then base else base + 1

||| Calculate decoded length from encoded length
|||
||| This is an estimate; actual length depends on padding
public export
decodedLength : (encodedLength : Nat) -> Nat
decodedLength n = (n * 3) `div` 4

||| Calculate exact decoded length accounting for padding
public export
exactDecodedLength : (encodedLength : Nat) -> (paddingCount : Nat) -> Nat
exactDecodedLength n padding = (n * 3) `div` 4 - padding

--------------------------------------------------------------------------------
-- Encoding Options
--------------------------------------------------------------------------------

||| Options for Base64 encoding
public export
record EncodeOptions where
  constructor MkEncodeOptions
  ||| Which Base64 variant to use
  variant : Base64Variant
  ||| Line length for MIME (ignored for other variants)
  lineLength : Nat

||| Default encoding options (Standard Base64)
public export
defaultEncodeOptions : EncodeOptions
defaultEncodeOptions = MkEncodeOptions Standard 0

||| URL-safe encoding options
public export
urlSafeOptions : EncodeOptions
urlSafeOptions = MkEncodeOptions URLSafe 0

||| URL-safe no padding options
public export
urlSafeNoPadOptions : EncodeOptions
urlSafeNoPadOptions = MkEncodeOptions URLSafeNoPad 0

||| MIME encoding options
public export
mimeOptions : EncodeOptions
mimeOptions = MkEncodeOptions MIME mimeLineLength

--------------------------------------------------------------------------------
-- Decoding Options
--------------------------------------------------------------------------------

||| Options for Base64 decoding
public export
record DecodeOptions where
  constructor MkDecodeOptions
  ||| Which Base64 variant to expect
  variant : Base64Variant
  ||| Whether to ignore whitespace
  ignoreWhitespace : Bool
  ||| Whether to be strict about padding
  strictPadding : Bool

||| Default decoding options
public export
defaultDecodeOptions : DecodeOptions
defaultDecodeOptions = MkDecodeOptions Standard False True

||| Lenient decoding options (ignore whitespace, relaxed padding)
public export
lenientDecodeOptions : DecodeOptions
lenientDecodeOptions = MkDecodeOptions Standard True False

||| URL-safe decoding options
public export
urlSafeDecodeOptions : DecodeOptions
urlSafeDecodeOptions = MkDecodeOptions URLSafe False False

||| MIME decoding options (ignore whitespace)
public export
mimeDecodeOptions : DecodeOptions
mimeDecodeOptions = MkDecodeOptions MIME True True

--------------------------------------------------------------------------------
-- Character Index Lookup
--------------------------------------------------------------------------------

||| Get the 6-bit value for a Base64 character
public export
charToValue : Base64Variant -> Char -> Maybe Bits8
charToValue variant c =
  if c >= 'A' && c <= 'Z' then Just (cast (ord c - ord 'A'))
  else if c >= 'a' && c <= 'z' then Just (cast (ord c - ord 'a' + 26))
  else if c >= '0' && c <= '9' then Just (cast (ord c - ord '0' + 52))
  else case variant of
    Standard => if c == '+' then Just 62 else if c == '/' then Just 63 else Nothing
    MIME => if c == '+' then Just 62 else if c == '/' then Just 63 else Nothing
    URLSafe => if c == '-' then Just 62 else if c == '_' then Just 63 else Nothing
    URLSafeNoPad => if c == '-' then Just 62 else if c == '_' then Just 63 else Nothing

||| Get the character for a 6-bit value
public export
valueToChar : Base64Variant -> Bits8 -> Char
valueToChar variant v =
  let alphabet = unpack (getAlphabet variant)
      idx = cast {to=Nat} v
  in case index' idx alphabet of
       Just c => c
       Nothing => 'A'
  where
    index' : Nat -> List a -> Maybe a
    index' _ [] = Nothing
    index' Z (x :: _) = Just x
    index' (S k) (_ :: xs) = index' k xs

--------------------------------------------------------------------------------
-- Padding Character
--------------------------------------------------------------------------------

||| The padding character
public export
paddingChar : Char
paddingChar = '='

||| Check if character is padding
public export
isPaddingChar : Char -> Bool
isPaddingChar c = c == '='

--------------------------------------------------------------------------------
-- Result Type Alias
--------------------------------------------------------------------------------

||| Result type for Base64 operations
public export
Base64Result : Type -> Type
Base64Result = Result Base64Error
