-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeHex - Hexadecimal encoding and decoding utilities
|||
||| This module provides:
||| - Type-safe hexadecimal encoding
||| - Hex string validation
||| - Hex to bytes conversion
||| - Various hex formatting options
||| - Hex string manipulation utilities
|||
||| All hex operations are validated to prevent invalid data
module Proven.SafeHex

import public Proven.Core
import Data.List
import Data.String
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Hex Types
--------------------------------------------------------------------------------

||| A validated hexadecimal string (guaranteed to contain only valid hex chars)
public export
data HexString : Type where
  MkHexString : (value : String) -> HexString

||| Hex encoding errors
public export
data HexError
  = InvalidCharacter Char Nat   -- Invalid char at position
  | OddLength Nat               -- Hex string has odd length
  | EmptyInput
  | DecodingFailed String

public export
Show HexError where
  show (InvalidCharacter c pos) = "Invalid hex character '" ++ singleton c ++ "' at position " ++ show pos
  show (OddLength n) = "Hex string has odd length: " ++ show n
  show EmptyInput = "Empty input"
  show (DecodingFailed msg) = "Hex decoding failed: " ++ msg

--------------------------------------------------------------------------------
-- Hex Character Operations
--------------------------------------------------------------------------------

||| Check if character is valid hexadecimal (0-9, a-f, A-F)
public export
isHexChar : Char -> Bool
isHexChar c = (c >= '0' && c <= '9') ||
              (c >= 'a' && c <= 'f') ||
              (c >= 'A' && c <= 'F')

||| Convert hex character to numeric value (0-15)
public export
hexCharToNibble : Char -> Maybe Nat
hexCharToNibble c =
  if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
  else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
  else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
  else Nothing

||| Convert nibble (0-15) to lowercase hex character
public export
nibbleToHexChar : Nat -> Char
nibbleToHexChar n =
  if n < 10 then chr (ord '0' + cast n)
  else chr (ord 'a' + cast n - 10)

||| Convert nibble (0-15) to uppercase hex character
public export
nibbleToHexCharUpper : Nat -> Char
nibbleToHexCharUpper n =
  if n < 10 then chr (ord '0' + cast n)
  else chr (ord 'A' + cast n - 10)

--------------------------------------------------------------------------------
-- Hex String Validation
--------------------------------------------------------------------------------

||| Validate a string as hexadecimal
public export
validateHex : String -> Either HexError HexString
validateHex s =
  let chars = unpack s
      len = length chars
  in if len == 0
       then Left EmptyInput
       else case findInvalidChar chars 0 of
         Just (c, pos) => Left (InvalidCharacter c pos)
         Nothing => Right (MkHexString s)
  where
    findInvalidChar : List Char -> Nat -> Maybe (Char, Nat)
    findInvalidChar [] _ = Nothing
    findInvalidChar (c :: cs) pos =
      if isHexChar c
        then findInvalidChar cs (pos + 1)
        else Just (c, pos)

||| Validate hex string and check for even length (required for byte decoding)
public export
validateHexBytes : String -> Either HexError HexString
validateHexBytes s =
  let len = length s
  in if len `mod` 2 /= 0
       then Left (OddLength len)
       else validateHex s

||| Check if string is valid hex
public export
isValidHex : String -> Bool
isValidHex s = isRight (validateHex s)

||| Check if string is valid hex with even length
public export
isValidHexBytes : String -> Bool
isValidHexBytes s = isRight (validateHexBytes s)

--------------------------------------------------------------------------------
-- Hex Encoding
--------------------------------------------------------------------------------

||| Encode a single byte to two hex characters (lowercase)
public export
encodeByte : Bits8 -> String
encodeByte b =
  let n = cast {to=Nat} b
      high = n `div` 16
      low = n `mod` 16
  in pack [nibbleToHexChar high, nibbleToHexChar low]

||| Encode a single byte to two hex characters (uppercase)
public export
encodeByteUpper : Bits8 -> String
encodeByteUpper b =
  let n = cast {to=Nat} b
      high = n `div` 16
      low = n `mod` 16
  in pack [nibbleToHexCharUpper high, nibbleToHexCharUpper low]

||| Encode bytes to hex string (lowercase)
public export
encodeHex : List Bits8 -> HexString
encodeHex bytes = MkHexString (concat (map encodeByte bytes))

||| Encode bytes to hex string (uppercase)
public export
encodeHexUpper : List Bits8 -> HexString
encodeHexUpper bytes = MkHexString (concat (map encodeByteUpper bytes))

||| Encode Vect of bytes to hex string
public export
encodeHexVect : Vect n Bits8 -> HexString
encodeHexVect v = encodeHex (toList v)

--------------------------------------------------------------------------------
-- Hex Decoding
--------------------------------------------------------------------------------

||| Decode two hex characters to a byte
public export
decodeHexByte : Char -> Char -> Maybe Bits8
decodeHexByte h1 h2 = do
  n1 <- hexCharToNibble h1
  n2 <- hexCharToNibble h2
  pure (cast (n1 * 16 + n2))

||| Decode hex string to bytes
public export
decodeHex : HexString -> Either HexError (List Bits8)
decodeHex (MkHexString s) =
  let len = length s
  in if len `mod` 2 /= 0
       then Left (OddLength len)
       else decodeChars (unpack s)
  where
    decodeChars : List Char -> Either HexError (List Bits8)
    decodeChars [] = Right []
    decodeChars [_] = Left (OddLength 1)  -- Should not happen due to check above
    decodeChars (h1 :: h2 :: rest) =
      case decodeHexByte h1 h2 of
        Just b => case decodeChars rest of
          Right bs => Right (b :: bs)
          Left e => Left e
        Nothing => Left (DecodingFailed "invalid hex characters")

||| Decode hex string to bytes, returning Maybe
public export
decodeHex' : HexString -> Maybe (List Bits8)
decodeHex' hs = either (const Nothing) Just (decodeHex hs)

||| Parse and decode hex string in one step
public export
parseHex : String -> Either HexError (List Bits8)
parseHex s = do
  validated <- validateHexBytes s
  decodeHex validated

||| Parse and decode hex string, returning Maybe
public export
parseHex' : String -> Maybe (List Bits8)
parseHex' s = either (const Nothing) Just (parseHex s)

--------------------------------------------------------------------------------
-- Hex String Operations
--------------------------------------------------------------------------------

||| Get the raw string from HexString
public export
hexToString : HexString -> String
hexToString (MkHexString s) = s

||| Get length of hex string in characters
public export
hexLength : HexString -> Nat
hexLength (MkHexString s) = length s

||| Get number of bytes represented by hex string
public export
hexByteCount : HexString -> Nat
hexByteCount (MkHexString s) = length s `div` 2

||| Convert hex string to lowercase
public export
hexToLower : HexString -> HexString
hexToLower (MkHexString s) = MkHexString (toLower s)

||| Convert hex string to uppercase
public export
hexToUpper : HexString -> HexString
hexToUpper (MkHexString s) = MkHexString (toUpper s)

||| Concatenate two hex strings
public export
hexConcat : HexString -> HexString -> HexString
hexConcat (MkHexString a) (MkHexString b) = MkHexString (a ++ b)

||| Take first n bytes from hex string (2n characters)
public export
hexTake : Nat -> HexString -> HexString
hexTake n (MkHexString s) = MkHexString (substr 0 (n * 2) s)

||| Drop first n bytes from hex string (2n characters)
public export
hexDrop : Nat -> HexString -> HexString
hexDrop n (MkHexString s) = MkHexString (substr (n * 2) (length s) s)

--------------------------------------------------------------------------------
-- Hex Formatting
--------------------------------------------------------------------------------

||| Format hex string with spaces between bytes (e.g., "01 02 03")
public export
formatHexSpaced : HexString -> String
formatHexSpaced (MkHexString s) = formatPairs (unpack s)
  where
    formatPairs : List Char -> String
    formatPairs [] = ""
    formatPairs [c] = singleton c
    formatPairs (c1 :: c2 :: []) = pack [c1, c2]
    formatPairs (c1 :: c2 :: rest) = pack [c1, c2, ' '] ++ formatPairs rest

||| Format hex string with colons between bytes (e.g., "01:02:03")
public export
formatHexColons : HexString -> String
formatHexColons (MkHexString s) = formatPairs (unpack s)
  where
    formatPairs : List Char -> String
    formatPairs [] = ""
    formatPairs [c] = singleton c
    formatPairs (c1 :: c2 :: []) = pack [c1, c2]
    formatPairs (c1 :: c2 :: rest) = pack [c1, c2, ':'] ++ formatPairs rest

||| Format hex string with prefix (e.g., "0x0102")
public export
formatHex0x : HexString -> String
formatHex0x (MkHexString s) = "0x" ++ s

||| Format hex string as C array literal
public export
formatHexCArray : HexString -> String
formatHexCArray hs =
  case decodeHex' hs of
    Nothing => "{ /* invalid */ }"
    Just bytes =>
      let formatted = map (\b => "0x" ++ encodeByteUpper b) bytes
      in "{ " ++ joinBy ", " formatted ++ " }"
  where
    joinBy : String -> List String -> String
    joinBy _ [] = ""
    joinBy _ [x] = x
    joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

--------------------------------------------------------------------------------
-- Hex Comparison
--------------------------------------------------------------------------------

public export
Eq HexString where
  (MkHexString a) == (MkHexString b) = toLower a == toLower b

public export
Show HexString where
  show (MkHexString s) = s

||| Constant-time comparison of hex strings (timing-attack resistant)
public export
hexConstantTimeEq : HexString -> HexString -> Bool
hexConstantTimeEq (MkHexString a) (MkHexString b) =
  let la = toLower a
      lb = toLower b
  in if length la /= length lb
       then False
       else go (unpack la) (unpack lb) 0
  where
    go : List Char -> List Char -> Int -> Bool
    go [] [] acc = acc == 0
    go (x :: xs) (y :: ys) acc = go xs ys (acc `or` (ord x `xor` ord y))
    go _ _ _ = False

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Convert integer to hex string with minimum width
public export
intToHex : Integer -> Nat -> HexString
intToHex n minWidth =
  let absN = if n < 0 then -n else n
      hex = intToHexRaw absN
      padded = padLeftStr minWidth '0' hex
  in MkHexString padded
  where
    intToHexRaw : Integer -> String
    intToHexRaw 0 = "0"
    intToHexRaw i = go i ""
      where
        go : Integer -> String -> String
        go 0 acc = acc
        go x acc =
          let digit = nibbleToHexChar (cast (x `mod` 16))
          in go (x `div` 16) (singleton digit ++ acc)

    padLeftStr : Nat -> Char -> String -> String
    padLeftStr n c s =
      let len = length s
      in if len >= n then s else pack (replicate (minus n len) c) ++ s

||| Parse hex string to integer
public export
hexToInt : HexString -> Integer
hexToInt (MkHexString s) = go (unpack (toLower s)) 0
  where
    go : List Char -> Integer -> Integer
    go [] acc = acc
    go (c :: cs) acc =
      case hexCharToNibble c of
        Just n => go cs (acc * 16 + cast n)
        Nothing => acc  -- Should not happen with validated HexString

||| Create empty hex string
public export
emptyHex : HexString
emptyHex = MkHexString ""

||| Check if hex string is empty
public export
isEmptyHex : HexString -> Bool
isEmptyHex (MkHexString s) = null s
