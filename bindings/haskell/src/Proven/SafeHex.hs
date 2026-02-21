{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe hexadecimal encoding and decoding utilities.
--
-- Provides:
--
-- * Type-safe hex string validation
-- * Hex encoding and decoding
-- * Constant-time comparison for security-sensitive contexts
-- * Various hex formatting options
module Proven.SafeHex
  ( -- * Types
    HexString(..)
  , HexError(..)
    -- * Validation
  , validateHex
  , validateHexBytes
  , isValidHex
  , isValidHexBytes
  , isHexChar
    -- * Encoding
  , encodeHex
  , encodeHexUpper
  , encodeByte
  , encodeByteUpper
    -- * Decoding
  , decodeHex
  , decodeHex'
  , parseHex
  , parseHex'
  , decodeHexByte
    -- * HexString Operations
  , hexToString
  , hexLength
  , hexByteCount
  , hexToLower
  , hexToUpper
  , hexConcat
  , hexTake
  , hexDrop
    -- * Formatting
  , formatHexSpaced
  , formatHexColons
  , formatHex0x
  , formatHexCArray
    -- * Comparison
  , constantTimeEqual
    -- * Conversion
  , intToHex
  , hexToInt
  , emptyHex
  , isEmptyHex
  ) where

import Data.Bits (xor, (.|.))
import Data.Char (chr, ord, toLower, toUpper)
import Data.List (intercalate)
import Data.Word (Word8)

-- | A validated hexadecimal string (guaranteed to contain only valid hex chars).
newtype HexString = MkHexString { hexValue :: String }
  deriving (Ord)

instance Show HexString where
  show (MkHexString s) = s

instance Eq HexString where
  (MkHexString a) == (MkHexString b) = map toLower a == map toLower b

-- | Hex encoding errors.
data HexError
  = InvalidCharacter Char Int   -- ^ Invalid char at position
  | OddLength Int               -- ^ Hex string has odd length
  | EmptyInput
  | DecodingFailed String
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Hex Character Operations
--------------------------------------------------------------------------------

-- | Check if character is valid hexadecimal (0-9, a-f, A-F).
isHexChar :: Char -> Bool
isHexChar c = (c >= '0' && c <= '9') ||
              (c >= 'a' && c <= 'f') ||
              (c >= 'A' && c <= 'F')

-- | Convert hex character to numeric value (0-15).
hexCharToNibble :: Char -> Maybe Int
hexCharToNibble c
  | c >= '0' && c <= '9' = Just (ord c - ord '0')
  | c >= 'a' && c <= 'f' = Just (ord c - ord 'a' + 10)
  | c >= 'A' && c <= 'F' = Just (ord c - ord 'A' + 10)
  | otherwise = Nothing

-- | Convert nibble (0-15) to lowercase hex character.
nibbleToHexChar :: Int -> Char
nibbleToHexChar n
  | n < 10    = chr (ord '0' + n)
  | otherwise = chr (ord 'a' + n - 10)

-- | Convert nibble (0-15) to uppercase hex character.
nibbleToHexCharUpper :: Int -> Char
nibbleToHexCharUpper n
  | n < 10    = chr (ord '0' + n)
  | otherwise = chr (ord 'A' + n - 10)

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Find first invalid hex character with its position.
findInvalidChar :: String -> Int -> Maybe (Char, Int)
findInvalidChar [] _ = Nothing
findInvalidChar (c:cs) pos
  | isHexChar c = findInvalidChar cs (pos + 1)
  | otherwise   = Just (c, pos)

-- | Validate a string as hexadecimal.
validateHex :: String -> Either HexError HexString
validateHex s
  | null s    = Left EmptyInput
  | otherwise = case findInvalidChar s 0 of
      Just (c, pos) -> Left (InvalidCharacter c pos)
      Nothing       -> Right (MkHexString s)

-- | Validate hex string and check for even length (required for byte decoding).
validateHexBytes :: String -> Either HexError HexString
validateHexBytes s
  | length s `mod` 2 /= 0 = Left (OddLength (length s))
  | otherwise = validateHex s

-- | Check if string is valid hex.
isValidHex :: String -> Bool
isValidHex s = either (const False) (const True) (validateHex s)

-- | Check if string is valid hex with even length.
isValidHexBytes :: String -> Bool
isValidHexBytes s = either (const False) (const True) (validateHexBytes s)

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

-- | Encode a single byte to two hex characters (lowercase).
encodeByte :: Word8 -> String
encodeByte b =
  let n = fromIntegral b
      high = n `div` 16
      low = n `mod` 16
  in [nibbleToHexChar high, nibbleToHexChar low]

-- | Encode a single byte to two hex characters (uppercase).
encodeByteUpper :: Word8 -> String
encodeByteUpper b =
  let n = fromIntegral b
      high = n `div` 16
      low = n `mod` 16
  in [nibbleToHexCharUpper high, nibbleToHexCharUpper low]

-- | Encode bytes to hex string (lowercase).
encodeHex :: [Word8] -> HexString
encodeHex bytes = MkHexString (concatMap encodeByte bytes)

-- | Encode bytes to hex string (uppercase).
encodeHexUpper :: [Word8] -> HexString
encodeHexUpper bytes = MkHexString (concatMap encodeByteUpper bytes)

--------------------------------------------------------------------------------
-- Decoding
--------------------------------------------------------------------------------

-- | Decode two hex characters to a byte.
decodeHexByte :: Char -> Char -> Maybe Word8
decodeHexByte h1 h2 = do
  n1 <- hexCharToNibble h1
  n2 <- hexCharToNibble h2
  return (fromIntegral (n1 * 16 + n2))

-- | Decode hex string to bytes.
decodeHex :: HexString -> Either HexError [Word8]
decodeHex (MkHexString s)
  | length s `mod` 2 /= 0 = Left (OddLength (length s))
  | otherwise = decodeChars s
  where
    decodeChars :: String -> Either HexError [Word8]
    decodeChars [] = Right []
    decodeChars [_] = Left (OddLength 1)
    decodeChars (h1:h2:rest) = case decodeHexByte h1 h2 of
      Nothing -> Left (DecodingFailed "invalid hex characters")
      Just b  -> case decodeChars rest of
        Left e   -> Left e
        Right bs -> Right (b : bs)

-- | Decode hex string to bytes, returning Maybe.
decodeHex' :: HexString -> Maybe [Word8]
decodeHex' hs = either (const Nothing) Just (decodeHex hs)

-- | Parse and decode hex string in one step.
parseHex :: String -> Either HexError [Word8]
parseHex s = do
  validated <- validateHexBytes s
  decodeHex validated

-- | Parse and decode hex string, returning Maybe.
parseHex' :: String -> Maybe [Word8]
parseHex' s = either (const Nothing) Just (parseHex s)

--------------------------------------------------------------------------------
-- HexString Operations
--------------------------------------------------------------------------------

-- | Get the raw string from HexString.
hexToString :: HexString -> String
hexToString (MkHexString s) = s

-- | Get length of hex string in characters.
hexLength :: HexString -> Int
hexLength (MkHexString s) = length s

-- | Get number of bytes represented by hex string.
hexByteCount :: HexString -> Int
hexByteCount (MkHexString s) = length s `div` 2

-- | Convert hex string to lowercase.
hexToLower :: HexString -> HexString
hexToLower (MkHexString s) = MkHexString (map toLower s)

-- | Convert hex string to uppercase.
hexToUpper :: HexString -> HexString
hexToUpper (MkHexString s) = MkHexString (map toUpper s)

-- | Concatenate two hex strings.
hexConcat :: HexString -> HexString -> HexString
hexConcat (MkHexString a) (MkHexString b) = MkHexString (a ++ b)

-- | Take first n bytes from hex string (2n characters).
hexTake :: Int -> HexString -> HexString
hexTake n (MkHexString s) = MkHexString (take (n * 2) s)

-- | Drop first n bytes from hex string (2n characters).
hexDrop :: Int -> HexString -> HexString
hexDrop n (MkHexString s) = MkHexString (drop (n * 2) s)

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

-- | Format pairs of characters with a separator.
formatPairs :: Char -> String -> String
formatPairs _ [] = ""
formatPairs _ [c] = [c]
formatPairs _ [c1, c2] = [c1, c2]
formatPairs sep (c1:c2:rest) = c1 : c2 : sep : formatPairs sep rest

-- | Format hex string with spaces between bytes (e.g., "01 02 03").
formatHexSpaced :: HexString -> String
formatHexSpaced (MkHexString s) = formatPairs ' ' s

-- | Format hex string with colons between bytes (e.g., "01:02:03").
formatHexColons :: HexString -> String
formatHexColons (MkHexString s) = formatPairs ':' s

-- | Format hex string with prefix (e.g., "0x0102").
formatHex0x :: HexString -> String
formatHex0x (MkHexString s) = "0x" ++ s

-- | Format hex string as C array literal.
formatHexCArray :: HexString -> String
formatHexCArray hs = case decodeHex' hs of
  Nothing    -> "{ /* invalid */ }"
  Just bytes ->
    let formatted = map (\b -> "0x" ++ encodeByteUpper b) bytes
    in "{ " ++ intercalate ", " formatted ++ " }"

--------------------------------------------------------------------------------
-- Constant-Time Comparison
--------------------------------------------------------------------------------

-- | Constant-time comparison of hex strings (timing-attack resistant).
--
-- This function compares two hex strings in constant time to prevent
-- timing side-channel attacks. It uses bitwise XOR and OR operations
-- to avoid early exit on mismatches.
constantTimeEqual :: HexString -> HexString -> Bool
constantTimeEqual (MkHexString a) (MkHexString b) =
  let la = map toLower a
      lb = map toLower b
  in if length la /= length lb
       then False
       else go la lb 0
  where
    go :: String -> String -> Int -> Bool
    go [] [] acc = acc == 0
    go (x:xs) (y:ys) acc = go xs ys (acc .|. (ord x `xor` ord y))
    go _ _ _ = False

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Pad string with character on the left.
padLeftStr :: Int -> Char -> String -> String
padLeftStr n c s
  | length s >= n = s
  | otherwise = replicate (n - length s) c ++ s

-- | Convert integer to hex string with minimum width.
intToHex :: Integer -> Int -> HexString
intToHex n minWidth =
  let absN = abs n
      hex = intToHexRaw absN
      padded = padLeftStr minWidth '0' hex
  in MkHexString padded
  where
    intToHexRaw :: Integer -> String
    intToHexRaw 0 = "0"
    intToHexRaw i = go i ""
      where
        go 0 acc = acc
        go x acc =
          let digit = nibbleToHexChar (fromIntegral (x `mod` 16))
          in go (x `div` 16) (digit : acc)

-- | Parse hex string to integer.
hexToInt :: HexString -> Integer
hexToInt (MkHexString s) = go (map toLower s) 0
  where
    go :: String -> Integer -> Integer
    go [] acc = acc
    go (c:cs) acc = case hexCharToNibble c of
      Just n  -> go cs (acc * 16 + fromIntegral n)
      Nothing -> acc  -- Should not happen with validated HexString

-- | Create empty hex string.
emptyHex :: HexString
emptyHex = MkHexString ""

-- | Check if hex string is empty.
isEmptyHex :: HexString -> Bool
isEmptyHex (MkHexString s) = null s
