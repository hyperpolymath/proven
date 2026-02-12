{- SPDX-License-Identifier: Apache-2.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe UUID parsing, validation, and formatting.
--
-- Provides type-safe UUID operations following RFC 4122:
--
-- * UUID newtype with version and variant detection
-- * Parsing from canonical 8-4-4-4-12 format
-- * Multiple output formats (canonical, URN, compact)
-- * Nil UUID and namespace constants
module Proven.SafeUUID
  ( -- * Types
    UUID(..)
  , UuidVersion(..)
  , UuidVariant(..)
  , UuidError(..)
    -- * Constants
  , nilUUID
  , namespaceDNS
  , namespaceURL
  , namespaceOID
  , namespaceX500
    -- * Parsing
  , parseUUID
  , parseUUID'
  , isValidUUID
  , looksLikeUUID
    -- * Properties
  , uuidVersion
  , uuidVariant
  , isNilUUID
    -- * Formatting
  , formatUUID
  , formatURN
  , formatCompact
  , formatUppercase
    -- * Construction
  , uuidFromBytes
  , uuidV4FromBytes
  ) where

import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.Char (chr, ord, toUpper, isHexDigit)
import Data.Word (Word8)

-- | UUID version enumeration per RFC 4122.
data UuidVersion
  = V1    -- ^ Time-based
  | V2    -- ^ DCE Security
  | V3    -- ^ Name-based (MD5)
  | V4    -- ^ Random
  | V5    -- ^ Name-based (SHA-1)
  | VNil  -- ^ Nil UUID (all zeros)
  deriving (Show, Eq, Ord)

-- | UUID variant enumeration per RFC 4122.
data UuidVariant
  = NCS        -- ^ Reserved for NCS backward compatibility
  | RFC4122    -- ^ Standard RFC 4122 variant
  | Microsoft  -- ^ Reserved for Microsoft backward compatibility
  | Future     -- ^ Reserved for future definition
  deriving (Show, Eq, Ord)

-- | UUID parsing errors.
data UuidError
  = InvalidLength Int
  | InvalidCharacter Char Int
  | InvalidFormat String
  | InvalidVersion Int
  | InvalidVariantError
  deriving (Show, Eq)

-- | A validated UUID stored as 16 bytes.
newtype UUID = MkUUID { uuidBytes :: [Word8] }
  deriving (Eq, Ord)

instance Show UUID where
  show = formatUUID

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | The nil UUID (all zeros): 00000000-0000-0000-0000-000000000000
nilUUID :: UUID
nilUUID = MkUUID (replicate 16 0)

-- | DNS namespace UUID for v3/v5: 6ba7b810-9dad-11d1-80b4-00c04fd430c8
namespaceDNS :: UUID
namespaceDNS = MkUUID [0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
                       0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]

-- | URL namespace UUID for v3/v5: 6ba7b811-9dad-11d1-80b4-00c04fd430c8
namespaceURL :: UUID
namespaceURL = MkUUID [0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
                       0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]

-- | OID namespace UUID for v3/v5: 6ba7b812-9dad-11d1-80b4-00c04fd430c8
namespaceOID :: UUID
namespaceOID = MkUUID [0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
                       0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]

-- | X.500 namespace UUID for v3/v5: 6ba7b814-9dad-11d1-80b4-00c04fd430c8
namespaceX500 :: UUID
namespaceX500 = MkUUID [0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
                        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- | Extract the version from a UUID.
uuidVersion :: UUID -> UuidVersion
uuidVersion (MkUUID bytes)
  | length bytes < 7 = VNil
  | otherwise =
      let versionByte = bytes !! 6
          version = (fromIntegral versionByte `shiftR` 4) .&. 0x0F :: Int
      in case version of
           1 -> V1
           2 -> V2
           3 -> V3
           4 -> V4
           5 -> V5
           _ -> VNil

-- | Extract the variant from a UUID.
uuidVariant :: UUID -> UuidVariant
uuidVariant (MkUUID bytes)
  | length bytes < 9 = NCS
  | otherwise =
      let variantByte = fromIntegral (bytes !! 8) :: Int
      in if (variantByte `shiftR` 7) == 0 then NCS
         else if (variantByte `shiftR` 6) == 2 then RFC4122
         else if (variantByte `shiftR` 5) == 6 then Microsoft
         else Future

-- | Check if UUID is the nil UUID.
isNilUUID :: UUID -> Bool
isNilUUID (MkUUID bytes) = all (== 0) bytes

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse hex character to value (0-15).
hexValue :: Char -> Maybe Int
hexValue c
  | c >= '0' && c <= '9' = Just (ord c - ord '0')
  | c >= 'a' && c <= 'f' = Just (ord c - ord 'a' + 10)
  | c >= 'A' && c <= 'F' = Just (ord c - ord 'A' + 10)
  | otherwise = Nothing

-- | Parse two hex characters to a byte.
parseHexByte :: Char -> Char -> Maybe Word8
parseHexByte h1 h2 = do
  v1 <- hexValue h1
  v2 <- hexValue h2
  return (fromIntegral (v1 * 16 + v2))

-- | Parse UUID from canonical format (8-4-4-4-12).
-- Example: "550e8400-e29b-41d4-a716-446655440000"
parseUUID :: String -> Either UuidError UUID
parseUUID s
  | len /= 36 = Left (InvalidLength len)
  | otherwise = parseCanonical s
  where
    len = length s

    parseCanonical :: String -> Either UuidError UUID
    parseCanonical str
      | str !! 8 /= '-' || str !! 13 /= '-' || str !! 18 /= '-' || str !! 23 /= '-' =
          Left (InvalidFormat "missing or misplaced hyphens")
      | otherwise =
          let hexChars = filter (/= '-') str
          in if all isHexDigit hexChars
               then parseHexChars hexChars
               else Left (InvalidFormat "contains non-hex characters")

    parseHexChars :: String -> Either UuidError UUID
    parseHexChars cs = case pairUp cs of
      Nothing -> Left (InvalidFormat "odd character count")
      Just pairs -> case traverse (uncurry parseHexByte) pairs of
        Nothing -> Left (InvalidFormat "hex parse failed")
        Just bytes
          | length bytes == 16 -> Right (MkUUID bytes)
          | otherwise -> Left (InvalidFormat "wrong byte count")

    pairUp :: [a] -> Maybe [(a, a)]
    pairUp [] = Just []
    pairUp [_] = Nothing
    pairUp (x:y:rest) = do
      rest' <- pairUp rest
      return ((x, y) : rest')

-- | Parse UUID from string, returning Maybe.
parseUUID' :: String -> Maybe UUID
parseUUID' s = either (const Nothing) Just (parseUUID s)

-- | Validate UUID string format.
isValidUUID :: String -> Bool
isValidUUID s = either (const False) (const True) (parseUUID s)

-- | Quick check if string looks like a UUID (format only, not content).
looksLikeUUID :: String -> Bool
looksLikeUUID s =
  length s == 36 &&
  s !! 8 == '-' &&
  s !! 13 == '-' &&
  s !! 18 == '-' &&
  s !! 23 == '-'

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

-- | Hex digit from nibble (0-15).
hexDigit :: Int -> Char
hexDigit n
  | n < 10    = chr (ord '0' + n)
  | otherwise = chr (ord 'a' + n - 10)

-- | Format byte as two hex characters.
formatByte :: Word8 -> String
formatByte b =
  let n = fromIntegral b
  in [hexDigit (n `div` 16), hexDigit (n `mod` 16)]

-- | Format UUID in canonical form (8-4-4-4-12 with lowercase).
formatUUID :: UUID -> String
formatUUID (MkUUID bytes)
  | length bytes /= 16 = ""
  | otherwise =
      let hexBytes = map formatByte bytes
      in case hexBytes of
           [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15] ->
             b0 ++ b1 ++ b2 ++ b3 ++ "-" ++
             b4 ++ b5 ++ "-" ++
             b6 ++ b7 ++ "-" ++
             b8 ++ b9 ++ "-" ++
             b10 ++ b11 ++ b12 ++ b13 ++ b14 ++ b15
           _ -> ""  -- Should not happen due to length check

-- | Format UUID as URN (urn:uuid:...).
formatURN :: UUID -> String
formatURN uuid = "urn:uuid:" ++ formatUUID uuid

-- | Format UUID without hyphens.
formatCompact :: UUID -> String
formatCompact (MkUUID bytes) = concatMap formatByte bytes

-- | Format UUID with uppercase.
formatUppercase :: UUID -> String
formatUppercase uuid = map toUpper (formatUUID uuid)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Create UUID from exactly 16 bytes.
uuidFromBytes :: [Word8] -> Maybe UUID
uuidFromBytes bytes
  | length bytes == 16 = Just (MkUUID bytes)
  | otherwise = Nothing

-- | Set version bits in UUID bytes.
setVersion :: [Word8] -> UuidVersion -> [Word8]
setVersion bytes ver
  | length bytes < 7 = bytes
  | otherwise =
      let versionNibble :: Word8
          versionNibble = case ver of
            V1   -> 1
            V2   -> 2
            V3   -> 3
            V4   -> 4
            V5   -> 5
            VNil -> 0
          byte6 = bytes !! 6
          newByte6 = (byte6 .&. 0x0F) .|. (versionNibble `shiftL` 4)
      in take 6 bytes ++ [newByte6] ++ drop 7 bytes

-- | Set variant bits in UUID bytes (RFC 4122).
setVariantRFC4122 :: [Word8] -> [Word8]
setVariantRFC4122 bytes
  | length bytes < 9 = bytes
  | otherwise =
      let byte8 = bytes !! 8
          newByte8 = (byte8 .&. 0x3F) .|. 0x80
      in take 8 bytes ++ [newByte8] ++ drop 9 bytes

-- | Create UUID v4 from 16 random bytes.
-- Sets the version and variant bits appropriately.
uuidV4FromBytes :: [Word8] -> Maybe UUID
uuidV4FromBytes randomBytes
  | length randomBytes /= 16 = Nothing
  | otherwise =
      let withVersion = setVersion randomBytes V4
          withVariant = setVariantRFC4122 withVersion
      in Just (MkUUID withVariant)
