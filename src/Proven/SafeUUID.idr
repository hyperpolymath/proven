-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeUUID - UUID generation and validation with type safety
|||
||| This module provides:
||| - Type-safe UUID representation
||| - UUID v4 (random) generation interface
||| - UUID parsing and validation
||| - UUID formatting (canonical, URN, etc.)
||| - Nil UUID constant
|||
||| UUIDs follow RFC 4122 specification
module Proven.SafeUUID

import public Proven.Core
import Data.List
import Data.String
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- UUID Types
--------------------------------------------------------------------------------

||| UUID version enumeration
public export
data UUIDVersion
  = V1    -- Time-based
  | V2    -- DCE Security
  | V3    -- Name-based (MD5)
  | V4    -- Random
  | V5    -- Name-based (SHA-1)
  | VNil  -- Nil UUID (all zeros)

public export
Show UUIDVersion where
  show V1 = "v1"
  show V2 = "v2"
  show V3 = "v3"
  show V4 = "v4"
  show V5 = "v5"
  show VNil = "nil"

||| UUID variant enumeration
public export
data UUIDVariant
  = NCS         -- Reserved for NCS backward compatibility
  | RFC4122     -- Standard RFC 4122 variant
  | Microsoft   -- Reserved for Microsoft backward compatibility
  | Future      -- Reserved for future definition

public export
Show UUIDVariant where
  show NCS = "NCS"
  show RFC4122 = "RFC4122"
  show Microsoft = "Microsoft"
  show Future = "Future"

||| A validated UUID
||| Stores the 128-bit value as 16 bytes
public export
data UUID : Type where
  MkUUID : (bytes : Vect 16 Bits8) -> UUID

||| UUID parsing errors
public export
data UUIDError
  = InvalidLength Nat
  | InvalidCharacter Char Nat
  | InvalidFormat String
  | InvalidVersion Nat
  | InvalidVariant

public export
Show UUIDError where
  show (InvalidLength n) = "Invalid UUID length: " ++ show n ++ " (expected 36)"
  show (InvalidCharacter c pos) = "Invalid character '" ++ singleton c ++ "' at position " ++ show pos
  show (InvalidFormat msg) = "Invalid UUID format: " ++ msg
  show (InvalidVersion v) = "Invalid UUID version: " ++ show v
  show InvalidVariant = "Invalid UUID variant"

--------------------------------------------------------------------------------
-- UUID Constants
--------------------------------------------------------------------------------

||| The nil UUID (all zeros)
public export
nilUUID : UUID
nilUUID = MkUUID (replicate 16 0)

||| DNS namespace UUID for v3/v5
public export
namespaceDNS : UUID
namespaceDNS = MkUUID [0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
                       0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]

||| URL namespace UUID for v3/v5
public export
namespaceURL : UUID
namespaceURL = MkUUID [0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
                       0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]

||| OID namespace UUID for v3/v5
public export
namespaceOID : UUID
namespaceOID = MkUUID [0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
                       0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]

||| X.500 namespace UUID for v3/v5
public export
namespaceX500 : UUID
namespaceX500 = MkUUID [0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
                        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]

--------------------------------------------------------------------------------
-- UUID Properties
--------------------------------------------------------------------------------

||| Extract the version from a UUID
public export
uuidVersion : UUID -> UUIDVersion
uuidVersion (MkUUID bytes) =
  let versionByte = index 6 bytes
      version = (cast versionByte `shiftR` 4) `mod` 16
  in case version of
       1 => V1
       2 => V2
       3 => V3
       4 => V4
       5 => V5
       _ => VNil  -- Treat unknown as Nil

||| Extract the variant from a UUID
public export
uuidVariant : UUID -> UUIDVariant
uuidVariant (MkUUID bytes) =
  let variantByte = cast {to=Nat} (index 8 bytes)
  in if (variantByte `shiftR` 7) == 0 then NCS
     else if (variantByte `shiftR` 6) == 2 then RFC4122
     else if (variantByte `shiftR` 5) == 6 then Microsoft
     else Future

||| Check if UUID is the nil UUID
public export
isNilUUID : UUID -> Bool
isNilUUID (MkUUID bytes) = all (== 0) bytes

--------------------------------------------------------------------------------
-- UUID Parsing
--------------------------------------------------------------------------------

||| Check if character is valid hexadecimal
hexChar : Char -> Bool
hexChar c = (c >= '0' && c <= '9') ||
            (c >= 'a' && c <= 'f') ||
            (c >= 'A' && c <= 'F')

||| Parse hex character to value
hexValue : Char -> Maybe Nat
hexValue c =
  if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
  else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
  else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
  else Nothing

||| Parse two hex characters to a byte
parseHexByte : Char -> Char -> Maybe Bits8
parseHexByte h1 h2 = do
  v1 <- hexValue h1
  v2 <- hexValue h2
  pure (cast (v1 * 16 + v2))

||| Parse UUID from canonical format (8-4-4-4-12)
||| Example: "550e8400-e29b-41d4-a716-446655440000"
public export
parseUUID : String -> Either UUIDError UUID
parseUUID s =
  let chars = unpack s
      len = length chars
  in if len /= 36
       then Left (InvalidLength len)
       else parseCanonical chars
  where
    parseCanonical : List Char -> Either UUIDError UUID
    parseCanonical cs =
      case cs of
        -- 8-4-4-4-12 format with hyphens at positions 8, 13, 18, 23
        [c0,c1,c2,c3,c4,c5,c6,c7,'-',
         c8,c9,c10,c11,'-',
         c12,c13,c14,c15,'-',
         c16,c17,c18,c19,'-',
         c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31] =>
          let hexChars = [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,
                         c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31]
          in if all hexChar hexChars
               then parseHexChars hexChars
               else Left (InvalidFormat "contains non-hex characters")
        _ => Left (InvalidFormat "missing or misplaced hyphens")

    parseHexChars : List Char -> Either UUIDError UUID
    parseHexChars cs =
      case pairUp cs of
        Just pairs =>
          case traverse (\(h1, h2) => parseHexByte h1 h2) pairs of
            Just bytes => case toVect 16 bytes of
              Just v => Right (MkUUID v)
              Nothing => Left (InvalidFormat "wrong byte count")
            Nothing => Left (InvalidFormat "hex parse failed")
        Nothing => Left (InvalidFormat "odd character count")

    pairUp : List a -> Maybe (List (a, a))
    pairUp [] = Just []
    pairUp [_] = Nothing
    pairUp (x :: y :: rest) = do
      rest' <- pairUp rest
      pure ((x, y) :: rest')

||| Parse UUID from string, returning Maybe
public export
parseUUID' : String -> Maybe UUID
parseUUID' s = either (const Nothing) Just (parseUUID s)

--------------------------------------------------------------------------------
-- UUID Formatting
--------------------------------------------------------------------------------

||| Hex digit from nibble
hexDigit : Nat -> Char
hexDigit n = if n < 10
             then chr (ord '0' + cast n)
             else chr (ord 'a' + cast n - 10)

||| Format byte as two hex characters
formatByte : Bits8 -> String
formatByte b =
  let n = cast {to=Nat} b
  in pack [hexDigit (n `div` 16), hexDigit (n `mod` 16)]

||| Format UUID in canonical form (8-4-4-4-12 with lowercase)
public export
formatUUID : UUID -> String
formatUUID (MkUUID bytes) =
  let hexBytes = map formatByte (toList bytes)
  in case hexBytes of
       [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15] =>
         b0 ++ b1 ++ b2 ++ b3 ++ "-" ++
         b4 ++ b5 ++ "-" ++
         b6 ++ b7 ++ "-" ++
         b8 ++ b9 ++ "-" ++
         b10 ++ b11 ++ b12 ++ b13 ++ b14 ++ b15
       _ => ""  -- Should never happen

||| Format UUID as URN (urn:uuid:...)
public export
formatURN : UUID -> String
formatURN uuid = "urn:uuid:" ++ formatUUID uuid

||| Format UUID without hyphens
public export
formatCompact : UUID -> String
formatCompact (MkUUID bytes) = concat (map formatByte (toList bytes))

||| Format UUID with uppercase
public export
formatUppercase : UUID -> String
formatUppercase uuid = toUpper (formatUUID uuid)

--------------------------------------------------------------------------------
-- UUID Validation
--------------------------------------------------------------------------------

||| Validate UUID string format
public export
isValidUUID : String -> Bool
isValidUUID s = isRight (parseUUID s)

||| Check if string looks like a UUID (quick check)
public export
looksLikeUUID : String -> Bool
looksLikeUUID s =
  let len = length s
  in len == 36 &&
     strIndex s 8 == Just '-' &&
     strIndex s 13 == Just '-' &&
     strIndex s 18 == Just '-' &&
     strIndex s 23 == Just '-'
  where
    strIndex : String -> Nat -> Maybe Char
    strIndex str idx =
      let chars = unpack str
      in if idx < length chars
           then Just (assert_total $ index' (cast idx) chars)
           else Nothing

    index' : Int -> List a -> a
    index' 0 (x :: _) = x
    index' n (_ :: xs) = index' (n - 1) xs
    index' _ [] = believe_me ()  -- Should never happen due to bounds check

--------------------------------------------------------------------------------
-- UUID Comparison
--------------------------------------------------------------------------------

public export
Eq UUID where
  (MkUUID a) == (MkUUID b) = a == b

public export
Ord UUID where
  compare (MkUUID a) (MkUUID b) = compare (toList a) (toList b)

public export
Show UUID where
  show = formatUUID

--------------------------------------------------------------------------------
-- UUID Generation Interface
--------------------------------------------------------------------------------

||| Interface for UUID generation (requires FFI for actual random)
public export
interface UUIDGenerator m where
  ||| Generate a random v4 UUID
  generateV4 : m UUID

  ||| Generate a v5 UUID from namespace and name
  generateV5 : UUID -> String -> m UUID

||| Set version bits in UUID bytes
setVersion : Vect 16 Bits8 -> UUIDVersion -> Vect 16 Bits8
setVersion bytes ver =
  let versionNibble = case ver of
        V1 => 1
        V2 => 2
        V3 => 3
        V4 => 4
        V5 => 5
        VNil => 0
      byte6 = index 6 bytes
      newByte6 = (byte6 `and` 0x0F) `or` (cast versionNibble `shiftL` 4)
  in replaceAt 6 newByte6 bytes

||| Set variant bits in UUID bytes (RFC 4122)
setVariantRFC4122 : Vect 16 Bits8 -> Vect 16 Bits8
setVariantRFC4122 bytes =
  let byte8 = index 8 bytes
      newByte8 = (byte8 `and` 0x3F) `or` 0x80
  in replaceAt 8 newByte8 bytes

||| Create UUID v4 from 16 random bytes
||| Sets the version and variant bits appropriately
public export
uuidV4FromBytes : Vect 16 Bits8 -> UUID
uuidV4FromBytes randomBytes =
  let withVersion = setVersion randomBytes V4
      withVariant = setVariantRFC4122 withVersion
  in MkUUID withVariant
