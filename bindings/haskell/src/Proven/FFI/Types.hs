{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI result types matching the C ABI exported by libproven.
--
-- These Storable instances mirror the extern struct definitions in
-- @ffi\/zig\/src\/main.zig@. All sizes and alignments match the
-- C ABI layout used by the Zig-compiled shared library.
--
-- __No computation happens here__: these are pure data marshaling types.
module Proven.FFI.Types
  ( -- * Status Codes
    ProvenStatus(..)
  , statusFromCInt
  , statusIsOk
    -- * Result Types
  , IntResult(..)
  , BoolResult(..)
  , StringResult(..)
  , FloatResult(..)
    -- * IPv4
  , IPv4Address(..)
  , IPv4Result(..)
    -- * UUID
  , FFI_UUID(..)
  , UUIDResult(..)
    -- * Currency
  , CurrencyResult(..)
    -- * Phone
  , PhoneResult(..)
    -- * DateTime
  , FFI_DateTime(..)
  , DateTimeResult(..)
    -- * SemanticVersion
  , FFI_SemanticVersion(..)
  , VersionResult(..)
    -- * Color
  , RGBColor(..)
  , HSLColor(..)
  , ColorParseResult(..)
    -- * Geo
  , GeoCoordinate(..)
  , GeoResult(..)
    -- * Password
  , PasswordResult(..)
    -- * Retry
  , RetryConfig(..)
    -- * Enums
  , JsonType(..)
  , LengthUnitC(..)
  , TempUnitC(..)
  , CircuitStateC(..)
  , PasswordStrengthC(..)
  , SameSiteC(..)
  , CharsetC(..)
  , MediaCategoryC(..)
    -- * Cookie
  , CookieAttributes(..)
    -- * ContentType
  , ContentTypeResult(..)
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- | Status codes returned by libproven functions.
-- Maps to @ProvenStatus@ enum in the Zig FFI layer.
data ProvenStatus
  = StatusOk                 -- ^  0: Success
  | StatusErrNullPointer     -- ^ -1: Null pointer argument
  | StatusErrInvalidArgument -- ^ -2: Invalid argument
  | StatusErrOverflow        -- ^ -3: Arithmetic overflow
  | StatusErrUnderflow       -- ^ -4: Arithmetic underflow
  | StatusErrDivisionByZero  -- ^ -5: Division by zero
  | StatusErrParseFailure    -- ^ -6: Parse failure
  | StatusErrValidationFailed-- ^ -7: Validation failed
  | StatusErrOutOfBounds     -- ^ -8: Out of bounds
  | StatusErrEncodingError   -- ^ -9: Encoding error
  | StatusErrAllocationFailed-- ^ -10: Memory allocation failed
  | StatusErrNotImplemented  -- ^ -99: Not implemented
  | StatusUnknown !CInt      -- ^ Unknown status code
  deriving (Eq, Show)

-- | Convert a raw CInt status code to a ProvenStatus.
statusFromCInt :: CInt -> ProvenStatus
statusFromCInt 0    = StatusOk
statusFromCInt (-1) = StatusErrNullPointer
statusFromCInt (-2) = StatusErrInvalidArgument
statusFromCInt (-3) = StatusErrOverflow
statusFromCInt (-4) = StatusErrUnderflow
statusFromCInt (-5) = StatusErrDivisionByZero
statusFromCInt (-6) = StatusErrParseFailure
statusFromCInt (-7) = StatusErrValidationFailed
statusFromCInt (-8) = StatusErrOutOfBounds
statusFromCInt (-9) = StatusErrEncodingError
statusFromCInt (-10) = StatusErrAllocationFailed
statusFromCInt (-99) = StatusErrNotImplemented
statusFromCInt n    = StatusUnknown n

-- | Check if a status indicates success.
statusIsOk :: ProvenStatus -> Bool
statusIsOk StatusOk = True
statusIsOk _        = False

--------------------------------------------------------------------------------
-- IntResult: { status: i32, value: i64 }
-- Layout: 4 bytes status + 4 padding + 8 bytes value = 16 bytes
--------------------------------------------------------------------------------

-- | Result for integer operations.
-- C layout: @extern struct { status: ProvenStatus(i32), value: i64 }@
data IntResult = IntResult
  { irStatusRaw :: !CInt
  , irValue     :: !CLLong
  } deriving (Eq, Show)

instance Storable IntResult where
  sizeOf    _ = 16
  alignment _ = 8
  peek ptr = IntResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
  poke ptr (IntResult s v) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 v

--------------------------------------------------------------------------------
-- BoolResult: { status: i32, value: bool(i8) }
-- Layout: 4 bytes status + 1 byte bool + 3 padding = 8 bytes
--------------------------------------------------------------------------------

-- | Result for boolean operations.
-- C layout: @extern struct { status: ProvenStatus(i32), value: bool }@
data BoolResult = BoolResult
  { brStatusRaw :: !CInt
  , brValue     :: !CChar
  } deriving (Eq, Show)

instance Storable BoolResult where
  sizeOf    _ = 8
  alignment _ = 4
  peek ptr = BoolResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
  poke ptr (BoolResult s v) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 v

--------------------------------------------------------------------------------
-- StringResult: { status: i32, value: *u8, length: usize }
-- Layout: 4 bytes status + 4 padding + 8 bytes ptr + 8 bytes length = 24 bytes
--------------------------------------------------------------------------------

-- | Result for string operations. Caller must free the string via
-- @proven_free_string@.
data StringResult = StringResult
  { srStatusRaw :: !CInt
  , srValue     :: !(Ptr CChar)
  , srLength    :: !CSize
  } deriving (Eq, Show)

instance Storable StringResult where
  sizeOf    _ = 24
  alignment _ = 8
  peek ptr = StringResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 16
  poke ptr (StringResult s v l) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 v
    pokeByteOff ptr 16 l

--------------------------------------------------------------------------------
-- FloatResult: { status: i32, value: f64 }
-- Layout: 4 bytes status + 4 padding + 8 bytes value = 16 bytes
--------------------------------------------------------------------------------

-- | Result for floating-point operations.
data FloatResult = FloatResult
  { frStatusRaw :: !CInt
  , frValue     :: !CDouble
  } deriving (Eq, Show)

instance Storable FloatResult where
  sizeOf    _ = 16
  alignment _ = 8
  peek ptr = FloatResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
  poke ptr (FloatResult s v) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 v

--------------------------------------------------------------------------------
-- IPv4Address: { octets: [4]u8 }
--------------------------------------------------------------------------------

-- | IPv4 address as 4 octets.
data IPv4Address = IPv4Address
  { ipv4A :: !Word8
  , ipv4B :: !Word8
  , ipv4C :: !Word8
  , ipv4D :: !Word8
  } deriving (Eq, Show)

instance Storable IPv4Address where
  sizeOf    _ = 4
  alignment _ = 1
  peek ptr = IPv4Address
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 1
    <*> peekByteOff ptr 2
    <*> peekByteOff ptr 3
  poke ptr (IPv4Address a b c d) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 1 b
    pokeByteOff ptr 2 c
    pokeByteOff ptr 3 d

-- | Result for IPv4 parsing.
-- Layout: 4 bytes status + 4 bytes address = 8 bytes
data IPv4Result = IPv4Result
  { ipv4rStatusRaw :: !CInt
  , ipv4rAddress   :: !IPv4Address
  } deriving (Eq, Show)

instance Storable IPv4Result where
  sizeOf    _ = 8
  alignment _ = 4
  peek ptr = IPv4Result
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
  poke ptr (IPv4Result s a) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 a

--------------------------------------------------------------------------------
-- UUID: { bytes: [16]u8 }
--------------------------------------------------------------------------------

-- | UUID as 16 raw bytes.
newtype FFI_UUID = FFI_UUID { uuidBytes :: [Word8] }
  deriving (Eq, Show)

instance Storable FFI_UUID where
  sizeOf    _ = 16
  alignment _ = 1
  peek ptr = do
    bs <- mapM (\i -> peekByteOff ptr i) [0..15]
    return (FFI_UUID bs)
  poke ptr (FFI_UUID bs) =
    mapM_ (\(i, b) -> pokeByteOff ptr i b) (zip [0..15] (bs ++ repeat 0))

-- | Result for UUID operations.
-- Layout: 4 bytes status + 16 bytes uuid = 20 bytes, padded to alignment
data UUIDResult = UUIDResult
  { uuidrStatusRaw :: !CInt
  , uuidrUUID      :: !FFI_UUID
  } deriving (Eq, Show)

instance Storable UUIDResult where
  sizeOf    _ = 20
  alignment _ = 4
  peek ptr = UUIDResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
  poke ptr (UUIDResult s u) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 u

--------------------------------------------------------------------------------
-- CurrencyResult: { status: i32, amount_minor: i64, currency_code: [3]u8, decimal_places: u8 }
--------------------------------------------------------------------------------

-- | Result for currency parsing.
data CurrencyResult = CurrencyResult
  { currStatusRaw     :: !CInt
  , currAmountMinor   :: !CLLong
  , currCode          :: !(Word8, Word8, Word8)
  , currDecimalPlaces :: !Word8
  } deriving (Eq, Show)

instance Storable CurrencyResult where
  sizeOf    _ = 24
  alignment _ = 8
  peek ptr = do
    s <- peekByteOff ptr 0
    a <- peekByteOff ptr 8
    c0 <- peekByteOff ptr 16 :: IO Word8
    c1 <- peekByteOff ptr 17 :: IO Word8
    c2 <- peekByteOff ptr 18 :: IO Word8
    d <- peekByteOff ptr 19
    return (CurrencyResult s a (c0, c1, c2) d)
  poke ptr (CurrencyResult s a (c0, c1, c2) d) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 a
    pokeByteOff ptr 16 c0
    pokeByteOff ptr 17 c1
    pokeByteOff ptr 18 c2
    pokeByteOff ptr 19 d

--------------------------------------------------------------------------------
-- PhoneResult: { status: i32, country_code: u16, national_number: u64, is_valid: bool }
--------------------------------------------------------------------------------

-- | Result for phone number parsing.
data PhoneResult = PhoneResult
  { phoneStatusRaw       :: !CInt
  , phoneCountryCodeRaw  :: !Word16
  , phoneNationalNumber  :: !Word64
  , phoneIsValid         :: !CChar
  } deriving (Eq, Show)

instance Storable PhoneResult where
  sizeOf    _ = 24
  alignment _ = 8
  peek ptr = PhoneResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 16
  poke ptr (PhoneResult s c n v) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 c
    pokeByteOff ptr 8 n
    pokeByteOff ptr 16 v

--------------------------------------------------------------------------------
-- DateTime: { year: i32, month: u8, day: u8, hour: u8, minute: u8,
--             second: u8, nanosecond: u32, tz_offset_minutes: i16 }
--------------------------------------------------------------------------------

-- | DateTime components from libproven.
data FFI_DateTime = FFI_DateTime
  { dtYear           :: !CInt
  , dtMonth          :: !Word8
  , dtDay            :: !Word8
  , dtHour           :: !Word8
  , dtMinute         :: !Word8
  , dtSecond         :: !Word8
  , dtNanosecond     :: !Word32
  , dtTzOffsetMinutes :: !Int16
  } deriving (Eq, Show)

instance Storable FFI_DateTime where
  sizeOf    _ = 16
  alignment _ = 4
  peek ptr = FFI_DateTime
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 5
    <*> peekByteOff ptr 6
    <*> peekByteOff ptr 7
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12 -- aligned to 4 after the u8 fields + padding
    <*> peekByteOff ptr 14 -- aligned after u32 (nanosecond is 4 bytes at offset 12, next is i16)
  poke ptr (FFI_DateTime y mo d h mi s ns tz) = do
    pokeByteOff ptr 0  y
    pokeByteOff ptr 4  mo
    pokeByteOff ptr 5  d
    pokeByteOff ptr 6  h
    pokeByteOff ptr 7  mi
    pokeByteOff ptr 8  s
    pokeByteOff ptr 12 ns
    pokeByteOff ptr 14 tz

-- | Result for DateTime parsing.
data DateTimeResult = DateTimeResult
  { dtrStatusRaw :: !CInt
  , dtrDateTime  :: !FFI_DateTime
  } deriving (Eq, Show)

instance Storable DateTimeResult where
  sizeOf    _ = 20
  alignment _ = 4
  peek ptr = DateTimeResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
  poke ptr (DateTimeResult s dt) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 dt

--------------------------------------------------------------------------------
-- SemanticVersion
--------------------------------------------------------------------------------

-- | Semantic version from libproven.
data FFI_SemanticVersion = FFI_SemanticVersion
  { svMajorRaw        :: !Word32
  , svMinorRaw        :: !Word32
  , svPatchRaw        :: !Word32
  , svPrereleaseLen   :: !CSize
  , svPrereleasePtr   :: !(Ptr CChar)
  } deriving (Eq, Show)

instance Storable FFI_SemanticVersion where
  sizeOf    _ = 32
  alignment _ = 8
  peek ptr = FFI_SemanticVersion
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
  poke ptr (FFI_SemanticVersion ma mi pa pl pp) = do
    pokeByteOff ptr 0  ma
    pokeByteOff ptr 4  mi
    pokeByteOff ptr 8  pa
    pokeByteOff ptr 16 pl
    pokeByteOff ptr 24 pp

-- | Result for version parsing.
data VersionResult = VersionResult
  { vrStatusRaw :: !CInt
  , vrVersion   :: !FFI_SemanticVersion
  } deriving (Eq, Show)

instance Storable VersionResult where
  sizeOf    _ = 40
  alignment _ = 8
  peek ptr = VersionResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
  poke ptr (VersionResult s v) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 v

--------------------------------------------------------------------------------
-- Color types
--------------------------------------------------------------------------------

-- | RGB color (3 bytes).
data RGBColor = RGBColor
  { rgbR :: !Word8
  , rgbG :: !Word8
  , rgbB :: !Word8
  } deriving (Eq, Show)

instance Storable RGBColor where
  sizeOf    _ = 3
  alignment _ = 1
  peek ptr = RGBColor
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 1
    <*> peekByteOff ptr 2
  poke ptr (RGBColor r g b) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 1 g
    pokeByteOff ptr 2 b

-- | HSL color.
data HSLColor = HSLColor
  { hslH :: !CDouble
  , hslS :: !CDouble
  , hslL :: !CDouble
  } deriving (Eq, Show)

instance Storable HSLColor where
  sizeOf    _ = 24
  alignment _ = 8
  peek ptr = HSLColor
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 16
  poke ptr (HSLColor h s l) = do
    pokeByteOff ptr 0 h
    pokeByteOff ptr 8 s
    pokeByteOff ptr 16 l

-- | Result for color parsing.
data ColorParseResult = ColorParseResult
  { cprStatusRaw :: !CInt
  , cprColor     :: !RGBColor
  } deriving (Eq, Show)

instance Storable ColorParseResult where
  sizeOf    _ = 8
  alignment _ = 4
  peek ptr = ColorParseResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
  poke ptr (ColorParseResult s c) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 c

--------------------------------------------------------------------------------
-- Geo types
--------------------------------------------------------------------------------

-- | Geographic coordinate.
data GeoCoordinate = GeoCoordinate
  { geoLat :: !CDouble
  , geoLon :: !CDouble
  } deriving (Eq, Show)

instance Storable GeoCoordinate where
  sizeOf    _ = 16
  alignment _ = 8
  peek ptr = GeoCoordinate
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
  poke ptr (GeoCoordinate la lo) = do
    pokeByteOff ptr 0 la
    pokeByteOff ptr 8 lo

-- | Result for geo validation.
data GeoResult = GeoResult
  { grStatusRaw   :: !CInt
  , grCoordinate  :: !GeoCoordinate
  } deriving (Eq, Show)

instance Storable GeoResult where
  sizeOf    _ = 24
  alignment _ = 8
  peek ptr = GeoResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
  poke ptr (GeoResult s c) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 c

--------------------------------------------------------------------------------
-- Password types
--------------------------------------------------------------------------------

-- | Result for password validation.
data PasswordResult = PasswordResult
  { pwStrength     :: !CInt
  , pwHasLowercase :: !CChar
  , pwHasUppercase :: !CChar
  , pwHasDigit     :: !CChar
  , pwHasSpecial   :: !CChar
  , pwLength       :: !CSize
  } deriving (Eq, Show)

instance Storable PasswordResult where
  sizeOf    _ = 16
  alignment _ = 8
  peek ptr = PasswordResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 5
    <*> peekByteOff ptr 6
    <*> peekByteOff ptr 7
    <*> peekByteOff ptr 8
  poke ptr (PasswordResult st lo up di sp ln) = do
    pokeByteOff ptr 0 st
    pokeByteOff ptr 4 lo
    pokeByteOff ptr 5 up
    pokeByteOff ptr 6 di
    pokeByteOff ptr 7 sp
    pokeByteOff ptr 8 ln

--------------------------------------------------------------------------------
-- Retry types
--------------------------------------------------------------------------------

-- | Retry configuration.
data RetryConfig = RetryConfig
  { rcMaxAttempts  :: !Word32
  , rcBaseDelayMs  :: !Word64
  , rcMaxDelayMs   :: !Word64
  , rcMultiplier   :: !CDouble
  } deriving (Eq, Show)

instance Storable RetryConfig where
  sizeOf    _ = 32
  alignment _ = 8
  peek ptr = RetryConfig
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
  poke ptr (RetryConfig ma bd md mu) = do
    pokeByteOff ptr 0  ma
    pokeByteOff ptr 8  bd
    pokeByteOff ptr 16 md
    pokeByteOff ptr 24 mu

--------------------------------------------------------------------------------
-- Enum wrappers (passed as CInt / i32 on the C ABI)
--------------------------------------------------------------------------------

-- | JSON value type enum.
newtype JsonType = JsonType { unJsonType :: CInt }
  deriving (Eq, Show, Storable)

-- | Length unit enum for unit conversions.
newtype LengthUnitC = LengthUnitC { unLengthUnitC :: CInt }
  deriving (Eq, Show, Storable)

-- | Temperature unit enum.
newtype TempUnitC = TempUnitC { unTempUnitC :: CInt }
  deriving (Eq, Show, Storable)

-- | Circuit breaker state enum.
newtype CircuitStateC = CircuitStateC { unCircuitStateC :: CInt }
  deriving (Eq, Show, Storable)

-- | Password strength enum.
newtype PasswordStrengthC = PasswordStrengthC { unPasswordStrengthC :: CInt }
  deriving (Eq, Show, Storable)

-- | SameSite cookie attribute enum.
newtype SameSiteC = SameSiteC { unSameSiteC :: CInt }
  deriving (Eq, Show, Storable)

-- | Charset enum.
newtype CharsetC = CharsetC { unCharsetC :: CInt }
  deriving (Eq, Show, Storable)

-- | Media category enum.
newtype MediaCategoryC = MediaCategoryC { unMediaCategoryC :: CInt }
  deriving (Eq, Show, Storable)

--------------------------------------------------------------------------------
-- Cookie types
--------------------------------------------------------------------------------

-- | Cookie attributes for Set-Cookie header building.
data CookieAttributes = CookieAttributes
  { caDomain      :: !(Ptr CChar)
  , caDomainLen   :: !CSize
  , caPath        :: !(Ptr CChar)
  , caPathLen     :: !CSize
  , caMaxAge      :: !CLLong
  , caSecure      :: !CChar
  , caHttpOnly    :: !CChar
  , caSameSite    :: !SameSiteC
  , caPartitioned :: !CChar
  } deriving (Eq, Show)

instance Storable CookieAttributes where
  sizeOf    _ = 56
  alignment _ = 8
  peek ptr = CookieAttributes
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
    <*> peekByteOff ptr 32
    <*> peekByteOff ptr 40
    <*> peekByteOff ptr 41
    <*> peekByteOff ptr 44
    <*> peekByteOff ptr 48
  poke ptr (CookieAttributes d dl p pl ma s ho ss pa) = do
    pokeByteOff ptr 0  d
    pokeByteOff ptr 8  dl
    pokeByteOff ptr 16 p
    pokeByteOff ptr 24 pl
    pokeByteOff ptr 32 ma
    pokeByteOff ptr 40 s
    pokeByteOff ptr 41 ho
    pokeByteOff ptr 44 ss
    pokeByteOff ptr 48 pa

--------------------------------------------------------------------------------
-- ContentType types
--------------------------------------------------------------------------------

-- | Content type parse result.
data ContentTypeResult = ContentTypeResult
  { ctrStatusRaw    :: !CInt
  , ctrMediaType    :: !(Ptr CChar)
  , ctrMediaTypeLen :: !CSize
  , ctrSubtype      :: !(Ptr CChar)
  , ctrSubtypeLen   :: !CSize
  , ctrSuffix       :: !(Ptr CChar)
  , ctrSuffixLen    :: !CSize
  , ctrCategory     :: !MediaCategoryC
  , ctrCharset      :: !CharsetC
  , ctrHasCharset   :: !CChar
  } deriving (Eq, Show)

instance Storable ContentTypeResult where
  sizeOf    _ = 72
  alignment _ = 8
  peek ptr = ContentTypeResult
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 16
    <*> peekByteOff ptr 24
    <*> peekByteOff ptr 32
    <*> peekByteOff ptr 40
    <*> peekByteOff ptr 48
    <*> peekByteOff ptr 56
    <*> peekByteOff ptr 60
    <*> peekByteOff ptr 64
  poke ptr (ContentTypeResult s mt mtl st stl su sul cat cs hcs) = do
    pokeByteOff ptr 0  s
    pokeByteOff ptr 8  mt
    pokeByteOff ptr 16 mtl
    pokeByteOff ptr 24 st
    pokeByteOff ptr 32 stl
    pokeByteOff ptr 40 su
    pokeByteOff ptr 48 sul
    pokeByteOff ptr 56 cat
    pokeByteOff ptr 60 cs
    pokeByteOff ptr 64 hcs
