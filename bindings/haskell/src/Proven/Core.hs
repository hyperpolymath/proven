{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Core types and helpers for safe Haskell wrappers around libproven FFI.
--
-- Provides error types, marshaling helpers for strings and byte buffers,
-- and common patterns used across all @Proven.Safe*@ modules.
--
-- __No computation is reimplemented here__: all helpers exist solely
-- to marshal data between Haskell and the C ABI.
module Proven.Core
  ( -- * Error Types
    ProvenError(..)
  , Result
    -- * Marshaling Helpers
  , withCStringLen'
  , withByteString
  , peekProvenString
  , intResultToMaybe
  , intResultToEither
  , boolResultToBool
  , floatResultToMaybe
  , floatResultToEither
  , stringResultToMaybe
  , stringResultToEither
  , ensureInit
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Proven.FFI.Types

-- | Error types corresponding to @ProvenStatus@ codes from libproven.
data ProvenError
  = ErrNullPointer       -- ^ Null pointer was passed
  | ErrInvalidArgument   -- ^ Invalid argument
  | ErrOverflow          -- ^ Arithmetic overflow
  | ErrUnderflow         -- ^ Arithmetic underflow
  | ErrDivisionByZero    -- ^ Division by zero
  | ErrParseFailure      -- ^ Parse failure
  | ErrValidationFailed  -- ^ Validation failed
  | ErrOutOfBounds       -- ^ Out of bounds
  | ErrEncodingError     -- ^ Encoding error
  | ErrAllocationFailed  -- ^ Memory allocation failed
  | ErrNotImplemented    -- ^ Not implemented
  | ErrUnknown !Int      -- ^ Unknown error code
  deriving (Eq, Show)

-- | Convenient alias for results from libproven.
type Result a = Either ProvenError a

-- | Convert a raw CInt status to a ProvenError.
statusToError :: CInt -> ProvenError
statusToError (-1)  = ErrNullPointer
statusToError (-2)  = ErrInvalidArgument
statusToError (-3)  = ErrOverflow
statusToError (-4)  = ErrUnderflow
statusToError (-5)  = ErrDivisionByZero
statusToError (-6)  = ErrParseFailure
statusToError (-7)  = ErrValidationFailed
statusToError (-8)  = ErrOutOfBounds
statusToError (-9)  = ErrEncodingError
statusToError (-10) = ErrAllocationFailed
statusToError (-99) = ErrNotImplemented
statusToError n     = ErrUnknown (fromIntegral n)

-- | Use a Haskell String as a C byte buffer with length.
-- The string is encoded as UTF-8 via 'withCStringLen'.
withCStringLen' :: String -> (Ptr Word8 -> CSize -> IO a) -> IO a
withCStringLen' str action =
  withCStringLen str $ \(ptr, len) ->
    action (castPtr ptr) (fromIntegral len)

-- | Use a ByteString as a C byte buffer with length.
withByteString :: BS.ByteString -> (Ptr Word8 -> CSize -> IO a) -> IO a
withByteString bs action =
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    action (castPtr ptr) (fromIntegral len)

-- | Peek a string from a StringResult, freeing the C-allocated string.
-- Returns Nothing if the status is not ok or the pointer is null.
peekProvenString :: StringResult -> (Ptr CChar -> IO ()) -> IO (Maybe String)
peekProvenString sr freeStr
  | srStatusRaw sr /= 0 = return Nothing
  | srValue sr == nullPtr = return Nothing
  | otherwise = do
      str <- peekCString (srValue sr)
      freeStr (srValue sr)
      return (Just str)

-- | Convert an IntResult to Maybe Int.
intResultToMaybe :: IntResult -> Maybe Int
intResultToMaybe ir
  | irStatusRaw ir == 0 = Just (fromIntegral (irValue ir))
  | otherwise           = Nothing

-- | Convert an IntResult to Either ProvenError Int.
intResultToEither :: IntResult -> Either ProvenError Int
intResultToEither ir
  | irStatusRaw ir == 0 = Right (fromIntegral (irValue ir))
  | otherwise           = Left (statusToError (irStatusRaw ir))

-- | Convert a BoolResult to Maybe Bool.
boolResultToBool :: BoolResult -> Maybe Bool
boolResultToBool br
  | brStatusRaw br == 0 = Just (brValue br /= 0)
  | otherwise           = Nothing

-- | Convert a FloatResult to Maybe Double.
floatResultToMaybe :: FloatResult -> Maybe Double
floatResultToMaybe fr
  | frStatusRaw fr == 0 = Just (realToFrac (frValue fr))
  | otherwise           = Nothing

-- | Convert a FloatResult to Either ProvenError Double.
floatResultToEither :: FloatResult -> Either ProvenError Double
floatResultToEither fr
  | frStatusRaw fr == 0 = Right (realToFrac (frValue fr))
  | otherwise           = Left (statusToError (frStatusRaw fr))

-- | Convert a StringResult to Maybe String, freeing the C string.
stringResultToMaybe :: (Ptr CChar -> IO ()) -> StringResult -> IO (Maybe String)
stringResultToMaybe = flip peekProvenString

-- | Convert a StringResult to Either ProvenError String, freeing the C string.
stringResultToEither :: (Ptr CChar -> IO ()) -> StringResult -> IO (Either ProvenError String)
stringResultToEither freeStr sr
  | srStatusRaw sr /= 0 = return (Left (statusToError (srStatusRaw sr)))
  | srValue sr == nullPtr = return (Left ErrNullPointer)
  | otherwise = do
      str <- peekCString (srValue sr)
      freeStr (srValue sr)
      return (Right str)

-- | Ensure the proven runtime is initialized. Returns True if already
-- initialized or initialization succeeded.
ensureInit :: IO CInt -> IO Bool
ensureInit initFn = do
  result <- initFn
  return (result == 0)
