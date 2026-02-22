{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Cryptographic safety operations via libproven FFI.
--
-- Constant-time comparison and random byte generation are performed
-- by the Idris 2 verified core.
module Proven.SafeCrypto
  ( constantTimeCompare
  , randomBytes
  ) where

import Foreign
import Foreign.C.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Proven.FFI (c_proven_crypto_constant_time_eq, c_proven_crypto_random_bytes)
import Proven.Core (withByteString, boolResultToBool)

-- | Compare two byte strings in constant time (timing-attack resistant).
-- Delegates to @proven_crypto_constant_time_eq@ in libproven.
constantTimeCompare :: BS.ByteString -> BS.ByteString -> IO (Maybe Bool)
constantTimeCompare a b =
  withByteString a $ \ptrA lenA ->
    withByteString b $ \ptrB lenB ->
      boolResultToBool <$> c_proven_crypto_constant_time_eq ptrA lenA ptrB lenB

-- | Generate cryptographically secure random bytes.
-- Delegates to @proven_crypto_random_bytes@ in libproven.
randomBytes :: Int -> IO (Maybe BS.ByteString)
randomBytes n
  | n <= 0 = return Nothing
  | otherwise = do
      fptr <- mallocForeignPtrBytes n
      withForeignPtr fptr $ \ptr -> do
        status <- c_proven_crypto_random_bytes ptr (fromIntegral n)
        if status == 0
          then return (Just (BSI.fromForeignPtr fptr 0 n))
          else return Nothing
