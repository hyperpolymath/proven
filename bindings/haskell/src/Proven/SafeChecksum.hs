{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe checksum operations via libproven FFI.
--
-- CRC32 computation and verification are performed by the
-- Idris 2 verified core.
module Proven.SafeChecksum
  ( crc32
  , verifyCrc32
  ) where

import Data.Word (Word32)
import qualified Data.ByteString as BS
import Proven.FFI (c_proven_checksum_crc32, c_proven_checksum_verify_crc32)
import Proven.Core (withByteString, intResultToMaybe, boolResultToBool)

-- | Calculate CRC32 checksum of a byte string.
-- Delegates to @proven_checksum_crc32@ in libproven.
crc32 :: BS.ByteString -> IO (Maybe Int)
crc32 bs = withByteString bs $ \ptr len ->
  intResultToMaybe <$> c_proven_checksum_crc32 ptr len

-- | Verify CRC32 matches an expected value.
-- Delegates to @proven_checksum_verify_crc32@ in libproven.
verifyCrc32 :: BS.ByteString -> Word32 -> IO (Maybe Bool)
verifyCrc32 bs expected = withByteString bs $ \ptr len ->
  boolResultToBool <$> c_proven_checksum_verify_crc32 ptr len expected
