{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe hexadecimal encoding/decoding via libproven FFI.
--
-- All hex operations are performed by the Idris 2 verified core.
module Proven.SafeHex
  ( hexEncode
  , hexEncodeUpper
  ) where

import qualified Data.ByteString as BS
import Proven.FFI (c_proven_hex_encode, c_proven_free_string)
import Proven.Core (withByteString, stringResultToMaybe)

-- | Encode bytes as lowercase hex string.
-- Delegates to @proven_hex_encode@ in libproven.
hexEncode :: BS.ByteString -> IO (Maybe String)
hexEncode bs = withByteString bs $ \ptr len -> do
  sr <- c_proven_hex_encode ptr len 0  -- 0 = lowercase
  stringResultToMaybe c_proven_free_string sr

-- | Encode bytes as uppercase hex string.
-- Delegates to @proven_hex_encode@ in libproven.
hexEncodeUpper :: BS.ByteString -> IO (Maybe String)
hexEncodeUpper bs = withByteString bs $ \ptr len -> do
  sr <- c_proven_hex_encode ptr len 1  -- 1 = uppercase
  stringResultToMaybe c_proven_free_string sr
