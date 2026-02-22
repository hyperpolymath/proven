-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeHex - FFI bindings to libproven hexadecimal encoding/decoding
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeHex
  ( hexEncode
  , hexEncodeUpper
  , hexDecode
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Encode bytes (array of ints 0-255) to lowercase hex string (delegates to Idris 2).
foreign import hexEncodeImpl :: Array Int -> Boolean -> String

hexEncode :: Array Int -> String
hexEncode bytes = hexEncodeImpl bytes false

-- | Encode bytes (array of ints 0-255) to uppercase hex string (delegates to Idris 2).
hexEncodeUpper :: Array Int -> String
hexEncodeUpper bytes = hexEncodeImpl bytes true

-- | Decode hex string to bytes (delegates to Idris 2).
-- | Returns error for invalid hex input.
foreign import hexDecodeImpl :: String -> { status :: Int, bytes :: Array Int }

hexDecode :: String -> Result (Array Int) ProvenError
hexDecode s =
  let r = hexDecodeImpl s
  in if r.status == 0 then Ok r.bytes else Err InvalidHex
