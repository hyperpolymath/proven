-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeColor - FFI bindings to libproven color operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeColor
  ( parseHexColor
  , rgbToHsl
  , rgbToHex
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Parse a hex color string (delegates to Idris 2).
-- | Returns RGB components or error.
foreign import parseHexColorImpl :: String ->
  { status :: Int
  , r :: Int
  , g :: Int
  , b :: Int
  }

parseHexColor :: String -> Result { r :: Int, g :: Int, b :: Int } ProvenError
parseHexColor s =
  let result = parseHexColorImpl s
  in if result.status == 0
     then Ok { r: result.r, g: result.g, b: result.b }
     else Err InvalidColor

-- | Convert RGB to HSL (delegates to Idris 2).
foreign import rgbToHslImpl :: { r :: Int, g :: Int, b :: Int } -> { h :: Number, s :: Number, l :: Number }

rgbToHsl :: { r :: Int, g :: Int, b :: Int } -> { h :: Number, s :: Number, l :: Number }
rgbToHsl = rgbToHslImpl

-- | Convert RGB to hex string (delegates to Idris 2).
foreign import toHexImpl :: { r :: Int, g :: Int, b :: Int } -> String

rgbToHex :: { r :: Int, g :: Int, b :: Int } -> String
rgbToHex = toHexImpl
