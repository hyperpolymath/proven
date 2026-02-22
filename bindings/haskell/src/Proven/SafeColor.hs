{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe color operations via libproven FFI.
--
-- Color parsing and conversion are performed by the Idris 2 verified core.
module Proven.SafeColor
  ( RGB(..)
  , HSL(..)
  , parseHexColor
  , rgbToHsl
  , colorToHex
  ) where

import Data.Word (Word8)
import Proven.FFI (c_proven_color_parse_hex, c_proven_color_rgb_to_hsl,
                   c_proven_color_to_hex, c_proven_free_string)
import Proven.FFI.Types (RGBColor(..), HSLColor(..), ColorParseResult(..))
import Proven.Core (withCStringLen', stringResultToMaybe)

-- | RGB color with 8-bit components.
data RGB = RGB
  { rgbRed   :: !Word8
  , rgbGreen :: !Word8
  , rgbBlue  :: !Word8
  } deriving (Eq, Show)

-- | HSL color (hue 0-360, saturation 0-1, lightness 0-1).
data HSL = HSL
  { hslHue        :: !Double
  , hslSaturation :: !Double
  , hslLightness  :: !Double
  } deriving (Eq, Show)

-- | Parse a hex color string (#RRGGBB or #RGB).
-- Delegates to @proven_color_parse_hex@ in libproven.
parseHexColor :: String -> IO (Maybe RGB)
parseHexColor str = withCStringLen' str $ \ptr len -> do
  result <- c_proven_color_parse_hex ptr len
  if cprStatusRaw result == 0
    then let c = cprColor result
         in return (Just (RGB (rgbR c) (rgbG c) (rgbB c)))
    else return Nothing

-- | Convert RGB to HSL.
-- Delegates to @proven_color_rgb_to_hsl@ in libproven.
rgbToHsl :: RGB -> IO HSL
rgbToHsl (RGB r g b) = do
  result <- c_proven_color_rgb_to_hsl (RGBColor r g b)
  return (HSL (realToFrac (hslH result))
              (realToFrac (hslS result))
              (realToFrac (hslL result)))

-- | Format RGB color as hex string.
-- Delegates to @proven_color_to_hex@ in libproven.
colorToHex :: RGB -> IO (Maybe String)
colorToHex (RGB r g b) = do
  sr <- c_proven_color_to_hex (RGBColor r g b)
  stringResultToMaybe c_proven_free_string sr
