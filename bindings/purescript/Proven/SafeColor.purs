-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe color handling and conversion.
-- |
-- | Provides validated color values with conversions between
-- | RGB, HSL, and hex formats.

module Proven.SafeColor
  ( SafeColor
  , Color(..)
  , Rgb(..)
  , Hsl(..)
  , parseHex
  , parseRgb
  , parseHsl
  , toHex
  , toRgb
  , toHsl
  , isValidHex
  , lighten
  , darken
  , saturate
  , desaturate
  , invert
  , mix
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Int (floor, toNumber)
import Proven.Result (Result(..), ProvenError(..))

-- | SafeColor namespace marker (not instantiated).
data SafeColor

-- | RGB color (red, green, blue values 0-255).
newtype Rgb = Rgb { r :: Int, g :: Int, b :: Int }

derive instance eqRgb :: Eq Rgb

instance showRgb :: Show Rgb where
  show (Rgb c) = "rgb(" <> show c.r <> ", " <> show c.g <> ", " <> show c.b <> ")"

-- | HSL color (hue 0-360, saturation 0-100, lightness 0-100).
newtype Hsl = Hsl { h :: Number, s :: Number, l :: Number }

derive instance eqHsl :: Eq Hsl

instance showHsl :: Show Hsl where
  show (Hsl c) = "hsl(" <> show c.h <> ", " <> show c.s <> "%, " <> show c.l <> "%)"

-- | Unified color type.
data Color
  = ColorRgb Rgb
  | ColorHsl Hsl
  | ColorHex String

derive instance eqColor :: Eq Color

instance showColor :: Show Color where
  show (ColorRgb rgb) = show rgb
  show (ColorHsl hsl) = show hsl
  show (ColorHex hex) = hex

-- | Check if a string is a valid hex color.
isValidHex :: String -> Boolean
isValidHex hex = isValidHexColorImpl hex

foreign import isValidHexColorImpl :: String -> Boolean

-- | Parse a hex color string (#RGB or #RRGGBB).
parseHex :: String -> Result Rgb ProvenError
parseHex hex
  | not (isValidHex hex) = Err InvalidColor
  | otherwise =
      let result = parseHexImpl hex
      in Ok (Rgb { r: result.r, g: result.g, b: result.b })

foreign import parseHexImpl :: String -> { r :: Int, g :: Int, b :: Int }

-- | Parse an RGB color from values.
parseRgb :: Int -> Int -> Int -> Result Rgb ProvenError
parseRgb r g b
  | r < 0 || r > 255 = Err InvalidColor
  | g < 0 || g > 255 = Err InvalidColor
  | b < 0 || b > 255 = Err InvalidColor
  | otherwise = Ok (Rgb { r, g, b })

-- | Parse an HSL color from values.
parseHsl :: Number -> Number -> Number -> Result Hsl ProvenError
parseHsl h s l
  | h < 0.0 || h > 360.0 = Err InvalidColor
  | s < 0.0 || s > 100.0 = Err InvalidColor
  | l < 0.0 || l > 100.0 = Err InvalidColor
  | otherwise = Ok (Hsl { h, s, l })

-- | Convert to hex string.
toHex :: Rgb -> String
toHex (Rgb c) = toHexColorImpl c.r c.g c.b

foreign import toHexColorImpl :: Int -> Int -> Int -> String

-- | Convert HSL to RGB.
toRgb :: Hsl -> Rgb
toRgb (Hsl c) =
  let result = hslToRgbImpl c.h c.s c.l
  in Rgb { r: result.r, g: result.g, b: result.b }

foreign import hslToRgbImpl :: Number -> Number -> Number -> { r :: Int, g :: Int, b :: Int }

-- | Convert RGB to HSL.
toHsl :: Rgb -> Hsl
toHsl (Rgb c) =
  let result = rgbToHslImpl c.r c.g c.b
  in Hsl { h: result.h, s: result.s, l: result.l }

foreign import rgbToHslImpl :: Int -> Int -> Int -> { h :: Number, s :: Number, l :: Number }

-- | Lighten a color by a percentage.
lighten :: Number -> Hsl -> Hsl
lighten amount (Hsl c) =
  let newL = min 100.0 (c.l + amount)
  in Hsl c { l = newL }

-- | Darken a color by a percentage.
darken :: Number -> Hsl -> Hsl
darken amount (Hsl c) =
  let newL = max 0.0 (c.l - amount)
  in Hsl c { l = newL }

-- | Increase saturation by a percentage.
saturate :: Number -> Hsl -> Hsl
saturate amount (Hsl c) =
  let newS = min 100.0 (c.s + amount)
  in Hsl c { s = newS }

-- | Decrease saturation by a percentage.
desaturate :: Number -> Hsl -> Hsl
desaturate amount (Hsl c) =
  let newS = max 0.0 (c.s - amount)
  in Hsl c { s = newS }

-- | Invert a color.
invert :: Rgb -> Rgb
invert (Rgb c) = Rgb { r: 255 - c.r, g: 255 - c.g, b: 255 - c.b }

-- | Mix two colors with a given weight (0-1).
mix :: Number -> Rgb -> Rgb -> Rgb
mix weight (Rgb a) (Rgb b) =
  let
    w = clamp 0.0 1.0 weight
    mixChannel ac bc = floor (toNumber ac * w + toNumber bc * (1.0 - w))
  in Rgb
    { r: mixChannel a.r b.r
    , g: mixChannel a.g b.g
    , b: mixChannel a.b b.b
    }
  where
    clamp minVal maxVal v
      | v < minVal = minVal
      | v > maxVal = maxVal
      | otherwise = v
