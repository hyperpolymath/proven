{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe color operations with validation.
--
-- Provides secure color handling with RGB, HSL, HSV, and hex
-- conversion and manipulation.
module Proven.SafeColor
  ( -- * Types
    RGB(..)
  , RGBA(..)
  , HSL(..)
  , HSV(..)
  , Color(..)
    -- * Construction
  , makeRgb
  , makeRgba
  , makeHsl
  , makeHsv
  , fromHex
  , fromHex8
    -- * Conversion
  , rgbToHsl
  , hslToRgb
  , rgbToHsv
  , hsvToRgb
  , toHex
  , toHex8
    -- * Manipulation
  , lighten
  , darken
  , saturate
  , desaturate
  , adjustHue
  , invert
  , complement
  , grayscale
    -- * Blending
  , mixColors
  , blendNormal
  , blendMultiply
  , blendScreen
  , blendOverlay
    -- * Analysis
  , luminance
  , contrast
  , isLight
  , isDark
    -- * Validation
  , isValidHex
    -- * Named Colors
  , white
  , black
  , red
  , green
  , blue
  , transparent
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isHexDigit, digitToInt)
import Data.Word (Word8)
import Proven.Core (ProvenError(..), Result)

-- | RGB color with 8-bit components.
data RGB = RGB
  { rgbR :: !Word8
  , rgbG :: !Word8
  , rgbB :: !Word8
  } deriving (Eq, Show)

-- | RGBA color with alpha channel.
data RGBA = RGBA
  { rgbaR :: !Word8
  , rgbaG :: !Word8
  , rgbaB :: !Word8
  , rgbaA :: !Word8
  } deriving (Eq, Show)

-- | HSL color (hue, saturation, lightness).
data HSL = HSL
  { hslH :: !Double  -- ^ Hue [0, 360)
  , hslS :: !Double  -- ^ Saturation [0, 1]
  , hslL :: !Double  -- ^ Lightness [0, 1]
  } deriving (Eq, Show)

-- | HSV color (hue, saturation, value).
data HSV = HSV
  { hsvH :: !Double  -- ^ Hue [0, 360)
  , hsvS :: !Double  -- ^ Saturation [0, 1]
  , hsvV :: !Double  -- ^ Value [0, 1]
  } deriving (Eq, Show)

-- | Generic color representation.
data Color
  = ColorRGB !RGB
  | ColorRGBA !RGBA
  | ColorHSL !HSL
  | ColorHSV !HSV
  deriving (Eq, Show)

-- | Create an RGB color.
makeRgb :: Word8 -> Word8 -> Word8 -> RGB
makeRgb = RGB

-- | Create an RGBA color.
makeRgba :: Word8 -> Word8 -> Word8 -> Word8 -> RGBA
makeRgba = RGBA

-- | Create an HSL color with validation.
makeHsl :: Double -> Double -> Double -> Result HSL
makeHsl h s l
  | h < 0 || h >= 360 = Left (OutOfRange "Hue must be in [0, 360)")
  | s < 0 || s > 1 = Left (OutOfRange "Saturation must be in [0, 1]")
  | l < 0 || l > 1 = Left (OutOfRange "Lightness must be in [0, 1]")
  | otherwise = Right (HSL h s l)

-- | Create an HSV color with validation.
makeHsv :: Double -> Double -> Double -> Result HSV
makeHsv h s v
  | h < 0 || h >= 360 = Left (OutOfRange "Hue must be in [0, 360)")
  | s < 0 || s > 1 = Left (OutOfRange "Saturation must be in [0, 1]")
  | v < 0 || v > 1 = Left (OutOfRange "Value must be in [0, 1]")
  | otherwise = Right (HSV h s v)

-- | Parse hex color string (e.g., "#FF0000" or "FF0000").
fromHex :: Text -> Result RGB
fromHex input = do
  let cleaned = T.toLower $ T.dropWhile (== '#') input
  case T.length cleaned of
    3 -> parseShortHex cleaned
    6 -> parseLongHex cleaned
    _ -> Left (InvalidFormat "Hex color must be 3 or 6 characters")

parseShortHex :: Text -> Result RGB
parseShortHex t = do
  let chars = T.unpack t
  case chars of
    [r, g, b] | all isHexDigit chars -> do
      let expand c = let v = digitToInt c in fromIntegral (v * 16 + v)
      Right (RGB (expand r) (expand g) (expand b))
    _ -> Left (InvalidFormat "Invalid hex characters")

parseLongHex :: Text -> Result RGB
parseLongHex t = do
  let chars = T.unpack t
  case chars of
    [r1, r2, g1, g2, b1, b2] | all isHexDigit chars -> do
      let parse c1 c2 = fromIntegral (digitToInt c1 * 16 + digitToInt c2)
      Right (RGB (parse r1 r2) (parse g1 g2) (parse b1 b2))
    _ -> Left (InvalidFormat "Invalid hex characters")

-- | Parse 8-character hex with alpha (e.g., "#FF0000FF").
fromHex8 :: Text -> Result RGBA
fromHex8 input = do
  let cleaned = T.toLower $ T.dropWhile (== '#') input
  case T.length cleaned of
    8 -> parseLongHex8 cleaned
    _ -> Left (InvalidFormat "Hex+alpha must be 8 characters")

parseLongHex8 :: Text -> Result RGBA
parseLongHex8 t = do
  let chars = T.unpack t
  case chars of
    [r1, r2, g1, g2, b1, b2, a1, a2] | all isHexDigit chars -> do
      let parse c1 c2 = fromIntegral (digitToInt c1 * 16 + digitToInt c2)
      Right (RGBA (parse r1 r2) (parse g1 g2) (parse b1 b2) (parse a1 a2))
    _ -> Left (InvalidFormat "Invalid hex characters")

-- | Convert RGB to HSL.
rgbToHsl :: RGB -> HSL
rgbToHsl (RGB r g b) =
  let r' = fromIntegral r / 255
      g' = fromIntegral g / 255
      b' = fromIntegral b / 255
      maxC = maximum [r', g', b']
      minC = minimum [r', g', b']
      l = (maxC + minC) / 2
      d = maxC - minC
      s = if d == 0 then 0 else d / (1 - abs (2 * l - 1))
      h | d == 0 = 0
        | maxC == r' = 60 * (((g' - b') / d) `mod'` 6)
        | maxC == g' = 60 * (((b' - r') / d) + 2)
        | otherwise = 60 * (((r' - g') / d) + 4)
  in HSL (if h < 0 then h + 360 else h) s l
  where
    mod' a n = a - n * fromIntegral (floor (a / n) :: Int)

-- | Convert HSL to RGB.
hslToRgb :: HSL -> RGB
hslToRgb (HSL h s l) =
  let c = (1 - abs (2 * l - 1)) * s
      x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
      m = l - c / 2
      (r', g', b')
        | h < 60  = (c, x, 0)
        | h < 120 = (x, c, 0)
        | h < 180 = (0, c, x)
        | h < 240 = (0, x, c)
        | h < 300 = (x, 0, c)
        | otherwise = (c, 0, x)
      toWord8 v = round ((v + m) * 255)
  in RGB (toWord8 r') (toWord8 g') (toWord8 b')
  where
    mod' a n = a - n * fromIntegral (floor (a / n) :: Int)

-- | Convert RGB to HSV.
rgbToHsv :: RGB -> HSV
rgbToHsv (RGB r g b) =
  let r' = fromIntegral r / 255
      g' = fromIntegral g / 255
      b' = fromIntegral b / 255
      maxC = maximum [r', g', b']
      minC = minimum [r', g', b']
      d = maxC - minC
      v = maxC
      s = if maxC == 0 then 0 else d / maxC
      h | d == 0 = 0
        | maxC == r' = 60 * (((g' - b') / d) `mod'` 6)
        | maxC == g' = 60 * (((b' - r') / d) + 2)
        | otherwise = 60 * (((r' - g') / d) + 4)
  in HSV (if h < 0 then h + 360 else h) s v
  where
    mod' a n = a - n * fromIntegral (floor (a / n) :: Int)

-- | Convert HSV to RGB.
hsvToRgb :: HSV -> RGB
hsvToRgb (HSV h s v) =
  let c = v * s
      x = c * (1 - abs ((h / 60) `mod'` 2 - 1))
      m = v - c
      (r', g', b')
        | h < 60  = (c, x, 0)
        | h < 120 = (x, c, 0)
        | h < 180 = (0, c, x)
        | h < 240 = (0, x, c)
        | h < 300 = (x, 0, c)
        | otherwise = (c, 0, x)
      toWord8 val = round ((val + m) * 255)
  in RGB (toWord8 r') (toWord8 g') (toWord8 b')
  where
    mod' a n = a - n * fromIntegral (floor (a / n) :: Int)

-- | Convert RGB to hex string.
toHex :: RGB -> Text
toHex (RGB r g b) = T.pack $ '#' : toHexPair r ++ toHexPair g ++ toHexPair b
  where
    toHexPair n = [hexDigit (n `div` 16), hexDigit (n `mod` 16)]
    hexDigit n = "0123456789abcdef" !! fromIntegral n

-- | Convert RGBA to 8-character hex string.
toHex8 :: RGBA -> Text
toHex8 (RGBA r g b a) = T.pack $ '#' : toHexPair r ++ toHexPair g ++ toHexPair b ++ toHexPair a
  where
    toHexPair n = [hexDigit (n `div` 16), hexDigit (n `mod` 16)]
    hexDigit n = "0123456789abcdef" !! fromIntegral n

-- | Lighten a color by a factor [0, 1].
lighten :: Double -> RGB -> RGB
lighten amount rgb =
  let (HSL h s l) = rgbToHsl rgb
      newL = min 1 (l + amount)
  in hslToRgb (HSL h s newL)

-- | Darken a color by a factor [0, 1].
darken :: Double -> RGB -> RGB
darken amount rgb =
  let (HSL h s l) = rgbToHsl rgb
      newL = max 0 (l - amount)
  in hslToRgb (HSL h s newL)

-- | Saturate a color by a factor [0, 1].
saturate :: Double -> RGB -> RGB
saturate amount rgb =
  let (HSL h s l) = rgbToHsl rgb
      newS = min 1 (s + amount)
  in hslToRgb (HSL h newS l)

-- | Desaturate a color by a factor [0, 1].
desaturate :: Double -> RGB -> RGB
desaturate amount rgb =
  let (HSL h s l) = rgbToHsl rgb
      newS = max 0 (s - amount)
  in hslToRgb (HSL h newS l)

-- | Adjust hue by degrees.
adjustHue :: Double -> RGB -> RGB
adjustHue degrees rgb =
  let (HSL h s l) = rgbToHsl rgb
      newH = (h + degrees) `mod'` 360
  in hslToRgb (HSL (if newH < 0 then newH + 360 else newH) s l)
  where
    mod' a n = a - n * fromIntegral (floor (a / n) :: Int)

-- | Invert a color.
invert :: RGB -> RGB
invert (RGB r g b) = RGB (255 - r) (255 - g) (255 - b)

-- | Get complementary color (180 degree hue shift).
complement :: RGB -> RGB
complement = adjustHue 180

-- | Convert to grayscale.
grayscale :: RGB -> RGB
grayscale (RGB r g b) =
  let gray = round (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)
  in RGB gray gray gray

-- | Mix two colors with weight [0, 1] for first color.
mixColors :: Double -> RGB -> RGB -> RGB
mixColors weight (RGB r1 g1 b1) (RGB r2 g2 b2) =
  let mix c1 c2 = round (fromIntegral c1 * weight + fromIntegral c2 * (1 - weight))
  in RGB (mix r1 r2) (mix g1 g2) (mix b1 b2)

-- | Normal blend mode.
blendNormal :: Double -> RGB -> RGB -> RGB
blendNormal = mixColors

-- | Multiply blend mode.
blendMultiply :: RGB -> RGB -> RGB
blendMultiply (RGB r1 g1 b1) (RGB r2 g2 b2) =
  let blend c1 c2 = round (fromIntegral c1 * fromIntegral c2 / 255 :: Double)
  in RGB (blend r1 r2) (blend g1 g2) (blend b1 b2)

-- | Screen blend mode.
blendScreen :: RGB -> RGB -> RGB
blendScreen (RGB r1 g1 b1) (RGB r2 g2 b2) =
  let blend c1 c2 = 255 - round ((255 - fromIntegral c1) * (255 - fromIntegral c2) / 255 :: Double)
  in RGB (blend r1 r2) (blend g1 g2) (blend b1 b2)

-- | Overlay blend mode.
blendOverlay :: RGB -> RGB -> RGB
blendOverlay base@(RGB r1 g1 b1) blend@(RGB r2 g2 b2) =
  let overlay c1 c2
        | c1 < 128 = round (2 * fromIntegral c1 * fromIntegral c2 / 255 :: Double)
        | otherwise = 255 - round (2 * (255 - fromIntegral c1) * (255 - fromIntegral c2) / 255 :: Double)
  in RGB (overlay r1 r2) (overlay g1 g2) (overlay b1 b2)

-- | Calculate relative luminance.
luminance :: RGB -> Double
luminance (RGB r g b) =
  let srgbToLinear c =
        let c' = fromIntegral c / 255
        in if c' <= 0.03928 then c' / 12.92 else ((c' + 0.055) / 1.055) ** 2.4
  in 0.2126 * srgbToLinear r + 0.7152 * srgbToLinear g + 0.0722 * srgbToLinear b

-- | Calculate contrast ratio between two colors.
contrast :: RGB -> RGB -> Double
contrast c1 c2 =
  let l1 = luminance c1
      l2 = luminance c2
      lighter = max l1 l2
      darker = min l1 l2
  in (lighter + 0.05) / (darker + 0.05)

-- | Check if color is light.
isLight :: RGB -> Bool
isLight rgb = luminance rgb > 0.5

-- | Check if color is dark.
isDark :: RGB -> Bool
isDark = not . isLight

-- | Check if string is valid hex color.
isValidHex :: Text -> Bool
isValidHex input = case fromHex input of
  Right _ -> True
  Left _ -> False

-- Named colors

-- | White.
white :: RGB
white = RGB 255 255 255

-- | Black.
black :: RGB
black = RGB 0 0 0

-- | Red.
red :: RGB
red = RGB 255 0 0

-- | Green.
green :: RGB
green = RGB 0 255 0

-- | Blue.
blue :: RGB
blue = RGB 0 0 255

-- | Transparent (RGBA).
transparent :: RGBA
transparent = RGBA 0 0 0 0
