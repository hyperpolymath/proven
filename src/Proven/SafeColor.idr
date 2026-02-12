-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeColor - Safe color handling and conversion
|||
||| This module provides safe color operations including
||| parsing, conversion between color spaces, and WCAG
||| contrast calculations.
module Proven.SafeColor

import public Proven.Core
import public Proven.SafeHex
import Data.String

%default total

--------------------------------------------------------------------------------
-- Color Types
--------------------------------------------------------------------------------

||| RGB color with values 0-255
public export
record RGB where
  constructor MkRGB
  red : Nat
  green : Nat
  blue : Nat

||| RGBA color with alpha channel
public export
record RGBA where
  constructor MkRGBA
  red : Nat
  green : Nat
  blue : Nat
  alpha : Nat

||| HSL color (hue 0-360, saturation 0-100, lightness 0-100)
public export
record HSL where
  constructor MkHSL
  hue : Double
  saturation : Double
  lightness : Double

public export
Eq RGB where
  a == b = a.red == b.red && a.green == b.green && a.blue == b.blue

public export
Eq RGBA where
  a == b = a.red == b.red && a.green == b.green && a.blue == b.blue && a.alpha == b.alpha

--------------------------------------------------------------------------------
-- Color Parsing
--------------------------------------------------------------------------------

||| Parse a hex color (#RGB, #RRGGBB, or #RRGGBBAA)
public export
parseHex : String -> Maybe RGBA
parseHex s =
  let hex = if isPrefixOf "#" s then drop 1 s else s
  in case length hex of
       3 => parseShortHex hex
       6 => parseLongHex hex
       8 => parseAlphaHex hex
       _ => Nothing
  where
    parseShortHex : String -> Maybe RGBA
    parseShortHex str = do
      r <- hexToNibble =<< strIndex str 0
      g <- hexToNibble =<< strIndex str 1
      b <- hexToNibble =<< strIndex str 2
      Just (MkRGBA (r * 17) (g * 17) (b * 17) 255)

    parseLongHex : String -> Maybe RGBA
    parseLongHex str = do
      c0 <- strIndex str 0
      c1 <- strIndex str 1
      c2 <- strIndex str 2
      c3 <- strIndex str 3
      c4 <- strIndex str 4
      c5 <- strIndex str 5
      r <- hexPairToByte c0 c1
      g <- hexPairToByte c2 c3
      b <- hexPairToByte c4 c5
      Just (MkRGBA r g b 255)

    parseAlphaHex : String -> Maybe RGBA
    parseAlphaHex str = do
      c0 <- strIndex str 0
      c1 <- strIndex str 1
      c2 <- strIndex str 2
      c3 <- strIndex str 3
      c4 <- strIndex str 4
      c5 <- strIndex str 5
      c6 <- strIndex str 6
      c7 <- strIndex str 7
      r <- hexPairToByte c0 c1
      g <- hexPairToByte c2 c3
      b <- hexPairToByte c4 c5
      a <- hexPairToByte c6 c7
      Just (MkRGBA r g b a)

||| Convert color to hex string
public export
toHex : RGBA -> String
toHex c =
  let r = padHex 2 (intToHex (cast c.red))
      g = padHex 2 (intToHex (cast c.green))
      b = padHex 2 (intToHex (cast c.blue))
  in if c.alpha == 255
     then "#" ++ r ++ g ++ b
     else "#" ++ r ++ g ++ b ++ padHex 2 (intToHex (cast c.alpha))

--------------------------------------------------------------------------------
-- Color Conversion
--------------------------------------------------------------------------------

||| Convert RGB to HSL
public export
rgbToHSL : RGB -> HSL
rgbToHSL c =
  let r = cast c.red / 255.0
      g = cast c.green / 255.0
      b = cast c.blue / 255.0
      maxC = max r (max g b)
      minC = min r (min g b)
      l = (maxC + minC) / 2.0
      d = maxC - minC
  in if d < 0.00001
     then MkHSL 0.0 0.0 (l * 100.0)
     else
       let s = if l > 0.5 then d / (2.0 - maxC - minC) else d / (maxC + minC)
           h = if maxC == r then (g - b) / d + (if g < b then 6.0 else 0.0)
               else if maxC == g then (b - r) / d + 2.0
               else (r - g) / d + 4.0
       in MkHSL (h * 60.0) (s * 100.0) (l * 100.0)

||| Convert HSL to RGB
public export
hslToRGB : HSL -> RGB
hslToRGB c =
  let h = c.hue / 360.0
      s = c.saturation / 100.0
      l = c.lightness / 100.0
  in if s < 0.00001
     then let v = cast (floor (l * 255.0 + 0.5)) in MkRGB v v v
     else
       let q = if l < 0.5 then l * (1.0 + s) else l + s - l * s
           p = 2.0 * l - q
           r = hueToRGB p q (h + 1.0 / 3.0)
           g = hueToRGB p q h
           b = hueToRGB p q (h - 1.0 / 3.0)
       in MkRGB (toChannel r) (toChannel g) (toChannel b)
  where
    hueToRGB : Double -> Double -> Double -> Double
    hueToRGB p q t' =
      let t = if t' < 0.0 then t' + 1.0 else if t' > 1.0 then t' - 1.0 else t'
      in if t < 1.0 / 6.0 then p + (q - p) * 6.0 * t
         else if t < 1.0 / 2.0 then q
         else if t < 2.0 / 3.0 then p + (q - p) * (2.0 / 3.0 - t) * 6.0
         else p

    toChannel : Double -> Nat
    toChannel x = cast (floor (x * 255.0 + 0.5))

--------------------------------------------------------------------------------
-- WCAG Contrast
--------------------------------------------------------------------------------

||| Calculate relative luminance (WCAG)
public export
luminance : RGB -> Double
luminance c =
  let r = channelLuminance (cast c.red / 255.0)
      g = channelLuminance (cast c.green / 255.0)
      b = channelLuminance (cast c.blue / 255.0)
  in 0.2126 * r + 0.7152 * g + 0.0722 * b
  where
    channelLuminance : Double -> Double
    channelLuminance x = if x <= 0.03928 then x / 12.92 else pow ((x + 0.055) / 1.055) 2.4

||| Calculate contrast ratio (WCAG)
public export
contrastRatio : RGB -> RGB -> Double
contrastRatio c1 c2 =
  let l1 = luminance c1
      l2 = luminance c2
      lighter = max l1 l2
      darker = min l1 l2
  in (lighter + 0.05) / (darker + 0.05)

||| Check if contrast meets WCAG AA for normal text (4.5:1)
public export
meetsWCAG_AA : RGB -> RGB -> Bool
meetsWCAG_AA c1 c2 = contrastRatio c1 c2 >= 4.5

||| Check if contrast meets WCAG AAA for normal text (7:1)
public export
meetsWCAG_AAA : RGB -> RGB -> Bool
meetsWCAG_AAA c1 c2 = contrastRatio c1 c2 >= 7.0

--------------------------------------------------------------------------------
-- Color Utilities
--------------------------------------------------------------------------------

||| Blend two colors with alpha
public export
blend : RGBA -> RGBA -> RGB
blend fg bg =
  let alpha = cast fg.alpha / 255.0
      invAlpha = 1.0 - alpha
  in MkRGB
       (cast (floor (cast fg.red * alpha + cast bg.red * invAlpha + 0.5)))
       (cast (floor (cast fg.green * alpha + cast bg.green * invAlpha + 0.5)))
       (cast (floor (cast fg.blue * alpha + cast bg.blue * invAlpha + 0.5)))

||| Lighten a color by a percentage
public export
lighten : (percent : Double) -> RGB -> RGB
lighten pct c =
  let hsl = rgbToHSL c
      newL = min 100.0 (hsl.lightness + pct)
  in hslToRGB (MkHSL hsl.hue hsl.saturation newL)

||| Darken a color by a percentage
public export
darken : (percent : Double) -> RGB -> RGB
darken pct c =
  let hsl = rgbToHSL c
      newL = max 0.0 (hsl.lightness - pct)
  in hslToRGB (MkHSL hsl.hue hsl.saturation newL)
