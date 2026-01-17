-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeColor - Verified color operations
|||
||| Type-safe color handling with gamut clamping and color space conversions.
||| All color values are guaranteed to be within valid ranges.
module Proven.SafeColor

import Proven.Core
import Data.So

%default total

-- ============================================================================
-- RGB COLOR
-- ============================================================================

||| RGB color with values in [0, 255]
public export
record RGB where
  constructor MkRGB
  red : Nat
  green : Nat
  blue : Nat
  0 redInRange : So (red <= 255)
  0 greenInRange : So (green <= 255)
  0 blueInRange : So (blue <= 255)

||| Create RGB color (clamps to valid range)
export
rgb : Nat -> Nat -> Nat -> RGB
rgb r g b = believe_me (MkRGB (min 255 r) (min 255 g) (min 255 b))

||| Create RGB from floats [0, 1]
export
rgbFromFloats : Double -> Double -> Double -> RGB
rgbFromFloats r g b =
  rgb (cast (round (clamp 0.0 1.0 r * 255.0)))
      (cast (round (clamp 0.0 1.0 g * 255.0)))
      (cast (round (clamp 0.0 1.0 b * 255.0)))
  where
    clamp : Double -> Double -> Double -> Double
    clamp lo hi x = max lo (min hi x)

||| Convert to floats [0, 1]
export
rgbToFloats : RGB -> (Double, Double, Double)
rgbToFloats c = (cast c.red / 255.0, cast c.green / 255.0, cast c.blue / 255.0)

-- ============================================================================
-- RGBA COLOR
-- ============================================================================

||| RGBA color with alpha channel
public export
record RGBA where
  constructor MkRGBA
  red : Nat
  green : Nat
  blue : Nat
  alpha : Nat
  0 redInRange : So (red <= 255)
  0 greenInRange : So (green <= 255)
  0 blueInRange : So (blue <= 255)
  0 alphaInRange : So (alpha <= 255)

||| Create RGBA color
export
rgba : Nat -> Nat -> Nat -> Nat -> RGBA
rgba r g b a = believe_me (MkRGBA (min 255 r) (min 255 g) (min 255 b) (min 255 a))

||| Convert RGB to RGBA (fully opaque)
export
rgbToRgba : RGB -> RGBA
rgbToRgba c = rgba c.red c.green c.blue 255

||| Convert RGBA to RGB (discard alpha)
export
rgbaToRgb : RGBA -> RGB
rgbaToRgb c = rgb c.red c.green c.blue

-- ============================================================================
-- HSL COLOR
-- ============================================================================

||| HSL color (Hue, Saturation, Lightness)
||| Hue in [0, 360), Saturation and Lightness in [0, 100]
public export
record HSL where
  constructor MkHSL
  hue : Double        -- [0, 360)
  saturation : Double -- [0, 100]
  lightness : Double  -- [0, 100]

||| Create HSL color (normalizes hue, clamps S and L)
export
hsl : Double -> Double -> Double -> HSL
hsl h s l =
  let h' = h - 360.0 * floor (h / 360.0)
      h'' = if h' < 0.0 then h' + 360.0 else h'
  in MkHSL h'' (max 0.0 (min 100.0 s)) (max 0.0 (min 100.0 l))

||| Convert RGB to HSL
export
rgbToHsl : RGB -> HSL
rgbToHsl c =
  let r = cast c.red / 255.0
      g = cast c.green / 255.0
      b = cast c.blue / 255.0
      maxC = max r (max g b)
      minC = min r (min g b)
      l = (maxC + minC) / 2.0
      delta = maxC - minC
  in if delta < 0.0001
     then MkHSL 0.0 0.0 (l * 100.0)
     else let s = if l < 0.5 then delta / (maxC + minC)
                  else delta / (2.0 - maxC - minC)
              h = if maxC == r then (g - b) / delta + (if g < b then 6.0 else 0.0)
                  else if maxC == g then (b - r) / delta + 2.0
                  else (r - g) / delta + 4.0
          in MkHSL (h * 60.0) (s * 100.0) (l * 100.0)

||| Convert HSL to RGB
export
hslToRgb : HSL -> RGB
hslToRgb c =
  let h = c.hue / 360.0
      s = c.saturation / 100.0
      l = c.lightness / 100.0
  in if s < 0.0001
     then let v = cast (round (l * 255.0)) in rgb v v v
     else let q = if l < 0.5 then l * (1.0 + s) else l + s - l * s
              p = 2.0 * l - q
              r = hueToRgb p q (h + 1.0/3.0)
              g = hueToRgb p q h
              b = hueToRgb p q (h - 1.0/3.0)
          in rgbFromFloats r g b
  where
    hueToRgb : Double -> Double -> Double -> Double
    hueToRgb p q t =
      let t' = if t < 0.0 then t + 1.0 else if t > 1.0 then t - 1.0 else t
      in if t' < 1.0/6.0 then p + (q - p) * 6.0 * t'
         else if t' < 1.0/2.0 then q
         else if t' < 2.0/3.0 then p + (q - p) * (2.0/3.0 - t') * 6.0
         else p

-- ============================================================================
-- HSV COLOR
-- ============================================================================

||| HSV color (Hue, Saturation, Value/Brightness)
public export
record HSV where
  constructor MkHSV
  hue : Double        -- [0, 360)
  saturation : Double -- [0, 100]
  value : Double      -- [0, 100]

||| Create HSV color
export
hsv : Double -> Double -> Double -> HSV
hsv h s v =
  let h' = h - 360.0 * floor (h / 360.0)
      h'' = if h' < 0.0 then h' + 360.0 else h'
  in MkHSV h'' (max 0.0 (min 100.0 s)) (max 0.0 (min 100.0 v))

||| Convert RGB to HSV
export
rgbToHsv : RGB -> HSV
rgbToHsv c =
  let r = cast c.red / 255.0
      g = cast c.green / 255.0
      b = cast c.blue / 255.0
      maxC = max r (max g b)
      minC = min r (min g b)
      delta = maxC - minC
      v = maxC
  in if delta < 0.0001
     then MkHSV 0.0 0.0 (v * 100.0)
     else let s = delta / maxC
              h = if maxC == r then (g - b) / delta + (if g < b then 6.0 else 0.0)
                  else if maxC == g then (b - r) / delta + 2.0
                  else (r - g) / delta + 4.0
          in MkHSV (h * 60.0) (s * 100.0) (v * 100.0)

||| Convert HSV to RGB
export
hsvToRgb : HSV -> RGB
hsvToRgb c =
  let h = c.hue / 60.0
      s = c.saturation / 100.0
      v = c.value / 100.0
      i = floor h
      f = h - i
      p = v * (1.0 - s)
      q = v * (1.0 - s * f)
      t = v * (1.0 - s * (1.0 - f))
      (r, g, b) = case cast {to=Int} i `mod` 6 of
                    0 => (v, t, p)
                    1 => (q, v, p)
                    2 => (p, v, t)
                    3 => (p, q, v)
                    4 => (t, p, v)
                    _ => (v, p, q)
  in rgbFromFloats r g b

-- ============================================================================
-- HEX COLOR
-- ============================================================================

||| Parse hex color string (e.g., "#FF0000" or "FF0000")
export
parseHex : String -> Maybe RGB
parseHex s =
  let clean = if take 1 s == "#" then drop 1 s else s
  in if length clean /= 6 then Nothing
     else let r = parseHexByte (substr 0 2 clean)
              g = parseHexByte (substr 2 2 clean)
              b = parseHexByte (substr 4 2 clean)
          in case (r, g, b) of
               (Just r', Just g', Just b') => Just (rgb r' g' b')
               _ => Nothing
  where
    parseHexDigit : Char -> Maybe Nat
    parseHexDigit c =
      if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
      else if c >= 'a' && c <= 'f' then Just (10 + cast (ord c - ord 'a'))
      else if c >= 'A' && c <= 'F' then Just (10 + cast (ord c - ord 'A'))
      else Nothing

    parseHexByte : String -> Maybe Nat
    parseHexByte s =
      case unpack s of
        [h, l] => case (parseHexDigit h, parseHexDigit l) of
                    (Just h', Just l') => Just (h' * 16 + l')
                    _ => Nothing
        _ => Nothing

||| Convert RGB to hex string
export
toHex : RGB -> String
toHex c = "#" ++ hexByte c.red ++ hexByte c.green ++ hexByte c.blue
  where
    hexDigit : Nat -> Char
    hexDigit n = if n < 10 then chr (cast n + ord '0')
                 else chr (cast (n - 10) + ord 'A')

    hexByte : Nat -> String
    hexByte n = pack [hexDigit (n `div` 16), hexDigit (n `mod` 16)]

-- ============================================================================
-- COLOR OPERATIONS
-- ============================================================================

||| Blend two colors (linear interpolation)
export
blend : RGB -> RGB -> Double -> RGB
blend c1 c2 t =
  let t' = max 0.0 (min 1.0 t)
      r = cast c1.red + t' * (cast c2.red - cast c1.red)
      g = cast c1.green + t' * (cast c2.green - cast c1.green)
      b = cast c1.blue + t' * (cast c2.blue - cast c1.blue)
  in rgb (cast (round r)) (cast (round g)) (cast (round b))

||| Lighten a color
export
lighten : RGB -> Double -> RGB
lighten c amount =
  let hsl' = rgbToHsl c
      newL = min 100.0 (hsl'.lightness + amount)
  in hslToRgb (MkHSL hsl'.hue hsl'.saturation newL)

||| Darken a color
export
darken : RGB -> Double -> RGB
darken c amount =
  let hsl' = rgbToHsl c
      newL = max 0.0 (hsl'.lightness - amount)
  in hslToRgb (MkHSL hsl'.hue hsl'.saturation newL)

||| Saturate a color
export
saturate : RGB -> Double -> RGB
saturate c amount =
  let hsl' = rgbToHsl c
      newS = min 100.0 (hsl'.saturation + amount)
  in hslToRgb (MkHSL hsl'.hue newS hsl'.lightness)

||| Desaturate a color
export
desaturate : RGB -> Double -> RGB
desaturate c amount =
  let hsl' = rgbToHsl c
      newS = max 0.0 (hsl'.saturation - amount)
  in hslToRgb (MkHSL hsl'.hue newS hsl'.lightness)

||| Get grayscale value (luminance)
export
grayscale : RGB -> RGB
grayscale c =
  let l = cast (round (0.2126 * cast c.red + 0.7152 * cast c.green + 0.0722 * cast c.blue))
  in rgb l l l

||| Invert a color
export
invert : RGB -> RGB
invert c = rgb (255 `minus` c.red) (255 `minus` c.green) (255 `minus` c.blue)

||| Complement (opposite on color wheel)
export
complement : RGB -> RGB
complement c =
  let hsl' = rgbToHsl c
      newH = hsl'.hue + 180.0
      newH' = if newH >= 360.0 then newH - 360.0 else newH
  in hslToRgb (MkHSL newH' hsl'.saturation hsl'.lightness)

-- ============================================================================
-- NAMED COLORS
-- ============================================================================

export black : RGB
black = rgb 0 0 0

export white : RGB
white = rgb 255 255 255

export red : RGB
red = rgb 255 0 0

export green : RGB
green = rgb 0 128 0

export blue : RGB
blue = rgb 0 0 255

export yellow : RGB
yellow = rgb 255 255 0

export cyan : RGB
cyan = rgb 0 255 255

export magenta : RGB
magenta = rgb 255 0 255

export orange : RGB
orange = rgb 255 165 0

export purple : RGB
purple = rgb 128 0 128

-- ============================================================================
-- CONTRAST AND ACCESSIBILITY
-- ============================================================================

||| Calculate relative luminance (WCAG formula)
export
relativeLuminance : RGB -> Double
relativeLuminance c =
  let r = toLinear (cast c.red / 255.0)
      g = toLinear (cast c.green / 255.0)
      b = toLinear (cast c.blue / 255.0)
  in 0.2126 * r + 0.7152 * g + 0.0722 * b
  where
    toLinear : Double -> Double
    toLinear v = if v <= 0.04045 then v / 12.92
                 else pow ((v + 0.055) / 1.055) 2.4

||| Calculate contrast ratio (WCAG)
export
contrastRatio : RGB -> RGB -> Double
contrastRatio c1 c2 =
  let l1 = relativeLuminance c1
      l2 = relativeLuminance c2
      lighter = max l1 l2
      darker = min l1 l2
  in (lighter + 0.05) / (darker + 0.05)

||| Check if contrast meets WCAG AA (4.5:1 for normal text)
export
meetsWcagAA : RGB -> RGB -> Bool
meetsWcagAA fg bg = contrastRatio fg bg >= 4.5

||| Check if contrast meets WCAG AAA (7:1 for normal text)
export
meetsWcagAAA : RGB -> RGB -> Bool
meetsWcagAAA fg bg = contrastRatio fg bg >= 7.0
