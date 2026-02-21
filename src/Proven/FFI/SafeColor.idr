-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeColor operations
|||
||| This module exports color handling and WCAG contrast to the C ABI
||| via Idris2's RefC backend. All functions are proven total and validate colors.
|||
||| Return conventions:
||| - Parsing → (Int, r, g, b, a) where status 0 = success, 1 = error
||| - WCAG contrast → Double (ratio from 1:1 to 21:1)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: WCAG contrast ratios ensure accessibility.
|||           AA = 4.5:1 for normal text, AAA = 7:1 for normal text
|||           Large text (18pt+ or 14pt+ bold): AA = 3:1, AAA = 4.5:1
|||
||| Color ranges:
||| - RGB: 0-255 per channel
||| - HSL: hue 0-360, saturation 0-100, lightness 0-100
||| - Alpha: 0-255 (0 = fully transparent, 255 = fully opaque)
module Proven.FFI.SafeColor

import Proven.SafeColor
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_color_wcag_aa_normal : Double
proven_idris_color_wcag_aa_normal = 4.5

export
proven_idris_color_wcag_aaa_normal : Double
proven_idris_color_wcag_aaa_normal = 7.0

export
proven_idris_color_wcag_aa_large : Double
proven_idris_color_wcag_aa_large = 3.0

export
proven_idris_color_wcag_aaa_large : Double
proven_idris_color_wcag_aaa_large = 4.5

export
proven_idris_color_rgb_min : Int
proven_idris_color_rgb_min = 0

export
proven_idris_color_rgb_max : Int
proven_idris_color_rgb_max = 255

export
proven_idris_color_hue_min : Double
proven_idris_color_hue_min = 0.0

export
proven_idris_color_hue_max : Double
proven_idris_color_hue_max = 360.0

export
proven_idris_color_percent_min : Double
proven_idris_color_percent_min = 0.0

export
proven_idris_color_percent_max : Double
proven_idris_color_percent_max = 100.0

--------------------------------------------------------------------------------
-- Component Validation
--------------------------------------------------------------------------------

export
proven_idris_color_is_valid_rgb_component : Int -> Int
proven_idris_color_is_valid_rgb_component val =
  encodeBool (val >= 0 && val <= 255)

export
proven_idris_color_is_valid_alpha : Int -> Int
proven_idris_color_is_valid_alpha val =
  encodeBool (val >= 0 && val <= 255)

export
proven_idris_color_is_valid_hue : Double -> Int
proven_idris_color_is_valid_hue val =
  encodeBool (val >= 0.0 && val <= 360.0)

export
proven_idris_color_is_valid_saturation : Double -> Int
proven_idris_color_is_valid_saturation val =
  encodeBool (val >= 0.0 && val <= 100.0)

export
proven_idris_color_is_valid_lightness : Double -> Int
proven_idris_color_is_valid_lightness val =
  encodeBool (val >= 0.0 && val <= 100.0)

export
proven_idris_color_is_valid_rgb : Int -> Int -> Int -> Int
proven_idris_color_is_valid_rgb r g b =
  encodeBool (
    r >= 0 && r <= 255 &&
    g >= 0 && g <= 255 &&
    b >= 0 && b <= 255
  )

export
proven_idris_color_is_valid_rgba : Int -> Int -> Int -> Int -> Int
proven_idris_color_is_valid_rgba r g b a =
  encodeBool (
    r >= 0 && r <= 255 &&
    g >= 0 && g <= 255 &&
    b >= 0 && b <= 255 &&
    a >= 0 && a <= 255
  )

--------------------------------------------------------------------------------
-- Hex Parsing and Formatting
--------------------------------------------------------------------------------

export
proven_idris_color_parse_hex : String -> (Int, Int, Int, Int, Int)
proven_idris_color_parse_hex s =
  case parseHex s of
    Nothing => (1, 0, 0, 0, 0)  -- Error
    Just rgba => (0, cast rgba.red, cast rgba.green, cast rgba.blue, cast rgba.alpha)

export
proven_idris_color_to_hex : Int -> Int -> Int -> Int -> String
proven_idris_color_to_hex r g b a =
  toHex (MkRGBA (cast r) (cast g) (cast b) (cast a))

export
proven_idris_color_is_valid_hex_format : String -> Int
proven_idris_color_is_valid_hex_format s =
  let hex = if isPrefixOf "#" s then drop 1 s else s
      len = length hex
  in encodeBool (len == 3 || len == 6 || len == 8)

export
proven_idris_color_is_short_hex : String -> Int
proven_idris_color_is_short_hex s =
  let hex = if isPrefixOf "#" s then drop 1 s else s
  in encodeBool (length hex == 3)

export
proven_idris_color_is_long_hex : String -> Int
proven_idris_color_is_long_hex s =
  let hex = if isPrefixOf "#" s then drop 1 s else s
  in encodeBool (length hex == 6)

export
proven_idris_color_has_alpha_channel : String -> Int
proven_idris_color_has_alpha_channel s =
  let hex = if isPrefixOf "#" s then drop 1 s else s
  in encodeBool (length hex == 8)

--------------------------------------------------------------------------------
-- Color Space Conversion
--------------------------------------------------------------------------------

export
proven_idris_color_rgb_to_hsl : Int -> Int -> Int -> (Double, Double, Double)
proven_idris_color_rgb_to_hsl r g b =
  let rgb = MkRGB (cast r) (cast g) (cast b)
      hsl = rgbToHSL rgb
  in (hsl.hue, hsl.saturation, hsl.lightness)

export
proven_idris_color_hsl_to_rgb : Double -> Double -> Double -> (Int, Int, Int)
proven_idris_color_hsl_to_rgb h s l =
  let hsl = MkHSL h s l
      rgb = hslToRGB hsl
  in (cast rgb.red, cast rgb.green, cast rgb.blue)

--------------------------------------------------------------------------------
-- WCAG Luminance and Contrast
--------------------------------------------------------------------------------

export
proven_idris_color_luminance : Int -> Int -> Int -> Double
proven_idris_color_luminance r g b =
  luminance (MkRGB (cast r) (cast g) (cast b))

export
proven_idris_color_contrast_ratio : Int -> Int -> Int -> Int -> Int -> Int -> Double
proven_idris_color_contrast_ratio r1 g1 b1 r2 g2 b2 =
  let c1 = MkRGB (cast r1) (cast g1) (cast b1)
      c2 = MkRGB (cast r2) (cast g2) (cast b2)
  in contrastRatio c1 c2

export
proven_idris_color_meets_wcag_aa : Int -> Int -> Int -> Int -> Int -> Int -> Int
proven_idris_color_meets_wcag_aa r1 g1 b1 r2 g2 b2 =
  let c1 = MkRGB (cast r1) (cast g1) (cast b1)
      c2 = MkRGB (cast r2) (cast g2) (cast b2)
  in encodeBool (meetsWCAG_AA c1 c2)

export
proven_idris_color_meets_wcag_aaa : Int -> Int -> Int -> Int -> Int -> Int -> Int
proven_idris_color_meets_wcag_aaa r1 g1 b1 r2 g2 b2 =
  let c1 = MkRGB (cast r1) (cast g1) (cast b1)
      c2 = MkRGB (cast r2) (cast g2) (cast b2)
  in encodeBool (meetsWCAG_AAA c1 c2)

export
proven_idris_color_meets_wcag_aa_large : Int -> Int -> Int -> Int -> Int -> Int -> Int
proven_idris_color_meets_wcag_aa_large r1 g1 b1 r2 g2 b2 =
  let ratio = proven_idris_color_contrast_ratio r1 g1 b1 r2 g2 b2
  in encodeBool (ratio >= 3.0)

export
proven_idris_color_meets_wcag_aaa_large : Int -> Int -> Int -> Int -> Int -> Int -> Int
proven_idris_color_meets_wcag_aaa_large r1 g1 b1 r2 g2 b2 =
  let ratio = proven_idris_color_contrast_ratio r1 g1 b1 r2 g2 b2
  in encodeBool (ratio >= 4.5)

export
proven_idris_color_wcag_level : Double -> String
proven_idris_color_wcag_level ratio =
  if ratio >= 7.0 then "AAA"
  else if ratio >= 4.5 then "AA"
  else if ratio >= 3.0 then "AA-large"
  else "Fail"

--------------------------------------------------------------------------------
-- Color Blending
--------------------------------------------------------------------------------

export
proven_idris_color_blend : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int)
proven_idris_color_blend r1 g1 b1 a1 r2 g2 b2 a2 =
  let fg = MkRGBA (cast r1) (cast g1) (cast b1) (cast a1)
      bg = MkRGBA (cast r2) (cast g2) (cast b2) (cast a2)
      result = blend fg bg
  in (cast result.red, cast result.green, cast result.blue)

export
proven_idris_color_blend_over_white : Int -> Int -> Int -> Int -> (Int, Int, Int)
proven_idris_color_blend_over_white r g b a =
  proven_idris_color_blend r g b a 255 255 255 255

export
proven_idris_color_blend_over_black : Int -> Int -> Int -> Int -> (Int, Int, Int)
proven_idris_color_blend_over_black r g b a =
  proven_idris_color_blend r g b a 0 0 0 255

--------------------------------------------------------------------------------
-- Color Adjustment
--------------------------------------------------------------------------------

export
proven_idris_color_lighten : Double -> Int -> Int -> Int -> (Int, Int, Int)
proven_idris_color_lighten percent r g b =
  let rgb = MkRGB (cast r) (cast g) (cast b)
      result = lighten percent rgb
  in (cast result.red, cast result.green, cast result.blue)

export
proven_idris_color_darken : Double -> Int -> Int -> Int -> (Int, Int, Int)
proven_idris_color_darken percent r g b =
  let rgb = MkRGB (cast r) (cast g) (cast b)
      result = darken percent rgb
  in (cast result.red, cast result.green, cast result.blue)

--------------------------------------------------------------------------------
-- Color Comparison
--------------------------------------------------------------------------------

export
proven_idris_color_rgb_equal : Int -> Int -> Int -> Int -> Int -> Int -> Int
proven_idris_color_rgb_equal r1 g1 b1 r2 g2 b2 =
  encodeBool (r1 == r2 && g1 == g2 && b1 == b2)

export
proven_idris_color_rgba_equal : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
proven_idris_color_rgba_equal r1 g1 b1 a1 r2 g2 b2 a2 =
  encodeBool (r1 == r2 && g1 == g2 && b1 == b2 && a1 == a2)

--------------------------------------------------------------------------------
-- Grayscale and Brightness
--------------------------------------------------------------------------------

export
proven_idris_color_to_grayscale : Int -> Int -> Int -> (Int, Int, Int)
proven_idris_color_to_grayscale r g b =
  let lum = luminance (MkRGB (cast r) (cast g) (cast b))
      gray = cast (floor (lum * 255.0 + 0.5))
  in (gray, gray, gray)

export
proven_idris_color_perceived_brightness : Int -> Int -> Int -> Double
proven_idris_color_perceived_brightness r g b =
  luminance (MkRGB (cast r) (cast g) (cast b))

export
proven_idris_color_is_dark : Int -> Int -> Int -> Int
proven_idris_color_is_dark r g b =
  encodeBool (proven_idris_color_perceived_brightness r g b < 0.5)

export
proven_idris_color_is_light : Int -> Int -> Int -> Int
proven_idris_color_is_light r g b =
  encodeBool (proven_idris_color_perceived_brightness r g b >= 0.5)

--------------------------------------------------------------------------------
-- Named Colors (common web colors)
--------------------------------------------------------------------------------

export
proven_idris_color_white : (Int, Int, Int)
proven_idris_color_white = (255, 255, 255)

export
proven_idris_color_black : (Int, Int, Int)
proven_idris_color_black = (0, 0, 0)

export
proven_idris_color_red : (Int, Int, Int)
proven_idris_color_red = (255, 0, 0)

export
proven_idris_color_green : (Int, Int, Int)
proven_idris_color_green = (0, 255, 0)

export
proven_idris_color_blue : (Int, Int, Int)
proven_idris_color_blue = (0, 0, 255)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_color_friendly_error : String -> String
proven_idris_color_friendly_error errorMsg =
  if isInfixOf "wcag" (toLower errorMsg) || isInfixOf "contrast" (toLower errorMsg)
    then "Color combination does not meet WCAG contrast requirements"
  else if isInfixOf "hex" (toLower errorMsg) || isInfixOf "parse" (toLower errorMsg)
    then "Invalid hex color format (expected #RGB, #RRGGBB, or #RRGGBBAA)"
  else if isInfixOf "rgb" (toLower errorMsg) || isInfixOf "component" (toLower errorMsg)
    then "Invalid RGB component (must be 0-255)"
  else if isInfixOf "hsl" (toLower errorMsg)
    then "Invalid HSL value (hue: 0-360, saturation/lightness: 0-100)"
  else if isInfixOf "alpha" (toLower errorMsg)
    then "Invalid alpha value (must be 0-255)"
  else
    "Color operation error"
