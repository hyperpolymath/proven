// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeColor - Color operations that cannot crash.
////
//// Provides safe color parsing, conversion, and manipulation.

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// An RGB color with 8-bit components.
pub opaque type Rgb {
  Rgb(red: Int, green: Int, blue: Int)
}

/// An RGBA color with alpha channel.
pub opaque type Rgba {
  Rgba(red: Int, green: Int, blue: Int, alpha: Float)
}

/// An HSL color.
pub opaque type Hsl {
  Hsl(hue: Float, saturation: Float, lightness: Float)
}

/// An HSV color.
pub opaque type Hsv {
  Hsv(hue: Float, saturation: Float, value: Float)
}

/// Error types for color operations.
pub type ColorError {
  InvalidHexLength(actual: Int)
  InvalidHexCharacter(position: Int)
  InvalidFormat(message: String)
  ComponentOutOfRange(component: String, value: Int)
}

/// Create an RGB color from components.
pub fn rgb(red: Int, green: Int, blue: Int) -> Rgb {
  Rgb(
    red: int.clamp(red, 0, 255),
    green: int.clamp(green, 0, 255),
    blue: int.clamp(blue, 0, 255),
  )
}

/// Create an RGBA color from components.
pub fn rgba(red: Int, green: Int, blue: Int, alpha: Float) -> Rgba {
  Rgba(
    red: int.clamp(red, 0, 255),
    green: int.clamp(green, 0, 255),
    blue: int.clamp(blue, 0, 255),
    alpha: clamp_float(alpha, 0.0, 1.0),
  )
}

fn clamp_float(value: Float, min: Float, max: Float) -> Float {
  case value <. min {
    True -> min
    False ->
      case value >. max {
        True -> max
        False -> value
      }
  }
}

/// Create an HSL color from components.
pub fn hsl(hue: Float, saturation: Float, lightness: Float) -> Hsl {
  Hsl(
    hue: modulo_float(hue, 360.0),
    saturation: clamp_float(saturation, 0.0, 100.0),
    lightness: clamp_float(lightness, 0.0, 100.0),
  )
}

fn modulo_float(value: Float, divisor: Float) -> Float {
  let quotient = float.truncate(value /. divisor)
  value -. int.to_float(quotient) *. divisor
}

/// Create an HSV color from components.
pub fn hsv(hue: Float, saturation: Float, value: Float) -> Hsv {
  Hsv(
    hue: modulo_float(hue, 360.0),
    saturation: clamp_float(saturation, 0.0, 100.0),
    value: clamp_float(value, 0.0, 100.0),
  )
}

/// Get red component.
pub fn get_red(color: Rgb) -> Int {
  color.red
}

/// Get green component.
pub fn get_green(color: Rgb) -> Int {
  color.green
}

/// Get blue component.
pub fn get_blue(color: Rgb) -> Int {
  color.blue
}

/// Parse a hex color string (with or without #).
pub fn parse_hex(input: String) -> Result(Rgb, ColorError) {
  let trimmed = string.trim(input)
  let without_hash = case string.starts_with(trimmed, "#") {
    True -> string.drop_start(trimmed, 1)
    False -> trimmed
  }

  let length = string.length(without_hash)
  case length {
    3 -> parse_short_hex(without_hash)
    6 -> parse_full_hex(without_hash)
    _ -> Error(InvalidHexLength(actual: length))
  }
}

fn parse_short_hex(input: String) -> Result(Rgb, ColorError) {
  let chars = string.to_graphemes(input)
  case chars {
    [r, g, b] ->
      case hex_char_to_int(r), hex_char_to_int(g), hex_char_to_int(b) {
        Ok(red), Ok(green), Ok(blue) ->
          Ok(rgb(red * 17, green * 17, blue * 17))
        Error(_), _, _ -> Error(InvalidHexCharacter(position: 0))
        _, Error(_), _ -> Error(InvalidHexCharacter(position: 1))
        _, _, Error(_) -> Error(InvalidHexCharacter(position: 2))
      }
    _ -> Error(InvalidHexLength(actual: list.length(chars)))
  }
}

fn parse_full_hex(input: String) -> Result(Rgb, ColorError) {
  let chars = string.to_graphemes(input)
  case chars {
    [r1, r2, g1, g2, b1, b2] ->
      case parse_hex_byte(r1, r2), parse_hex_byte(g1, g2), parse_hex_byte(b1, b2) {
        Ok(red), Ok(green), Ok(blue) -> Ok(rgb(red, green, blue))
        Error(pos), _, _ -> Error(InvalidHexCharacter(position: pos))
        _, Error(pos), _ -> Error(InvalidHexCharacter(position: 2 + pos))
        _, _, Error(pos) -> Error(InvalidHexCharacter(position: 4 + pos))
      }
    _ -> Error(InvalidHexLength(actual: list.length(chars)))
  }
}

fn parse_hex_byte(high: String, low: String) -> Result(Int, Int) {
  case hex_char_to_int(high), hex_char_to_int(low) {
    Ok(h), Ok(l) -> Ok(h * 16 + l)
    Error(_), _ -> Error(0)
    _, Error(_) -> Error(1)
  }
}

fn hex_char_to_int(char: String) -> Result(Int, Nil) {
  case char {
    "0" -> Ok(0)
    "1" -> Ok(1)
    "2" -> Ok(2)
    "3" -> Ok(3)
    "4" -> Ok(4)
    "5" -> Ok(5)
    "6" -> Ok(6)
    "7" -> Ok(7)
    "8" -> Ok(8)
    "9" -> Ok(9)
    "a" | "A" -> Ok(10)
    "b" | "B" -> Ok(11)
    "c" | "C" -> Ok(12)
    "d" | "D" -> Ok(13)
    "e" | "E" -> Ok(14)
    "f" | "F" -> Ok(15)
    _ -> Error(Nil)
  }
}

/// Format RGB as hex string (with #).
pub fn to_hex(color: Rgb) -> String {
  "#" <> int_to_hex_byte(color.red) <> int_to_hex_byte(color.green) <> int_to_hex_byte(color.blue)
}

fn int_to_hex_byte(value: Int) -> String {
  let high = value / 16
  let low = value % 16
  int_to_hex_char(high) <> int_to_hex_char(low)
}

fn int_to_hex_char(value: Int) -> String {
  case value {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    _ -> "f"
  }
}

/// Format RGB as CSS rgb() string.
pub fn to_css_rgb(color: Rgb) -> String {
  "rgb("
  <> int.to_string(color.red)
  <> ", "
  <> int.to_string(color.green)
  <> ", "
  <> int.to_string(color.blue)
  <> ")"
}

/// Format RGBA as CSS rgba() string.
pub fn to_css_rgba(color: Rgba) -> String {
  "rgba("
  <> int.to_string(color.red)
  <> ", "
  <> int.to_string(color.green)
  <> ", "
  <> int.to_string(color.blue)
  <> ", "
  <> float.to_string(color.alpha)
  <> ")"
}

/// Convert RGB to HSL.
pub fn rgb_to_hsl(color: Rgb) -> Hsl {
  let r = int.to_float(color.red) /. 255.0
  let g = int.to_float(color.green) /. 255.0
  let b = int.to_float(color.blue) /. 255.0

  let max_val = float.max(r, float.max(g, b))
  let min_val = float.min(r, float.min(g, b))
  let l = { max_val +. min_val } /. 2.0

  case max_val == min_val {
    True -> Hsl(hue: 0.0, saturation: 0.0, lightness: l *. 100.0)
    False -> {
      let d = max_val -. min_val
      let s = case l >. 0.5 {
        True -> d /. { 2.0 -. max_val -. min_val }
        False -> d /. { max_val +. min_val }
      }

      let h = case max_val == r {
        True -> { g -. b } /. d +. case g <. b {
          True -> 6.0
          False -> 0.0
        }
        False ->
          case max_val == g {
            True -> { b -. r } /. d +. 2.0
            False -> { r -. g } /. d +. 4.0
          }
      }

      Hsl(hue: h *. 60.0, saturation: s *. 100.0, lightness: l *. 100.0)
    }
  }
}

/// Convert HSL to RGB.
pub fn hsl_to_rgb(color: Hsl) -> Rgb {
  let h = color.hue /. 360.0
  let s = color.saturation /. 100.0
  let l = color.lightness /. 100.0

  case s == 0.0 {
    True -> {
      let value = float.round(l *. 255.0)
      rgb(value, value, value)
    }
    False -> {
      let q = case l <. 0.5 {
        True -> l *. { 1.0 +. s }
        False -> l +. s -. l *. s
      }
      let p = 2.0 *. l -. q

      let r = hue_to_rgb(p, q, h +. 1.0 /. 3.0)
      let g = hue_to_rgb(p, q, h)
      let b_val = hue_to_rgb(p, q, h -. 1.0 /. 3.0)

      rgb(
        float.round(r *. 255.0),
        float.round(g *. 255.0),
        float.round(b_val *. 255.0),
      )
    }
  }
}

fn hue_to_rgb(p: Float, q: Float, t: Float) -> Float {
  let adjusted_t = case t <. 0.0 {
    True -> t +. 1.0
    False ->
      case t >. 1.0 {
        True -> t -. 1.0
        False -> t
      }
  }

  case adjusted_t <. 1.0 /. 6.0 {
    True -> p +. { q -. p } *. 6.0 *. adjusted_t
    False ->
      case adjusted_t <. 1.0 /. 2.0 {
        True -> q
        False ->
          case adjusted_t <. 2.0 /. 3.0 {
            True -> p +. { q -. p } *. { 2.0 /. 3.0 -. adjusted_t } *. 6.0
            False -> p
          }
      }
  }
}

/// Lighten a color by a percentage.
pub fn lighten(color: Rgb, percent: Float) -> Rgb {
  let hsl_color = rgb_to_hsl(color)
  let new_lightness = clamp_float(hsl_color.lightness +. percent, 0.0, 100.0)
  hsl_to_rgb(Hsl(..hsl_color, lightness: new_lightness))
}

/// Darken a color by a percentage.
pub fn darken(color: Rgb, percent: Float) -> Rgb {
  lighten(color, 0.0 -. percent)
}

/// Saturate a color by a percentage.
pub fn saturate(color: Rgb, percent: Float) -> Rgb {
  let hsl_color = rgb_to_hsl(color)
  let new_saturation = clamp_float(hsl_color.saturation +. percent, 0.0, 100.0)
  hsl_to_rgb(Hsl(..hsl_color, saturation: new_saturation))
}

/// Desaturate a color by a percentage.
pub fn desaturate(color: Rgb, percent: Float) -> Rgb {
  saturate(color, 0.0 -. percent)
}

/// Convert to grayscale.
pub fn grayscale(color: Rgb) -> Rgb {
  desaturate(color, 100.0)
}

/// Invert a color.
pub fn invert(color: Rgb) -> Rgb {
  rgb(255 - color.red, 255 - color.green, 255 - color.blue)
}

/// Mix two colors together.
pub fn mix(color_a: Rgb, color_b: Rgb, weight: Float) -> Rgb {
  let w = clamp_float(weight, 0.0, 1.0)
  let inv_w = 1.0 -. w

  rgb(
    float.round(int.to_float(color_a.red) *. w +. int.to_float(color_b.red) *. inv_w),
    float.round(int.to_float(color_a.green) *. w +. int.to_float(color_b.green) *. inv_w),
    float.round(int.to_float(color_a.blue) *. w +. int.to_float(color_b.blue) *. inv_w),
  )
}

/// Get the complementary color.
pub fn complement(color: Rgb) -> Rgb {
  let hsl_color = rgb_to_hsl(color)
  let new_hue = modulo_float(hsl_color.hue +. 180.0, 360.0)
  hsl_to_rgb(Hsl(..hsl_color, hue: new_hue))
}

/// Calculate relative luminance (WCAG formula).
pub fn luminance(color: Rgb) -> Float {
  let r = linearize(int.to_float(color.red) /. 255.0)
  let g = linearize(int.to_float(color.green) /. 255.0)
  let b = linearize(int.to_float(color.blue) /. 255.0)
  0.2126 *. r +. 0.7152 *. g +. 0.0722 *. b
}

fn linearize(value: Float) -> Float {
  case value <=. 0.03928 {
    True -> value /. 12.92
    False -> float.power(
      { value +. 0.055 } /. 1.055,
      2.4,
    ) |> result_or_float(0.0)
  }
}

fn result_or_float(result: Result(Float, Nil), default: Float) -> Float {
  case result {
    Ok(v) -> v
    Error(_) -> default
  }
}

/// Calculate contrast ratio between two colors (WCAG).
pub fn contrast_ratio(color_a: Rgb, color_b: Rgb) -> Float {
  let lum_a = luminance(color_a)
  let lum_b = luminance(color_b)
  let lighter = float.max(lum_a, lum_b)
  let darker = float.min(lum_a, lum_b)
  { lighter +. 0.05 } /. { darker +. 0.05 }
}

/// Check if two colors have sufficient contrast for WCAG AA.
pub fn meets_wcag_aa(color_a: Rgb, color_b: Rgb) -> Bool {
  contrast_ratio(color_a, color_b) >=. 4.5
}

/// Check if two colors have sufficient contrast for WCAG AAA.
pub fn meets_wcag_aaa(color_a: Rgb, color_b: Rgb) -> Bool {
  contrast_ratio(color_a, color_b) >=. 7.0
}

/// Common color constants.
pub fn black() -> Rgb {
  rgb(0, 0, 0)
}

pub fn white() -> Rgb {
  rgb(255, 255, 255)
}

pub fn red() -> Rgb {
  rgb(255, 0, 0)
}

pub fn green() -> Rgb {
  rgb(0, 128, 0)
}

pub fn blue() -> Rgb {
  rgb(0, 0, 255)
}

pub fn yellow() -> Rgb {
  rgb(255, 255, 0)
}

pub fn cyan() -> Rgb {
  rgb(0, 255, 255)
}

pub fn magenta() -> Rgb {
  rgb(255, 0, 255)
}
