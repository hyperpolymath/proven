# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeColor - Color handling for Nickel
#
# Provides safe color operations with validation for RGB,
# HSL, and hex color formats.

{
  # Result type
  Result = {
    value | Dyn,
    ok | Bool,
  },

  # RGB color representation
  RGB = {
    r | Number,  # 0-255
    g | Number,  # 0-255
    b | Number,  # 0-255
  },

  # RGBA color with alpha
  RGBA = {
    r | Number,  # 0-255
    g | Number,  # 0-255
    b | Number,  # 0-255
    a | Number,  # 0-1
  },

  # HSL color representation
  HSL = {
    h | Number,  # 0-360
    s | Number,  # 0-100
    l | Number,  # 0-100
  },

  # Validate RGB component
  is_valid_rgb_component
    : Number -> Bool
    = fun n =>
      n >= 0 && n <= 255 && std.number.truncate n == n,

  # Validate alpha component
  is_valid_alpha
    : Number -> Bool
    = fun a =>
      a >= 0 && a <= 1,

  # Validate hue (0-360)
  is_valid_hue
    : Number -> Bool
    = fun h =>
      h >= 0 && h <= 360,

  # Validate saturation/lightness (0-100)
  is_valid_sl_component
    : Number -> Bool
    = fun n =>
      n >= 0 && n <= 100,

  # Validate RGB color
  is_valid_rgb
    : RGB -> Bool
    = fun color =>
      is_valid_rgb_component color.r &&
      is_valid_rgb_component color.g &&
      is_valid_rgb_component color.b,

  # Validate RGBA color
  is_valid_rgba
    : RGBA -> Bool
    = fun color =>
      is_valid_rgb_component color.r &&
      is_valid_rgb_component color.g &&
      is_valid_rgb_component color.b &&
      is_valid_alpha color.a,

  # Validate HSL color
  is_valid_hsl
    : HSL -> Bool
    = fun color =>
      is_valid_hue color.h &&
      is_valid_sl_component color.s &&
      is_valid_sl_component color.l,

  # Create RGB safely
  make_rgb
    : Number -> Number -> Number -> { value : RGB, ok : Bool }
    = fun r g b =>
      let color = { r = r, g = g, b = b } in
      if is_valid_rgb color then
        { value = color, ok = true }
      else
        { value = { r = 0, g = 0, b = 0 }, ok = false },

  # Create RGBA safely
  make_rgba
    : Number -> Number -> Number -> Number -> { value : RGBA, ok : Bool }
    = fun r g b a =>
      let color = { r = r, g = g, b = b, a = a } in
      if is_valid_rgba color then
        { value = color, ok = true }
      else
        { value = { r = 0, g = 0, b = 0, a = 1 }, ok = false },

  # Create HSL safely
  make_hsl
    : Number -> Number -> Number -> { value : HSL, ok : Bool }
    = fun h s l =>
      let color = { h = h, s = s, l = l } in
      if is_valid_hsl color then
        { value = color, ok = true }
      else
        { value = { h = 0, s = 0, l = 0 }, ok = false },

  # Convert RGB component to 2-digit hex
  component_to_hex
    : Number -> String
    = fun n =>
      let hex_chars = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"] in
      let hi = std.number.truncate (n / 16) in
      let lo = std.number.truncate (n % 16) in
      (std.array.at hi hex_chars) ++ (std.array.at lo hex_chars),

  # Convert RGB to hex string
  rgb_to_hex
    : RGB -> String
    = fun color =>
      "#" ++ component_to_hex color.r ++ component_to_hex color.g ++ component_to_hex color.b,

  # Check if string is valid hex color
  is_valid_hex
    : String -> Bool
    = fun hex =>
      let trimmed = std.string.trim hex in
      let len = std.string.length trimmed in
      if len != 4 && len != 7 then
        false
      else if std.string.substring 0 1 trimmed != "#" then
        false
      else
        let hex_part = std.string.substring 1 len trimmed in
        let chars = std.string.characters (std.string.lowercase hex_part) in
        std.array.all
          (fun c => (c >= "0" && c <= "9") || (c >= "a" && c <= "f"))
          chars,

  # Parse hex character to number
  hex_char_to_num
    : String -> Number
    = fun c =>
      let lower = std.string.lowercase c in
      if lower == "0" then 0 else if lower == "1" then 1
      else if lower == "2" then 2 else if lower == "3" then 3
      else if lower == "4" then 4 else if lower == "5" then 5
      else if lower == "6" then 6 else if lower == "7" then 7
      else if lower == "8" then 8 else if lower == "9" then 9
      else if lower == "a" then 10 else if lower == "b" then 11
      else if lower == "c" then 12 else if lower == "d" then 13
      else if lower == "e" then 14 else if lower == "f" then 15
      else 0,

  # Parse hex to RGB
  hex_to_rgb
    : String -> { value : RGB, ok : Bool }
    = fun hex =>
      if !(is_valid_hex hex) then
        { value = { r = 0, g = 0, b = 0 }, ok = false }
      else
        let hex_part = std.string.substring 1 (std.string.length hex) hex in
        let len = std.string.length hex_part in
        if len == 3 then
          # Short form #RGB -> #RRGGBB
          let chars = std.string.characters hex_part in
          let r = hex_char_to_num (std.array.at 0 chars) in
          let g = hex_char_to_num (std.array.at 1 chars) in
          let b = hex_char_to_num (std.array.at 2 chars) in
          { value = { r = r * 16 + r, g = g * 16 + g, b = b * 16 + b }, ok = true }
        else
          # Full form #RRGGBB
          let chars = std.string.characters hex_part in
          let r = hex_char_to_num (std.array.at 0 chars) * 16 + hex_char_to_num (std.array.at 1 chars) in
          let g = hex_char_to_num (std.array.at 2 chars) * 16 + hex_char_to_num (std.array.at 3 chars) in
          let b = hex_char_to_num (std.array.at 4 chars) * 16 + hex_char_to_num (std.array.at 5 chars) in
          { value = { r = r, g = g, b = b }, ok = true },

  # Blend two colors
  blend_rgb
    : RGB -> RGB -> Number -> RGB
    = fun a b t =>
      let clamped_t = if t < 0 then 0 else if t > 1 then 1 else t in
      {
        r = std.number.truncate (a.r + (b.r - a.r) * clamped_t),
        g = std.number.truncate (a.g + (b.g - a.g) * clamped_t),
        b = std.number.truncate (a.b + (b.b - a.b) * clamped_t),
      },

  # Lighten color
  lighten
    : RGB -> Number -> RGB
    = fun color amount =>
      {
        r = std.number.truncate (if color.r + amount > 255 then 255 else color.r + amount),
        g = std.number.truncate (if color.g + amount > 255 then 255 else color.g + amount),
        b = std.number.truncate (if color.b + amount > 255 then 255 else color.b + amount),
      },

  # Darken color
  darken
    : RGB -> Number -> RGB
    = fun color amount =>
      {
        r = std.number.truncate (if color.r - amount < 0 then 0 else color.r - amount),
        g = std.number.truncate (if color.g - amount < 0 then 0 else color.g - amount),
        b = std.number.truncate (if color.b - amount < 0 then 0 else color.b - amount),
      },

  # Invert color
  invert
    : RGB -> RGB
    = fun color =>
      { r = 255 - color.r, g = 255 - color.g, b = 255 - color.b },

  # Common colors
  BLACK = { r = 0, g = 0, b = 0 },
  WHITE = { r = 255, g = 255, b = 255 },
  RED = { r = 255, g = 0, b = 0 },
  GREEN = { r = 0, g = 255, b = 0 },
  BLUE = { r = 0, g = 0, b = 255 },
  YELLOW = { r = 255, g = 255, b = 0 },
  CYAN = { r = 0, g = 255, b = 255 },
  MAGENTA = { r = 255, g = 0, b = 255 },

  # Contracts
  ValidRGB = std.contract.from_predicate is_valid_rgb,
  ValidRGBA = std.contract.from_predicate is_valid_rgba,
  ValidHSL = std.contract.from_predicate is_valid_hsl,
  HexColor = std.contract.from_predicate is_valid_hex,
  RGBComponent = std.contract.from_predicate is_valid_rgb_component,
  Alpha = std.contract.from_predicate is_valid_alpha,
}
