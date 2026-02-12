# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # RGBA color representation.
  record RgbaColor, r : UInt8, g : UInt8, b : UInt8, a : UInt8 = 255_u8 do
    def to_hex : String
      if a == 255
        "#%02x%02x%02x" % {r, g, b}
      else
        "#%02x%02x%02x%02x" % {r, g, b, a}
      end
    end

    def to_rgb_string : String
      "rgb(#{r}, #{g}, #{b})"
    end

    def to_rgba_string : String
      "rgba(#{r}, #{g}, #{b}, #{(a / 255.0).round(2)})"
    end
  end

  # HSL color representation.
  record HslColor, h : Float64, s : Float64, l : Float64, a : Float64 = 1.0 do
    def to_string : String
      if a == 1.0
        "hsl(#{h.round.to_i}, #{(s * 100).round.to_i}%, #{(l * 100).round.to_i}%)"
      else
        "hsla(#{h.round.to_i}, #{(s * 100).round.to_i}%, #{(l * 100).round.to_i}%, #{a.round(2)})"
      end
    end
  end

  # Safe color operations.
  module SafeColor
    # Parse hex color string.
    def self.parse_hex(input : String) : RgbaColor?
      hex = input.strip.lstrip('#')

      case hex.size
      when 3
        # Short form: #RGB
        r = hex[0..0].to_u8?(16)
        g = hex[1..1].to_u8?(16)
        b = hex[2..2].to_u8?(16)
        return nil if r.nil? || g.nil? || b.nil?
        RgbaColor.new((r * 17).to_u8, (g * 17).to_u8, (b * 17).to_u8)
      when 4
        # Short form with alpha: #RGBA
        r = hex[0..0].to_u8?(16)
        g = hex[1..1].to_u8?(16)
        b = hex[2..2].to_u8?(16)
        a = hex[3..3].to_u8?(16)
        return nil if r.nil? || g.nil? || b.nil? || a.nil?
        RgbaColor.new((r * 17).to_u8, (g * 17).to_u8, (b * 17).to_u8, (a * 17).to_u8)
      when 6
        # Long form: #RRGGBB
        r = hex[0..1].to_u8?(16)
        g = hex[2..3].to_u8?(16)
        b = hex[4..5].to_u8?(16)
        return nil if r.nil? || g.nil? || b.nil?
        RgbaColor.new(r, g, b)
      when 8
        # Long form with alpha: #RRGGBBAA
        r = hex[0..1].to_u8?(16)
        g = hex[2..3].to_u8?(16)
        b = hex[4..5].to_u8?(16)
        a = hex[6..7].to_u8?(16)
        return nil if r.nil? || g.nil? || b.nil? || a.nil?
        RgbaColor.new(r, g, b, a)
      else
        nil
      end
    end

    # Parse RGB/RGBA string.
    def self.parse_rgb(input : String) : RgbaColor?
      lower = input.strip.downcase

      if match = lower.match(/^rgba?\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)(?:\s*,\s*([\d.]+))?\s*\)$/)
        r = match[1].to_u8?
        g = match[2].to_u8?
        b = match[3].to_u8?
        return nil if r.nil? || g.nil? || b.nil?

        a = if alpha_str = match[4]?
              alpha = alpha_str.to_f64?
              return nil if alpha.nil?
              (alpha.clamp(0.0, 1.0) * 255).round.to_u8
            else
              255_u8
            end

        RgbaColor.new(r, g, b, a)
      else
        nil
      end
    end

    # Parse any color format.
    def self.parse(input : String) : RgbaColor?
      trimmed = input.strip

      if trimmed.starts_with?('#')
        parse_hex(trimmed)
      elsif trimmed.downcase.starts_with?("rgb")
        parse_rgb(trimmed)
      else
        # Try named colors
        named_color(trimmed)
      end
    end

    # Get named color.
    def self.named_color(name : String) : RgbaColor?
      case name.downcase
      when "black"   then RgbaColor.new(0, 0, 0)
      when "white"   then RgbaColor.new(255, 255, 255)
      when "red"     then RgbaColor.new(255, 0, 0)
      when "green"   then RgbaColor.new(0, 128, 0)
      when "blue"    then RgbaColor.new(0, 0, 255)
      when "yellow"  then RgbaColor.new(255, 255, 0)
      when "cyan"    then RgbaColor.new(0, 255, 255)
      when "magenta" then RgbaColor.new(255, 0, 255)
      when "orange"  then RgbaColor.new(255, 165, 0)
      when "purple"  then RgbaColor.new(128, 0, 128)
      when "pink"    then RgbaColor.new(255, 192, 203)
      when "gray", "grey" then RgbaColor.new(128, 128, 128)
      when "silver"  then RgbaColor.new(192, 192, 192)
      when "navy"    then RgbaColor.new(0, 0, 128)
      when "teal"    then RgbaColor.new(0, 128, 128)
      when "olive"   then RgbaColor.new(128, 128, 0)
      when "maroon"  then RgbaColor.new(128, 0, 0)
      when "lime"    then RgbaColor.new(0, 255, 0)
      when "aqua"    then RgbaColor.new(0, 255, 255)
      when "fuchsia" then RgbaColor.new(255, 0, 255)
      else                nil
      end
    end

    # Convert RGB to HSL.
    def self.rgb_to_hsl(color : RgbaColor) : HslColor
      r = color.r / 255.0
      g = color.g / 255.0
      b = color.b / 255.0

      max = {r, g, b}.max
      min = {r, g, b}.min
      l = (max + min) / 2

      if max == min
        h = 0.0
        s = 0.0
      else
        d = max - min
        s = l > 0.5 ? d / (2 - max - min) : d / (max + min)

        h = case max
            when r then ((g - b) / d + (g < b ? 6 : 0)) / 6
            when g then ((b - r) / d + 2) / 6
            else        ((r - g) / d + 4) / 6
            end
        h *= 360
      end

      HslColor.new(h, s, l, color.a / 255.0)
    end

    # Convert HSL to RGB.
    def self.hsl_to_rgb(color : HslColor) : RgbaColor
      h = color.h / 360.0
      s = color.s
      l = color.l

      if s == 0
        v = (l * 255).round.to_u8
        return RgbaColor.new(v, v, v, (color.a * 255).round.to_u8)
      end

      q = l < 0.5 ? l * (1 + s) : l + s - l * s
      p = 2 * l - q

      r = hue_to_rgb(p, q, h + 1.0/3.0)
      g = hue_to_rgb(p, q, h)
      b = hue_to_rgb(p, q, h - 1.0/3.0)

      RgbaColor.new(
        (r * 255).round.to_u8,
        (g * 255).round.to_u8,
        (b * 255).round.to_u8,
        (color.a * 255).round.to_u8
      )
    end

    private def self.hue_to_rgb(p : Float64, q : Float64, t : Float64) : Float64
      t += 1 if t < 0
      t -= 1 if t > 1

      return p + (q - p) * 6 * t if t < 1.0/6.0
      return q if t < 0.5
      return p + (q - p) * (2.0/3.0 - t) * 6 if t < 2.0/3.0
      p
    end

    # Lighten color.
    def self.lighten(color : RgbaColor, amount : Float64) : RgbaColor
      hsl = rgb_to_hsl(color)
      new_l = (hsl.l + amount).clamp(0.0, 1.0)
      hsl_to_rgb(HslColor.new(hsl.h, hsl.s, new_l, hsl.a))
    end

    # Darken color.
    def self.darken(color : RgbaColor, amount : Float64) : RgbaColor
      lighten(color, -amount)
    end

    # Mix two colors.
    def self.mix(c1 : RgbaColor, c2 : RgbaColor, weight : Float64 = 0.5) : RgbaColor
      w = weight.clamp(0.0, 1.0)
      RgbaColor.new(
        ((c1.r * (1 - w) + c2.r * w).round.to_u8),
        ((c1.g * (1 - w) + c2.g * w).round.to_u8),
        ((c1.b * (1 - w) + c2.b * w).round.to_u8),
        ((c1.a * (1 - w) + c2.a * w).round.to_u8)
      )
    end

    # Get complementary color.
    def self.complement(color : RgbaColor) : RgbaColor
      hsl = rgb_to_hsl(color)
      new_h = (hsl.h + 180) % 360
      hsl_to_rgb(HslColor.new(new_h, hsl.s, hsl.l, hsl.a))
    end

    # Calculate relative luminance.
    def self.luminance(color : RgbaColor) : Float64
      r = color.r / 255.0
      g = color.g / 255.0
      b = color.b / 255.0

      r = r <= 0.03928 ? r / 12.92 : ((r + 0.055) / 1.055) ** 2.4
      g = g <= 0.03928 ? g / 12.92 : ((g + 0.055) / 1.055) ** 2.4
      b = b <= 0.03928 ? b / 12.92 : ((b + 0.055) / 1.055) ** 2.4

      0.2126 * r + 0.7152 * g + 0.0722 * b
    end

    # Calculate contrast ratio between two colors.
    def self.contrast_ratio(c1 : RgbaColor, c2 : RgbaColor) : Float64
      l1 = luminance(c1)
      l2 = luminance(c2)
      lighter = {l1, l2}.max
      darker = {l1, l2}.min
      (lighter + 0.05) / (darker + 0.05)
    end

    # Check if contrast meets WCAG AA (4.5:1).
    def self.meets_wcag_aa?(c1 : RgbaColor, c2 : RgbaColor) : Bool
      contrast_ratio(c1, c2) >= 4.5
    end

    # Check if contrast meets WCAG AAA (7:1).
    def self.meets_wcag_aaa?(c1 : RgbaColor, c2 : RgbaColor) : Bool
      contrast_ratio(c1, c2) >= 7.0
    end
  end
end
