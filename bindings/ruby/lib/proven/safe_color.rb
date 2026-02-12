# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe color handling with validation and WCAG contrast calculations.
  #
  # Provides RGB/RGBA color types with safe conversions and
  # accessibility-focused contrast ratio calculations.
  module SafeColor
    # RGB color representation.
    class RGB
      attr_reader :r, :g, :b

      def initialize(r, g, b)
        @r = r.clamp(0, 255).to_i
        @g = g.clamp(0, 255).to_i
        @b = b.clamp(0, 255).to_i
      end

      def to_hex
        format("#%02X%02X%02X", r, g, b)
      end

      def luminance
        SafeColor.luminance(self)
      end

      def to_a
        [r, g, b]
      end

      def ==(other)
        other.is_a?(RGB) && r == other.r && g == other.g && b == other.b
      end

      # Common color constants
      BLACK = RGB.new(0, 0, 0)
      WHITE = RGB.new(255, 255, 255)
      RED = RGB.new(255, 0, 0)
      GREEN = RGB.new(0, 255, 0)
      BLUE = RGB.new(0, 0, 255)
    end

    # RGBA color representation.
    class RGBA
      attr_reader :r, :g, :b, :a

      def initialize(r, g, b, a)
        @r = r.clamp(0, 255).to_i
        @g = g.clamp(0, 255).to_i
        @b = b.clamp(0, 255).to_i
        @a = a.clamp(0, 255).to_i
      end

      def to_rgb
        RGB.new(r, g, b)
      end

      def to_a
        [r, g, b, a]
      end

      def opacity
        a / 255.0
      end

      def ==(other)
        other.is_a?(RGBA) && r == other.r && g == other.g && b == other.b && a == other.a
      end
    end

    # Parse hex color string to RGB.
    #
    # @param hex_string [String] e.g., "#FF0000" or "FF0000"
    # @return [Result]
    def self.from_hex(hex_string)
      return Result.error(InvalidInputError.new("Hex string cannot be nil")) if hex_string.nil?

      clean = hex_string.strip.delete_prefix("#")

      # Support short form (e.g., "F00" -> "FF0000")
      clean = clean.chars.map { |c| c * 2 }.join if clean.length == 3

      unless clean.match?(/\A[0-9A-Fa-f]{6}\z/)
        return Result.error(InvalidFormatError.new("Invalid hex color format"))
      end

      r = clean[0, 2].to_i(16)
      g = clean[2, 2].to_i(16)
      b = clean[4, 2].to_i(16)

      Result.ok(RGB.new(r, g, b))
    end

    # Parse RGBA hex string.
    #
    # @param hex_string [String] e.g., "#FF0000FF" (with alpha)
    # @return [Result]
    def self.from_hex_rgba(hex_string)
      return Result.error(InvalidInputError.new("Hex string cannot be nil")) if hex_string.nil?

      clean = hex_string.strip.delete_prefix("#")

      unless clean.match?(/\A[0-9A-Fa-f]{8}\z/)
        return Result.error(InvalidFormatError.new("Invalid RGBA hex format"))
      end

      r = clean[0, 2].to_i(16)
      g = clean[2, 2].to_i(16)
      b = clean[4, 2].to_i(16)
      a = clean[6, 2].to_i(16)

      Result.ok(RGBA.new(r, g, b, a))
    end

    # Calculate relative luminance (WCAG formula).
    #
    # @param color [RGB]
    # @return [Float]
    def self.luminance(color)
      r = gamma_correct(color.r / 255.0)
      g = gamma_correct(color.g / 255.0)
      b = gamma_correct(color.b / 255.0)
      0.2126 * r + 0.7152 * g + 0.0722 * b
    end

    # Calculate WCAG contrast ratio between two colors.
    #
    # @param color1 [RGB]
    # @param color2 [RGB]
    # @return [Float]
    def self.contrast_ratio(color1, color2)
      l1 = luminance(color1)
      l2 = luminance(color2)
      lighter = [l1, l2].max
      darker = [l1, l2].min
      (lighter + 0.05) / (darker + 0.05)
    end

    # Check if contrast meets WCAG AA standard (4.5:1 for normal text).
    #
    # @param color1 [RGB]
    # @param color2 [RGB]
    # @return [Boolean]
    def self.meets_wcag_aa?(color1, color2)
      contrast_ratio(color1, color2) >= 4.5
    end

    # Check if contrast meets WCAG AAA standard (7:1 for normal text).
    #
    # @param color1 [RGB]
    # @param color2 [RGB]
    # @return [Boolean]
    def self.meets_wcag_aaa?(color1, color2)
      contrast_ratio(color1, color2) >= 7.0
    end

    # Check if contrast meets WCAG AA for large text (3:1).
    #
    # @param color1 [RGB]
    # @param color2 [RGB]
    # @return [Boolean]
    def self.meets_wcag_aa_large?(color1, color2)
      contrast_ratio(color1, color2) >= 3.0
    end

    # Blend foreground color with background using alpha.
    #
    # @param fg [RGBA] foreground color with alpha
    # @param bg [RGB] background color
    # @return [RGB]
    def self.blend(fg, bg)
      alpha = fg.a / 255.0
      inv_alpha = 1.0 - alpha

      RGB.new(
        (fg.r * alpha + bg.r * inv_alpha).round,
        (fg.g * alpha + bg.g * inv_alpha).round,
        (fg.b * alpha + bg.b * inv_alpha).round
      )
    end

    # Lighten a color by percentage.
    #
    # @param color [RGB]
    # @param percent [Float] 0.0 to 1.0
    # @return [RGB]
    def self.lighten(color, percent)
      RGB.new(
        (color.r + (255 - color.r) * percent).round,
        (color.g + (255 - color.g) * percent).round,
        (color.b + (255 - color.b) * percent).round
      )
    end

    # Darken a color by percentage.
    #
    # @param color [RGB]
    # @param percent [Float] 0.0 to 1.0
    # @return [RGB]
    def self.darken(color, percent)
      RGB.new(
        (color.r * (1 - percent)).round,
        (color.g * (1 - percent)).round,
        (color.b * (1 - percent)).round
      )
    end

    # Get grayscale equivalent.
    #
    # @param color [RGB]
    # @return [RGB]
    def self.grayscale(color)
      gray = (0.299 * color.r + 0.587 * color.g + 0.114 * color.b).round
      RGB.new(gray, gray, gray)
    end

    # Invert a color.
    #
    # @param color [RGB]
    # @return [RGB]
    def self.invert(color)
      RGB.new(255 - color.r, 255 - color.g, 255 - color.b)
    end

    class << self
      private

      def gamma_correct(value)
        if value <= 0.03928
          value / 12.92
        else
          ((value + 0.055) / 1.055)**2.4
        end
      end
    end
  end
end
