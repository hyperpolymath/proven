# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe color operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeColor
    # RGB color (0-255 per channel).
    RGB = Struct.new(:r, :g, :b, keyword_init: true) do
      def to_hex
        format("#%02X%02X%02X", r, g, b)
      end
    end

    # HSL color.
    HSL = Struct.new(:h, :s, :l, keyword_init: true)

    class << self
      # Parse hex color string to RGB.
      # Returns nil on invalid input.
      #
      # @param hex_string [String] e.g., "#FF0000" or "FF0000"
      # @return [RGB, nil]
      def from_hex(hex_string)
        return nil if hex_string.nil?
        ptr, len = FFI.str_to_ptr(hex_string)

        # ColorParseResult = { i32 status, RGBColor { u8 r, u8 g, u8 b } }
        # Layout: 4(status) + 1(r) + 1(g) + 1(b) + 1pad = 8 bytes
        buf = Fiddle::Pointer.malloc(8, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_color_parse_hex"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, ptr, len)

        packed = buf.to_str(8)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        r, g, b = packed[4, 3].unpack("CCC")
        RGB.new(r: r, g: g, b: b)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Convert RGB to HSL.
      #
      # @param color [RGB]
      # @return [HSL, nil]
      def rgb_to_hsl(color)
        return nil if color.nil?

        # RGBColor = { u8 r, u8 g, u8 b } = 3 bytes, passed as i32
        rgb_packed = color.r | (color.g << 8) | (color.b << 16)

        # HSLColor = { f32 h, f32 s, f32 l } = 12 bytes
        buf = Fiddle::Pointer.malloc(16, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_color_rgb_to_hsl"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_INT],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, rgb_packed)

        packed = buf.to_str(12)
        h, s, l = packed.unpack("eee") # 3x float32 little-endian
        HSL.new(h: h, s: s, l: l)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Convert RGB to hex string.
      # Returns nil on error.
      #
      # @param color [RGB]
      # @return [String, nil]
      def to_hex(color)
        return nil if color.nil?

        rgb_packed = color.r | (color.g << 8) | (color.b << 16)
        result = FFI.invoke_string_result(
          "proven_color_to_hex",
          [Fiddle::TYPE_INT],
          [rgb_packed]
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      rescue Fiddle::DLError, TypeError
        nil
      end
    end
  end
end
