# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeColor -- thin FFI wrapper around libproven's color operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeColor
    # Parse a hex color string (e.g. "#FF0000" or "#F00").
    # Returns {r, g, b} or nil.
    def self.parse_hex(input : String) : {UInt8, UInt8, UInt8}?
      slice = input.to_slice
      result = LibProven.proven_color_parse_hex(slice.to_unsafe, slice.size)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      {result.color.r, result.color.g, result.color.b}
    end

    # Convert RGB to HSL.  Returns {h, s, l}.
    def self.rgb_to_hsl(r : UInt8, g : UInt8, b : UInt8) : {Float64, Float64, Float64}
      rgb = LibProven::RGBColor.new
      rgb.r = r
      rgb.g = g
      rgb.b = b
      hsl = LibProven.proven_color_rgb_to_hsl(rgb)
      {hsl.h, hsl.s, hsl.l}
    end

    # Format RGB as hex string (e.g. "#rrggbb").
    def self.to_hex(r : UInt8, g : UInt8, b : UInt8) : String?
      rgb = LibProven::RGBColor.new
      rgb.r = r
      rgb.g = g
      rgb.b = b
      result = LibProven.proven_color_to_hex(rgb)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      return nil if result.value.null?
      str = String.new(result.value, result.length)
      LibProven.proven_free_string(result.value)
      str
    end
  end
end
