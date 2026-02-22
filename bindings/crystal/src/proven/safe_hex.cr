# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeHex -- thin FFI wrapper around libproven's hex operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeHex
    # Encode bytes to lowercase hex string.
    def self.encode(bytes : Bytes) : String?
      out_size = LibProven.hex_encode_size(bytes.size)
      buf = Bytes.new(out_size)
      result = LibProven.hex_encode(bytes.to_unsafe, bytes.size, buf.to_unsafe, buf.size)
      return nil unless result.status == 0
      String.new(buf.to_unsafe, result.chars_written)
    end

    # Encode bytes to uppercase hex string.
    def self.encode_upper(bytes : Bytes) : String?
      out_size = LibProven.hex_encode_size(bytes.size)
      buf = Bytes.new(out_size)
      result = LibProven.hex_encode_upper(bytes.to_unsafe, bytes.size, buf.to_unsafe, buf.size)
      return nil unless result.status == 0
      String.new(buf.to_unsafe, result.chars_written)
    end

    # Decode hex string to bytes.
    def self.decode(hex : String) : Bytes?
      slice = hex.to_slice
      out_size = LibProven.hex_decode_size(slice.size)
      buf = Bytes.new(out_size)
      result = LibProven.hex_decode(slice.to_unsafe, slice.size, buf.to_unsafe, buf.size)
      return nil unless result.status == 0
      buf[0, result.bytes_written]
    end

    # Validate hex string (all characters are hex digits).
    def self.valid?(input : String) : Bool
      slice = input.to_slice
      LibProven.hex_is_valid(slice.to_unsafe, slice.size)
    end

    # Validate hex bytes string (even length, all valid hex chars).
    def self.valid_bytes?(input : String) : Bool
      slice = input.to_slice
      LibProven.hex_is_valid_bytes(slice.to_unsafe, slice.size)
    end

    # Constant-time hex string comparison (case-insensitive, timing-safe).
    def self.constant_time_equal?(a : String, b : String) : Bool
      a_slice = a.to_slice
      b_slice = b.to_slice
      LibProven.hex_constant_time_eq(
        a_slice.to_unsafe, a_slice.size,
        b_slice.to_unsafe, b_slice.size
      )
    end

    # Parse hex string to unsigned 64-bit integer.
    def self.hex_to_int(hex : String) : UInt64?
      slice = hex.to_slice
      result = LibProven.hex_to_int(slice.to_unsafe, slice.size)
      return nil unless result.status == 0
      result.value
    end

    # Convert integer to hex string.
    def self.int_to_hex(value : UInt64, min_width : LibC::SizeT = 0, uppercase : Bool = false) : String?
      buf = Bytes.new(20) # max 16 hex digits + null + padding
      result = LibProven.hex_from_int(value, min_width, uppercase, buf.to_unsafe, buf.size)
      return nil unless result.status == 0
      String.new(buf.to_unsafe, result.chars_written)
    end

    # Format hex with spaces between bytes (e.g. "aa bb cc").
    def self.format_spaced(hex : String) : String?
      slice = hex.to_slice
      buf = Bytes.new(slice.size * 2)
      result = LibProven.hex_format_spaced(slice.to_unsafe, slice.size, buf.to_unsafe, buf.size)
      return nil unless result.status == 0
      String.new(buf.to_unsafe, result.chars_written)
    end

    # Format hex with colons between bytes (e.g. "aa:bb:cc").
    def self.format_colons(hex : String) : String?
      slice = hex.to_slice
      buf = Bytes.new(slice.size * 2)
      result = LibProven.hex_format_colons(slice.to_unsafe, slice.size, buf.to_unsafe, buf.size)
      return nil unless result.status == 0
      String.new(buf.to_unsafe, result.chars_written)
    end
  end
end
