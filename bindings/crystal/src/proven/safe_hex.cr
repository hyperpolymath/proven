# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Safe hexadecimal encoding, decoding, and comparison.
  module SafeHex
    # Lowercase hex alphabet.
    HEX_LOWER = "0123456789abcdef"

    # Uppercase hex alphabet.
    HEX_UPPER = "0123456789ABCDEF"

    # Check if a character is a valid hex digit.
    def self.hex_char?(char : Char) : Bool
      char.ascii_hexdigit?
    end

    # Convert a hex character to its nibble value (0-15).
    def self.hex_char_to_nibble(char : Char) : UInt8?
      case char
      when '0'..'9' then (char.ord - '0'.ord).to_u8
      when 'a'..'f' then (char.ord - 'a'.ord + 10).to_u8
      when 'A'..'F' then (char.ord - 'A'.ord + 10).to_u8
      else               nil
      end
    end

    # Convert a nibble value (0-15) to a lowercase hex character.
    def self.nibble_to_hex_char(nibble : UInt8) : Char
      HEX_LOWER[nibble & 0x0F]
    end

    # Convert a nibble value (0-15) to an uppercase hex character.
    def self.nibble_to_hex_char_upper(nibble : UInt8) : Char
      HEX_UPPER[nibble & 0x0F]
    end

    # Encode bytes to lowercase hex string.
    def self.encode(bytes : Bytes) : String
      String.build(bytes.size * 2) do |str|
        bytes.each do |byte|
          str << nibble_to_hex_char((byte >> 4) & 0x0F)
          str << nibble_to_hex_char(byte & 0x0F)
        end
      end
    end

    # Encode bytes to uppercase hex string.
    def self.encode_upper(bytes : Bytes) : String
      String.build(bytes.size * 2) do |str|
        bytes.each do |byte|
          str << nibble_to_hex_char_upper((byte >> 4) & 0x0F)
          str << nibble_to_hex_char_upper(byte & 0x0F)
        end
      end
    end

    # Decode hex string to bytes.
    def self.decode(hex : String) : Bytes?
      return nil if hex.size.odd?
      return Bytes.empty if hex.empty?

      result = Bytes.new(hex.size // 2)
      chars = hex.chars

      (hex.size // 2).times do |i|
        high = hex_char_to_nibble(chars[i * 2])
        low = hex_char_to_nibble(chars[i * 2 + 1])
        return nil if high.nil? || low.nil?
        result[i] = (high << 4) | low
      end

      result
    end

    # Validate hex string (all characters are hex digits).
    def self.valid?(input : String) : Bool
      input.chars.all? { |c| c.ascii_hexdigit? }
    end

    # Validate hex string with even length (can be decoded to bytes).
    def self.valid_bytes?(input : String) : Bool
      input.size.even? && valid?(input)
    end

    # Format hex with spaces between bytes.
    def self.format_spaced(hex : String) : String?
      return nil if hex.size.odd?
      return "" if hex.empty?

      String.build(hex.size + hex.size // 2 - 1) do |str|
        chars = hex.chars
        (hex.size // 2).times do |i|
          str << ' ' if i > 0
          str << chars[i * 2]
          str << chars[i * 2 + 1]
        end
      end
    end

    # Format hex with colons between bytes.
    def self.format_colons(hex : String) : String?
      return nil if hex.size.odd?
      return "" if hex.empty?

      String.build(hex.size + hex.size // 2 - 1) do |str|
        chars = hex.chars
        (hex.size // 2).times do |i|
          str << ':' if i > 0
          str << chars[i * 2]
          str << chars[i * 2 + 1]
        end
      end
    end

    # Constant-time comparison of two hex strings (timing-attack safe).
    def self.constant_time_equal?(a : String, b : String) : Bool
      return false if a.size != b.size

      a_lower = a.downcase
      b_lower = b.downcase

      diff = 0_u8
      a_lower.each_char_with_index do |char_a, i|
        char_b = b_lower[i]
        diff |= (char_a.ord ^ char_b.ord).to_u8
      end

      diff == 0
    end

    # Constant-time comparison of two byte arrays.
    def self.constant_time_bytes_equal?(a : Bytes, b : Bytes) : Bool
      return false if a.size != b.size

      diff = 0_u8
      a.size.times do |i|
        diff |= a[i] ^ b[i]
      end

      diff == 0
    end

    # Convert an unsigned integer to hex string with minimum width.
    def self.int_to_hex(value : UInt64, min_width : Int32 = 0) : String
      hex = value.to_s(16)
      if hex.size >= min_width
        hex
      else
        hex.rjust(min_width, '0')
      end
    end

    # Convert an unsigned integer to uppercase hex string with minimum width.
    def self.int_to_hex_upper(value : UInt64, min_width : Int32 = 0) : String
      hex = value.to_s(16).upcase
      if hex.size >= min_width
        hex
      else
        hex.rjust(min_width, '0')
      end
    end

    # Parse hex string to unsigned 64-bit integer.
    def self.hex_to_int(hex : String) : UInt64?
      return nil if hex.empty?
      return nil unless valid?(hex)
      hex.to_u64?(16)
    end

    # Parse hex string to unsigned 32-bit integer.
    def self.hex_to_int32(hex : String) : UInt32?
      return nil if hex.empty?
      return nil unless valid?(hex)
      hex.to_u32?(16)
    end

    # Strip "0x" or "0X" prefix if present.
    def self.strip_prefix(hex : String) : String
      if hex.starts_with?("0x") || hex.starts_with?("0X")
        hex[2..]
      else
        hex
      end
    end

    # Add "0x" prefix.
    def self.add_prefix(hex : String) : String
      "0x#{hex}"
    end

    # XOR two hex strings of equal length.
    def self.xor(a : String, b : String) : String?
      return nil if a.size != b.size
      return nil if a.size.odd?

      bytes_a = decode(a)
      bytes_b = decode(b)
      return nil if bytes_a.nil? || bytes_b.nil?

      result = Bytes.new(bytes_a.size)
      bytes_a.size.times do |i|
        result[i] = bytes_a[i] ^ bytes_b[i]
      end

      encode(result)
    end

    # Reverse the byte order of a hex string.
    def self.reverse_bytes(hex : String) : String?
      return nil if hex.size.odd?
      return "" if hex.empty?

      bytes = decode(hex)
      return nil if bytes.nil?

      reversed = Bytes.new(bytes.size)
      bytes.size.times do |i|
        reversed[i] = bytes[bytes.size - 1 - i]
      end

      encode(reversed)
    end

    # Generate random hex string of specified byte length.
    def self.random(byte_count : Int32) : String
      bytes = Random::Secure.random_bytes(byte_count)
      encode(bytes)
    end

    # Generate random hex string with uppercase.
    def self.random_upper(byte_count : Int32) : String
      bytes = Random::Secure.random_bytes(byte_count)
      encode_upper(bytes)
    end
  end
end

require "random/secure"
