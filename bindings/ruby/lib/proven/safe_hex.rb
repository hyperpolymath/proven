# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe hexadecimal encoding and decoding.
  module SafeHex
    # Valid hex characters.
    HEX_CHARS = "0123456789abcdefABCDEF"

    class << self
      # Check if character is valid hex.
      #
      # @param char [String] single character
      # @return [Boolean]
      def hex_char?(char)
        HEX_CHARS.include?(char)
      end

      # Convert hex char to nibble value.
      #
      # @param char [String] single character
      # @return [Integer, nil]
      def hex_char_to_nibble(char)
        case char
        when "0".."9" then char.ord - "0".ord
        when "a".."f" then char.ord - "a".ord + 10
        when "A".."F" then char.ord - "A".ord + 10
        end
      end

      # Convert nibble to lowercase hex char.
      #
      # @param nibble [Integer] 0-15
      # @return [String]
      def nibble_to_hex_char(nibble)
        if nibble < 10
          ("0".ord + nibble).chr
        else
          ("a".ord + nibble - 10).chr
        end
      end

      # Convert nibble to uppercase hex char.
      #
      # @param nibble [Integer] 0-15
      # @return [String]
      def nibble_to_hex_char_upper(nibble)
        if nibble < 10
          ("0".ord + nibble).chr
        else
          ("A".ord + nibble - 10).chr
        end
      end

      # Encode bytes to lowercase hex string.
      #
      # @param bytes [String, Array<Integer>] binary data
      # @return [String]
      def encode(bytes)
        byte_array = bytes.is_a?(String) ? bytes.bytes : bytes
        byte_array.map { |b| format("%02x", b) }.join
      end

      # Encode bytes to uppercase hex string.
      #
      # @param bytes [String, Array<Integer>] binary data
      # @return [String]
      def encode_upper(bytes)
        byte_array = bytes.is_a?(String) ? bytes.bytes : bytes
        byte_array.map { |b| format("%02X", b) }.join
      end

      # Decode hex string to bytes.
      #
      # @param hex_string [String]
      # @return [String, nil] binary string, nil on error
      def decode(hex_string)
        return nil if hex_string.length.odd?

        bytes = []
        hex_string.chars.each_slice(2) do |pair|
          high = hex_char_to_nibble(pair[0])
          low = hex_char_to_nibble(pair[1])
          return nil if high.nil? || low.nil?

          bytes << ((high << 4) | low)
        end

        bytes.pack("C*")
      end

      # Decode hex string, raising on failure.
      #
      # @param hex_string [String]
      # @return [String]
      # @raise [ArgumentError]
      def decode!(hex_string)
        result = decode(hex_string)
        raise ArgumentError, "Invalid hex string: #{hex_string}" if result.nil?

        result
      end

      # Validate hex string (all characters are hex).
      #
      # @param hex_string [String]
      # @return [Boolean]
      def valid?(hex_string)
        hex_string.chars.all? { |c| hex_char?(c) }
      end

      # Validate hex string with even length (valid for byte decoding).
      #
      # @param hex_string [String]
      # @return [Boolean]
      def valid_bytes?(hex_string)
        hex_string.length.even? && valid?(hex_string)
      end

      # Format hex with spaces between bytes.
      #
      # @param hex_string [String]
      # @return [String, nil] nil on error
      def format_spaced(hex_string)
        return nil if hex_string.length.odd?
        return "" if hex_string.empty?

        hex_string.chars.each_slice(2).map(&:join).join(" ")
      end

      # Format hex with colons between bytes.
      #
      # @param hex_string [String]
      # @return [String, nil] nil on error
      def format_colons(hex_string)
        return nil if hex_string.length.odd?
        return "" if hex_string.empty?

        hex_string.chars.each_slice(2).map(&:join).join(":")
      end

      # Constant-time comparison of hex strings.
      # Prevents timing attacks by always comparing all bytes.
      #
      # @param hex_a [String]
      # @param hex_b [String]
      # @return [Boolean]
      def constant_time_equal?(hex_a, hex_b)
        return false if hex_a.length != hex_b.length

        a_lower = hex_a.downcase
        b_lower = hex_b.downcase

        diff = 0
        a_lower.bytes.zip(b_lower.bytes).each do |byte_a, byte_b|
          diff |= byte_a ^ byte_b
        end

        diff.zero?
      end

      # Convert integer to hex string with minimum width.
      #
      # @param value [Integer]
      # @param min_width [Integer]
      # @return [String]
      def int_to_hex(value, min_width = 0)
        hex = format("%x", value)
        hex.rjust(min_width, "0")
      end

      # Convert integer to uppercase hex string with minimum width.
      #
      # @param value [Integer]
      # @param min_width [Integer]
      # @return [String]
      def int_to_hex_upper(value, min_width = 0)
        hex = format("%X", value)
        hex.rjust(min_width, "0")
      end

      # Parse hex string to integer.
      #
      # @param hex_string [String]
      # @return [Integer, nil]
      def hex_to_int(hex_string)
        return nil unless valid?(hex_string)
        return nil if hex_string.empty?

        hex_string.to_i(16)
      end

      # Parse hex string to integer, raising on failure.
      #
      # @param hex_string [String]
      # @return [Integer]
      # @raise [ArgumentError]
      def hex_to_int!(hex_string)
        result = hex_to_int(hex_string)
        raise ArgumentError, "Invalid hex number: #{hex_string}" if result.nil?

        result
      end
    end
  end
end
