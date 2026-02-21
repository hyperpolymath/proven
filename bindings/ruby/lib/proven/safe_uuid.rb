# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "securerandom"

module Proven
  # Safe UUID generation and validation following RFC 4122.
  module SafeUuid
    # UUID version types.
    module Version
      V1 = 1  # Time-based
      V2 = 2  # DCE Security
      V3 = 3  # Name-based (MD5)
      V4 = 4  # Random
      V5 = 5  # Name-based (SHA-1)
      NIL = 0 # Nil UUID
    end

    # UUID variant types.
    module Variant
      NCS = :ncs
      RFC4122 = :rfc4122
      MICROSOFT = :microsoft
      FUTURE = :future
    end

    # Represents a validated UUID (128 bits).
    class Uuid
      # The nil UUID (all zeros).
      NIL = new([0] * 16)

      # DNS namespace UUID.
      NAMESPACE_DNS = new([
        0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
      ])

      # URL namespace UUID.
      NAMESPACE_URL = new([
        0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
      ])

      attr_reader :bytes

      # Create UUID from bytes.
      #
      # @param bytes [Array<Integer>] 16 bytes
      def initialize(bytes)
        @bytes = bytes.dup.freeze
      end

      # Get the UUID version.
      #
      # @return [Integer]
      def version
        version_nibble = (@bytes[6] >> 4) & 0x0F
        case version_nibble
        when 1 then Version::V1
        when 2 then Version::V2
        when 3 then Version::V3
        when 4 then Version::V4
        when 5 then Version::V5
        else Version::NIL
        end
      end

      # Get the UUID variant.
      #
      # @return [Symbol]
      def variant
        byte = @bytes[8]
        if (byte >> 7).zero?
          Variant::NCS
        elsif (byte >> 6) == 0b10
          Variant::RFC4122
        elsif (byte >> 5) == 0b110
          Variant::MICROSOFT
        else
          Variant::FUTURE
        end
      end

      # Check if this is the nil UUID.
      #
      # @return [Boolean]
      def nil_uuid?
        @bytes.all?(&:zero?)
      end

      # Format as canonical string.
      #
      # @return [String]
      def to_s
        hex = @bytes.map { |b| format("%02x", b) }.join
        "#{hex[0, 8]}-#{hex[8, 4]}-#{hex[12, 4]}-#{hex[16, 4]}-#{hex[20, 12]}"
      end

      # Format as URN.
      #
      # @return [String]
      def to_urn
        "urn:uuid:#{self}"
      end

      # Equality check.
      #
      # @param other [Uuid]
      # @return [Boolean]
      def ==(other)
        return false unless other.is_a?(Uuid)

        @bytes == other.bytes
      end

      alias eql? ==

      # Hash for use in collections.
      #
      # @return [Integer]
      def hash
        @bytes.hash
      end
    end

    class << self
      # Parse UUID from canonical string format.
      #
      # @param uuid_string [String]
      # @return [Uuid, nil]
      def parse(uuid_string)
        return nil unless uuid_string.is_a?(String)
        return nil unless uuid_string.length == 36

        chars = uuid_string.chars
        return nil unless chars[8] == "-" && chars[13] == "-" && chars[18] == "-" && chars[23] == "-"

        hex_string = uuid_string.delete("-")
        return nil unless hex_string.length == 32
        return nil unless hex_string.match?(/\A[0-9a-fA-F]+\z/)

        bytes = hex_string.scan(/../).map { |pair| pair.to_i(16) }
        Uuid.new(bytes)
      end

      # Parse UUID, raising on failure.
      #
      # @param uuid_string [String]
      # @return [Uuid]
      # @raise [ArgumentError]
      def parse!(uuid_string)
        result = parse(uuid_string)
        raise ArgumentError, "Invalid UUID format: #{uuid_string}" if result.nil?

        result
      end

      # Generate a v4 (random) UUID.
      #
      # @return [Uuid]
      def v4
        random_bytes = SecureRandom.random_bytes(16).bytes
        v4_from_bytes(random_bytes)
      end

      # Generate a v4 UUID from provided random bytes.
      #
      # @param random_bytes [Array<Integer>] 16 bytes
      # @return [Uuid]
      def v4_from_bytes(random_bytes)
        bytes = random_bytes.dup
        # Set version to 4
        bytes[6] = (bytes[6] & 0x0F) | 0x40
        # Set variant to RFC 4122
        bytes[8] = (bytes[8] & 0x3F) | 0x80
        Uuid.new(bytes)
      end

      # Check if string is valid UUID format.
      #
      # @param uuid_string [String]
      # @return [Boolean]
      def valid?(uuid_string)
        !parse(uuid_string).nil?
      end

      # Format a UUID as canonical string.
      #
      # @param uuid [Uuid]
      # @return [String]
      def format(uuid)
        uuid.to_s
      end

      # Format a UUID as URN.
      #
      # @param uuid [Uuid]
      # @return [String]
      def format_urn(uuid)
        uuid.to_urn
      end

      # Create nil UUID.
      #
      # @return [Uuid]
      def nil_uuid
        Uuid::NIL
      end
    end
  end
end
