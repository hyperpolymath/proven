# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # UUID version types per RFC 4122.
  enum UuidVersion
    V1       # Time-based
    V2       # DCE Security
    V3       # Name-based (MD5)
    V4       # Random
    V5       # Name-based (SHA-1)
    Nil      # Nil UUID
    Unknown  # Unknown version
  end

  # UUID variant types per RFC 4122.
  enum UuidVariant
    Ncs       # NCS backward compatibility
    Rfc4122   # RFC 4122 standard
    Microsoft # Microsoft backward compatibility
    Future    # Reserved for future use
  end

  # A validated UUID (128 bits).
  struct Uuid
    getter bytes : StaticArray(UInt8, 16)

    # The nil UUID (all zeros).
    NIL = Uuid.new(StaticArray(UInt8, 16).new(0_u8))

    # DNS namespace UUID.
    NAMESPACE_DNS = Uuid.new(StaticArray[
      0x6b_u8, 0xa7_u8, 0xb8_u8, 0x10_u8, 0x9d_u8, 0xad_u8, 0x11_u8, 0xd1_u8,
      0x80_u8, 0xb4_u8, 0x00_u8, 0xc0_u8, 0x4f_u8, 0xd4_u8, 0x30_u8, 0xc8_u8,
    ])

    # URL namespace UUID.
    NAMESPACE_URL = Uuid.new(StaticArray[
      0x6b_u8, 0xa7_u8, 0xb8_u8, 0x11_u8, 0x9d_u8, 0xad_u8, 0x11_u8, 0xd1_u8,
      0x80_u8, 0xb4_u8, 0x00_u8, 0xc0_u8, 0x4f_u8, 0xd4_u8, 0x30_u8, 0xc8_u8,
    ])

    def initialize(@bytes : StaticArray(UInt8, 16))
    end

    # Create UUID from a slice of bytes.
    def self.from_slice(slice : Bytes) : Uuid?
      return nil unless slice.size == 16
      bytes = StaticArray(UInt8, 16).new(0_u8)
      16.times { |i| bytes[i] = slice[i] }
      Uuid.new(bytes)
    end

    # Get the UUID version.
    def version : UuidVersion
      version_nibble = (@bytes[6] >> 4) & 0x0F
      case version_nibble
      when 1 then UuidVersion::V1
      when 2 then UuidVersion::V2
      when 3 then UuidVersion::V3
      when 4 then UuidVersion::V4
      when 5 then UuidVersion::V5
      when 0 then is_nil? ? UuidVersion::Nil : UuidVersion::Unknown
      else        UuidVersion::Unknown
      end
    end

    # Get the UUID variant.
    def variant : UuidVariant
      byte = @bytes[8]
      if (byte >> 7) == 0
        UuidVariant::Ncs
      elsif (byte >> 6) == 0b10
        UuidVariant::Rfc4122
      elsif (byte >> 5) == 0b110
        UuidVariant::Microsoft
      else
        UuidVariant::Future
      end
    end

    # Check if this is the nil UUID.
    def is_nil? : Bool
      @bytes.all? { |b| b == 0 }
    end

    # Format as canonical string (8-4-4-4-12).
    def to_s : String
      String.build(36) do |str|
        @bytes.each_with_index do |byte, i|
          str << '-' if i == 4 || i == 6 || i == 8 || i == 10
          str << byte.to_s(16, precision: 2)
        end
      end
    end

    # Format as URN.
    def to_urn : String
      "urn:uuid:#{to_s}"
    end

    # Get bytes as a slice.
    def to_slice : Bytes
      Bytes.new(@bytes.to_unsafe, 16)
    end

    # Compare two UUIDs for equality.
    def ==(other : Uuid) : Bool
      @bytes == other.bytes
    end

    # Hash for use in collections.
    def hash(hasher)
      @bytes.hash(hasher)
    end
  end

  # Safe UUID validation and parsing.
  module SafeUuid
    # Parse UUID from canonical string format (8-4-4-4-12).
    def self.parse(input : String) : Uuid?
      return nil unless input.size == 36

      # Validate hyphen positions
      return nil unless input[8] == '-' && input[13] == '-' && input[18] == '-' && input[23] == '-'

      # Extract hex characters
      hex_string = String.build(32) do |str|
        input.each_char_with_index do |char, index|
          next if index == 8 || index == 13 || index == 18 || index == 23
          str << char
        end
      end

      return nil unless hex_string.size == 32

      # Parse hex to bytes
      bytes = StaticArray(UInt8, 16).new(0_u8)
      16.times do |i|
        byte_str = hex_string[i * 2, 2]
        byte_value = byte_str.to_u8?(16)
        return nil if byte_value.nil?
        bytes[i] = byte_value
      end

      Uuid.new(bytes)
    end

    # Generate a v4 (random) UUID from provided random bytes.
    def self.v4_from_bytes(random_bytes : StaticArray(UInt8, 16)) : Uuid
      bytes = random_bytes.dup
      # Set version to 4
      bytes[6] = (bytes[6] & 0x0F) | 0x40
      # Set variant to RFC 4122
      bytes[8] = (bytes[8] & 0x3F) | 0x80
      Uuid.new(bytes)
    end

    # Generate a v4 (random) UUID from a slice.
    def self.v4_from_slice(random_slice : Bytes) : Uuid?
      return nil unless random_slice.size == 16
      bytes = StaticArray(UInt8, 16).new(0_u8)
      16.times { |i| bytes[i] = random_slice[i] }
      v4_from_bytes(bytes)
    end

    # Generate a random v4 UUID.
    def self.v4 : Uuid
      bytes = StaticArray(UInt8, 16).new(0_u8)
      random = Random::Secure.random_bytes(16)
      16.times { |i| bytes[i] = random[i] }
      v4_from_bytes(bytes)
    end

    # Check if string is valid UUID format.
    def self.valid?(input : String) : Bool
      !parse(input).nil?
    end

    # Get the nil UUID.
    def self.nil_uuid : Uuid
      Uuid::NIL
    end

    # Compare two UUIDs for equality.
    def self.equal?(a : Uuid, b : Uuid) : Bool
      a == b
    end

    # Format UUID with uppercase hex.
    def self.format_upper(uuid : Uuid) : String
      uuid.to_s.upcase
    end

    # Format UUID without hyphens.
    def self.format_compact(uuid : Uuid) : String
      uuid.to_s.gsub("-", "")
    end

    # Parse compact format (32 hex chars, no hyphens).
    def self.parse_compact(input : String) : Uuid?
      return nil unless input.size == 32
      return nil unless input.chars.all? { |c| c.ascii_hexdigit? }

      # Insert hyphens to convert to canonical format
      canonical = "#{input[0, 8]}-#{input[8, 4]}-#{input[12, 4]}-#{input[16, 4]}-#{input[20, 12]}"
      parse(canonical)
    end
  end
end

require "random/secure"
