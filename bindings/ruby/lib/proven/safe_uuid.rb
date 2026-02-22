# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe UUID operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeUuid
    class << self
      # Generate a v4 (random) UUID string.
      # Returns nil on error.
      #
      # @return [String, nil] canonical UUID string (e.g., "550e8400-...")
      def v4
        # UUIDResult = { i32 status, [16]u8 uuid_bytes } = 20 bytes
        buf = Fiddle::Pointer.malloc(24, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_uuid_v4"],
          [Fiddle::TYPE_VOIDP],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf)

        packed = buf.to_str(20)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        uuid_bytes = packed[4, 16]
        uuid_to_string(uuid_bytes)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Parse a UUID string, returning the canonical form.
      # Returns nil on invalid input.
      #
      # @param uuid_string [String]
      # @return [String, nil]
      def parse(uuid_string)
        return nil if uuid_string.nil?
        ptr, len = FFI.str_to_ptr(uuid_string)

        # UUIDResult = { i32 status, [16]u8 uuid } = 20 bytes
        buf = Fiddle::Pointer.malloc(24, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_uuid_parse"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, ptr, len)

        packed = buf.to_str(20)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        uuid_bytes = packed[4, 16]
        uuid_to_string(uuid_bytes)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Check if string is valid UUID format.
      #
      # @param uuid_string [String]
      # @return [Boolean]
      def valid?(uuid_string)
        !parse(uuid_string).nil?
      end

      # Check if a UUID is the nil UUID (all zeros).
      #
      # @param uuid_string [String]
      # @return [Boolean, nil]
      def nil_uuid?(uuid_string)
        parsed = parse(uuid_string)
        return nil if parsed.nil?
        parsed == "00000000-0000-0000-0000-000000000000"
      end

      # Get the version of a parsed UUID.
      #
      # @param uuid_string [String]
      # @return [Integer, nil]
      def version(uuid_string)
        parsed_bytes = parse_to_bytes(uuid_string)
        return nil if parsed_bytes.nil?

        # Pack bytes and call proven_uuid_version
        packed_uuid = parsed_bytes.pack("C16")
        uuid_ptr = Fiddle::Pointer.to_ptr(packed_uuid)

        # UUID is passed as 16 bytes (struct by value), but since it's
        # exactly 16 bytes, on x86_64 it fits in two registers.
        # Use the int-based approach: pass as two i64 values.
        high = parsed_bytes[0, 8].pack("C8").unpack1("Q>")
        low = parsed_bytes[8, 8].pack("C8").unpack1("Q>")

        FFI.invoke_u8(
          "proven_uuid_version",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [high, low]
        )
      rescue Fiddle::DLError, TypeError
        nil
      end

      private

      # Format 16 bytes as canonical UUID string.
      def uuid_to_string(bytes)
        hex = bytes.unpack1("H*")
        "#{hex[0, 8]}-#{hex[8, 4]}-#{hex[12, 4]}-#{hex[16, 4]}-#{hex[20, 12]}"
      end

      # Parse UUID string into byte array (for passing to C).
      def parse_to_bytes(uuid_string)
        return nil if uuid_string.nil?
        return nil unless uuid_string.length == 36

        hex = uuid_string.delete("-")
        return nil unless hex.length == 32
        return nil unless hex.match?(/\A[0-9a-fA-F]+\z/)

        hex.scan(/../).map { |pair| pair.to_i(16) }
      end
    end
  end
end
