# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe hexadecimal operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeHex
    class << self
      # Encode bytes to lowercase hex string.
      # Returns nil on error.
      #
      # @param bytes [String] binary data
      # @return [String, nil]
      def encode(bytes)
        return nil if bytes.nil?
        ptr, len = FFI.str_to_ptr(bytes)
        result = FFI.invoke_string_result(
          "proven_hex_encode",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T, Fiddle::TYPE_INT],
          [ptr, len, 0] # uppercase = false
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      end

      # Encode bytes to uppercase hex string.
      # Returns nil on error.
      #
      # @param bytes [String] binary data
      # @return [String, nil]
      def encode_upper(bytes)
        return nil if bytes.nil?
        ptr, len = FFI.str_to_ptr(bytes)
        result = FFI.invoke_string_result(
          "proven_hex_encode",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T, Fiddle::TYPE_INT],
          [ptr, len, 1] # uppercase = true
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      end

      # Decode hex string to binary bytes.
      # Returns nil on invalid hex input.
      #
      # @param hex_string [String]
      # @return [String, nil] binary string
      def decode(hex_string)
        return nil if hex_string.nil?
        ptr, len = FFI.str_to_ptr(hex_string)

        # HexDecodeResult = { i32 status, ptr data, size_t length }
        # Same layout as StringResult (24 bytes with sret)
        buf = Fiddle::Pointer.malloc(32, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_hex_decode"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, ptr, len)

        packed = buf.to_str(24)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        data_ptr_val = packed[8, 8].unpack1("Q")
        length = packed[16, 8].unpack1("Q")

        if data_ptr_val != 0 && length > 0
          data_ptr = Fiddle::Pointer.new(data_ptr_val)
          bytes = data_ptr.to_str(length).dup

          # Free via proven_hex_free
          begin
            free_fn = Fiddle::Function.new(
              FFI.handler["proven_hex_free"],
              [Fiddle::TYPE_VOIDP],
              Fiddle::TYPE_VOID
            )
            free_fn.call(buf)
          rescue Fiddle::DLError
            # If free not available, best-effort
          end

          bytes
        else
          nil
        end
      rescue Fiddle::DLError, TypeError
        nil
      end
    end
  end
end
