# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe URL operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeUrl
    # Parsed URL components from libproven.
    UrlComponents = Struct.new(
      :scheme, :host, :port, :path, :query, :fragment,
      keyword_init: true
    )

    class << self
      # Parse a URL into components.
      # Returns nil on invalid input.
      #
      # @param url_string [String]
      # @return [UrlComponents, nil]
      def parse(url_string)
        return nil if url_string.nil?
        ptr, len = FFI.str_to_ptr(url_string)

        # UrlResult = { i32 status, UrlComponents { scheme, scheme_len, host, host_len,
        #   u16 port, bool has_port, path, path_len, query, query_len, fragment, fragment_len } }
        # UrlComponents has 6 string fields (ptr+len each) + port(u16) + has_port(bool)
        # Total: 4(status) + 4pad + (8+8)*5 + 2 + 1 + 5pad + 8 + 8 = complex
        # Approximate: 4 + 4 + 8+8 + 8+8 + 2+1+5 + 8+8 + 8+8 + 8+8 = ~100 bytes
        # Use generous allocation
        buf_size = 112
        buf = Fiddle::Pointer.malloc(buf_size, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_url_parse"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, ptr, len)

        packed = buf.to_str(buf_size)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        # UrlComponents layout on 64-bit (after status + 4pad = offset 8):
        # 8: scheme ptr (8 bytes)
        # 16: scheme_len (8 bytes)
        # 24: host ptr (8 bytes)
        # 32: host_len (8 bytes)
        # 40: port (u16, 2 bytes)
        # 42: has_port (bool, 1 byte)
        # 43: 5 padding
        # 48: path ptr (8 bytes)
        # 56: path_len (8 bytes)
        # 64: query ptr (8 bytes)
        # 72: query_len (8 bytes)
        # 80: fragment ptr (8 bytes)
        # 88: fragment_len (8 bytes)
        # Total component: 88 bytes from offset 8 = 96 total

        offset = 8 # After status + padding

        scheme = read_ffi_string(packed, offset)
        host = read_ffi_string(packed, offset + 16)
        port_val = packed[offset + 32, 2].unpack1("S") # u16
        has_port = packed[offset + 34, 1].unpack1("C") != 0
        path = read_ffi_string(packed, offset + 40)
        query = read_ffi_string(packed, offset + 56)
        fragment = read_ffi_string(packed, offset + 72)

        # Free the UrlComponents strings via proven_url_free
        begin
          free_fn = Fiddle::Function.new(
            FFI.handler["proven_url_free"],
            [Fiddle::TYPE_VOIDP],
            Fiddle::TYPE_VOID
          )
          # Point to the UrlComponents part (offset 8 in buf)
          components_ptr = buf + offset
          free_fn.call(components_ptr)
        rescue Fiddle::DLError
          # Best effort free
        end

        UrlComponents.new(
          scheme: scheme,
          host: host,
          port: has_port ? port_val : nil,
          path: path,
          query: query,
          fragment: fragment
        )
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Encode a string for safe URL inclusion.
      # Returns nil on error.
      #
      # @param value [String]
      # @return [String, nil]
      def encode(value)
        return nil if value.nil?
        ptr, len = FFI.str_to_ptr(value)
        result = FFI.invoke_string_result(
          "proven_http_url_encode",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      end

      # Decode a URL-encoded string.
      # Returns nil on error.
      #
      # @param value [String]
      # @return [String, nil]
      def decode(value)
        return nil if value.nil?
        ptr, len = FFI.str_to_ptr(value)
        result = FFI.invoke_string_result(
          "proven_http_url_decode",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      end

      private

      # Read a null-terminated C string from a packed struct at a given offset.
      # The struct has: ptr (8 bytes) + len (8 bytes).
      #
      # @param packed [String] packed binary data
      # @param offset [Integer] offset to the ptr field
      # @return [String, nil]
      def read_ffi_string(packed, offset)
        ptr_val = packed[offset, 8].unpack1("Q")
        length = packed[offset + 8, 8].unpack1("Q")
        return nil if ptr_val == 0 || length == 0

        str_ptr = Fiddle::Pointer.new(ptr_val)
        str_ptr.to_str(length).dup
      rescue TypeError
        nil
      end
    end
  end
end
