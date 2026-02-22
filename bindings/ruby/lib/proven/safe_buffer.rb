# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe buffer operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.
# Buffers are opaque pointers managed by libproven.

module Proven
  module SafeBuffer
    class << self
      # Create a bounded buffer with the given capacity.
      # Returns an opaque handle (Fiddle::Pointer) or nil on error.
      # Caller MUST call #free when done.
      #
      # @param capacity [Integer]
      # @return [Fiddle::Pointer, nil]
      def create(capacity)
        # BufferResult = { i32 status, ptr buffer } = 16 bytes
        buf = Fiddle::Pointer.malloc(16, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_buffer_create"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, capacity)

        packed = buf.to_str(16)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        ptr_val = packed[8, 8].unpack1("Q")
        return nil if ptr_val == 0
        Fiddle::Pointer.new(ptr_val)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Append data to a buffer.
      # Returns true on success, nil on error.
      #
      # @param buffer [Fiddle::Pointer] opaque buffer handle
      # @param data [String] data to append
      # @return [Boolean, nil]
      def append(buffer, data)
        return nil if buffer.nil? || data.nil?
        ptr, len = FFI.str_to_ptr(data)
        status = FFI.invoke_i32(
          "proven_buffer_append",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [buffer, ptr, len]
        )
        status == FFI::STATUS_OK
      end

      # Free a buffer handle.
      #
      # @param buffer [Fiddle::Pointer] opaque buffer handle
      def free(buffer)
        return if buffer.nil?
        FFI.invoke_void(
          "proven_buffer_free",
          [Fiddle::TYPE_VOIDP],
          [buffer]
        )
      end
    end
  end
end
