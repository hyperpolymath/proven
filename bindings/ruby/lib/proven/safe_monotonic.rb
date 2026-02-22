# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe monotonic counter operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.
# Monotonic counters are opaque pointers managed by libproven.

module Proven
  module SafeMonotonic
    class << self
      # Create a monotonic counter.
      # Returns an opaque handle or nil on error.
      # Caller MUST call #free when done.
      #
      # @param initial [Integer] initial value
      # @param max_value [Integer] maximum value before wrapping
      # @return [Fiddle::Pointer, nil]
      def create(initial, max_value)
        FFI.invoke_ptr(
          "proven_monotonic_create",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [initial, max_value]
        )
      end

      # Get the next value from the counter.
      # Returns nil if counter has reached max_value or error.
      #
      # @param counter [Fiddle::Pointer]
      # @return [Integer, nil]
      def next_value(counter)
        return nil if counter.nil?
        result = FFI.invoke_int_result(
          "proven_monotonic_next",
          [Fiddle::TYPE_VOIDP],
          [counter]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Free a monotonic counter handle.
      #
      # @param counter [Fiddle::Pointer]
      def free(counter)
        return if counter.nil?
        FFI.invoke_void(
          "proven_monotonic_free",
          [Fiddle::TYPE_VOIDP],
          [counter]
        )
      end
    end
  end
end
