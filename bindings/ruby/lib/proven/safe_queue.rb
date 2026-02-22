# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe bounded queue operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.
# Queues are opaque pointers managed by libproven.

module Proven
  module SafeQueue
    class << self
      # Create a bounded queue with the given capacity.
      # Returns an opaque handle or nil on error.
      # Caller MUST call #free when done.
      #
      # @param capacity [Integer]
      # @return [Fiddle::Pointer, nil]
      def create(capacity)
        FFI.invoke_ptr(
          "proven_queue_create",
          [Fiddle::TYPE_SIZE_T],
          [capacity]
        )
      end

      # Push a value onto the queue.
      # Returns true on success, false if full.
      #
      # @param queue [Fiddle::Pointer]
      # @param value [Integer]
      # @return [Boolean, nil]
      def push(queue, value)
        return nil if queue.nil?
        FFI.invoke_bool(
          "proven_queue_push",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_LONG_LONG],
          [queue, value]
        )
      end

      # Pop a value from the queue.
      # Returns nil if empty or error.
      #
      # @param queue [Fiddle::Pointer]
      # @return [Integer, nil]
      def pop(queue)
        return nil if queue.nil?
        result = FFI.invoke_int_result(
          "proven_queue_pop",
          [Fiddle::TYPE_VOIDP],
          [queue]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Get current queue size.
      #
      # @param queue [Fiddle::Pointer]
      # @return [Integer, nil]
      def size(queue)
        return nil if queue.nil?
        fn = Fiddle::Function.new(
          FFI.handler["proven_queue_size"],
          [Fiddle::TYPE_VOIDP],
          Fiddle::TYPE_SIZE_T,
          need_gvl: true
        )
        fn.call(queue)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Free a queue handle.
      #
      # @param queue [Fiddle::Pointer]
      def free(queue)
        return if queue.nil?
        FFI.invoke_void(
          "proven_queue_free",
          [Fiddle::TYPE_VOIDP],
          [queue]
        )
      end
    end
  end
end
