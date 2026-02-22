# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe LRU cache operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.
# LRU caches are opaque pointers managed by libproven.

module Proven
  module SafeLru
    class << self
      # Create an LRU cache with the given capacity.
      # Returns an opaque handle or nil on error.
      # Caller MUST call #free when done.
      #
      # @param capacity [Integer]
      # @return [Fiddle::Pointer, nil]
      def create(capacity)
        FFI.invoke_ptr(
          "proven_lru_create",
          [Fiddle::TYPE_SIZE_T],
          [capacity]
        )
      end

      # Get a value by key. Returns nil if not found or error.
      #
      # @param cache [Fiddle::Pointer]
      # @param key [Integer] u64 key
      # @return [Integer, nil]
      def get(cache, key)
        return nil if cache.nil?
        result = FFI.invoke_int_result(
          "proven_lru_get",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_LONG_LONG],
          [cache, key]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Insert a key-value pair, evicting LRU if necessary.
      # Returns true on success, nil on error.
      #
      # @param cache [Fiddle::Pointer]
      # @param key [Integer] u64 key
      # @param value [Integer] i64 value
      # @return [Boolean, nil]
      def put(cache, key, value)
        return nil if cache.nil?
        status = FFI.invoke_i32(
          "proven_lru_put",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [cache, key, value]
        )
        status == FFI::STATUS_OK
      end

      # Free an LRU cache handle.
      #
      # @param cache [Fiddle::Pointer]
      def free(cache)
        return if cache.nil?
        FFI.invoke_void(
          "proven_lru_free",
          [Fiddle::TYPE_VOIDP],
          [cache]
        )
      end
    end
  end
end
