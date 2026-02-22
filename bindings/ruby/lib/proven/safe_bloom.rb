# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe Bloom filter operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.
# Bloom filters are opaque pointers managed by libproven.

module Proven
  module SafeBloom
    class << self
      # Create a Bloom filter with optimal parameters.
      # Returns an opaque handle or nil on error.
      # Caller MUST call #free when done.
      #
      # @param expected_elements [Integer]
      # @param false_positive_rate [Float]
      # @return [Fiddle::Pointer, nil]
      def create(expected_elements, false_positive_rate)
        FFI.invoke_ptr(
          "proven_bloom_create",
          [Fiddle::TYPE_SIZE_T, Fiddle::TYPE_DOUBLE],
          [expected_elements, false_positive_rate]
        )
      end

      # Add an item to the Bloom filter.
      #
      # @param filter [Fiddle::Pointer]
      # @param item [String]
      def add(filter, item)
        return if filter.nil? || item.nil?
        ptr, len = FFI.str_to_ptr(item.to_s)
        FFI.invoke_void(
          "proven_bloom_add",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [filter, ptr, len]
        )
      end

      # Check if an item might be in the Bloom filter.
      # Returns false if definitely not present, true if possibly present.
      #
      # @param filter [Fiddle::Pointer]
      # @param item [String]
      # @return [Boolean, nil]
      def contains?(filter, item)
        return nil if filter.nil? || item.nil?
        ptr, len = FFI.str_to_ptr(item.to_s)
        FFI.invoke_bool(
          "proven_bloom_contains",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [filter, ptr, len]
        )
      end

      # Free a Bloom filter handle.
      #
      # @param filter [Fiddle::Pointer]
      def free(filter)
        return if filter.nil?
        FFI.invoke_void(
          "proven_bloom_free",
          [Fiddle::TYPE_VOIDP],
          [filter]
        )
      end
    end
  end
end
