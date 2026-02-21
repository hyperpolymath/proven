# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe LRU (Least Recently Used) cache implementation.
  #
  # Provides a bounded cache with automatic eviction of
  # least recently used entries.
  module SafeLru
    # LRU cache with bounded capacity.
    class LruCache
      attr_reader :capacity

      def initialize(capacity)
        @capacity = capacity
        @data = {}
        @order = [] # Most recent at end, least recent at front
      end

      def length
        @data.length
      end

      alias size length

      def empty?
        @data.empty?
      end

      def full?
        @data.length >= @capacity
      end

      # Get a value by key, marking it as recently used.
      #
      # @param key [Object]
      # @return [Object, nil]
      def get(key)
        return nil unless @data.key?(key)

        # Move to end (most recent)
        @order.delete(key)
        @order.push(key)
        @data[key]
      end

      # Get a value without updating recency.
      #
      # @param key [Object]
      # @return [Object, nil]
      def peek(key)
        @data[key]
      end

      # Insert a key-value pair, evicting LRU if necessary.
      #
      # @param key [Object]
      # @param value [Object]
      # @return [Object, nil] evicted value if any
      def put(key, value)
        evicted = nil

        # If key exists, update and move to front
        if @data.key?(key)
          old_value = @data[key]
          @data[key] = value
          @order.delete(key)
          @order.push(key)
          return old_value
        end

        # Need to evict?
        evicted = evict_lru if full?

        @data[key] = value
        @order.push(key)

        evicted
      end

      # Remove a key from the cache.
      #
      # @param key [Object]
      # @return [Object, nil]
      def remove(key)
        return nil unless @data.key?(key)

        @order.delete(key)
        @data.delete(key)
      end

      alias delete remove

      # Check if key exists.
      #
      # @param key [Object]
      # @return [Boolean]
      def contains?(key)
        @data.key?(key)
      end

      alias key? contains?
      alias has_key? contains?

      # Clear all entries.
      def clear
        @data.clear
        @order.clear
      end

      # Get all keys in LRU order (least recent first).
      #
      # @return [Array]
      def keys
        @order.dup
      end

      # Get all values in LRU order (least recent first).
      #
      # @return [Array]
      def values
        @order.map { |k| @data[k] }
      end

      # Iterate over key-value pairs in LRU order.
      #
      # @yield [key, value]
      def each(&block)
        @order.each do |key|
          block.call(key, @data[key])
        end
      end

      private

      def evict_lru
        return nil if @order.empty?

        lru_key = @order.shift
        @data.delete(lru_key)
      end
    end

    # Create a new LRU cache.
    #
    # @param capacity [Integer]
    # @return [LruCache]
    def self.lru_cache(capacity)
      LruCache.new(capacity)
    end
  end
end
