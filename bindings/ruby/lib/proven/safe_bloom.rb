# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "digest"

module Proven
  # Safe Bloom filter implementation.
  #
  # Provides probabilistic set membership testing with tunable
  # false positive rates.
  module SafeBloom
    # Bloom filter with configurable size and hash count.
    class BloomFilter
      attr_reader :size, :num_hashes

      def initialize(size, num_hashes)
        @bits = Array.new(size, false)
        @size = size
        @num_hashes = num_hashes
      end

      # Create with optimal parameters for expected items and false positive rate.
      #
      # @param expected_items [Integer]
      # @param false_positive_rate [Float]
      # @return [BloomFilter]
      def self.with_rate(expected_items, false_positive_rate)
        size = SafeBloom.optimal_size(expected_items, false_positive_rate)
        num_hashes = SafeBloom.optimal_hashes(size, expected_items)
        new(size, num_hashes)
      end

      # Insert an item into the filter.
      #
      # @param item [Object]
      def insert(item)
        @num_hashes.times do |i|
          hash = compute_hash(item, i)
          @bits[hash % @size] = true
        end
      end

      # Check if an item might be in the filter.
      # Returns false if definitely not present, true if possibly present.
      #
      # @param item [Object]
      # @return [Boolean]
      def contains?(item)
        @num_hashes.times do |i|
          hash = compute_hash(item, i)
          return false unless @bits[hash % @size]
        end
        true
      end

      alias include? contains?

      # Count set bits.
      #
      # @return [Integer]
      def count_ones
        @bits.count(true)
      end

      # Get fill ratio.
      #
      # @return [Float]
      def fill_ratio
        count_ones.to_f / @size
      end

      # Estimate false positive rate.
      #
      # @return [Float]
      def estimated_fpr
        fill_ratio**@num_hashes
      end

      # Clear the filter.
      def clear
        @bits.fill(false)
      end

      # Union of two filters (must be same size).
      #
      # @param other [BloomFilter]
      def union_with(other)
        return unless @size == other.size

        @bits.each_with_index do |_, i|
          @bits[i] = @bits[i] || other.bits[i]
        end
      end

      # Intersection of two filters (must be same size).
      #
      # @param other [BloomFilter]
      def intersect_with(other)
        return unless @size == other.size

        @bits.each_with_index do |_, i|
          @bits[i] = @bits[i] && other.bits[i]
        end
      end

      protected

      attr_reader :bits

      private

      def compute_hash(item, seed)
        h1 = Digest::SHA256.hexdigest(item.to_s).to_i(16)
        h2 = Digest::MD5.hexdigest(seed.to_s).to_i(16)
        (h1 + seed * h2).abs
      end
    end

    # Calculate optimal size for given parameters.
    #
    # @param expected_items [Integer]
    # @param false_positive_rate [Float]
    # @return [Integer]
    def self.optimal_size(expected_items, false_positive_rate)
      n = expected_items.to_f
      p = false_positive_rate
      ln2_sq = 0.4804530139182014 # ln(2)^2
      m = -(n * Math.log(p)) / ln2_sq
      m.ceil
    end

    # Calculate optimal hash count.
    #
    # @param filter_size [Integer]
    # @param expected_items [Integer]
    # @return [Integer]
    def self.optimal_hashes(filter_size, expected_items)
      m = filter_size.to_f
      n = expected_items.to_f
      k = (m / n) * 0.6931471805599453 # ln(2)
      [k.ceil, 1].max
    end

    # Create a new Bloom filter.
    #
    # @param size [Integer]
    # @param num_hashes [Integer]
    # @return [BloomFilter]
    def self.bloom_filter(size, num_hashes)
      BloomFilter.new(size, num_hashes)
    end

    # Create a Bloom filter with optimal parameters.
    #
    # @param expected_items [Integer]
    # @param false_positive_rate [Float]
    # @return [BloomFilter]
    def self.bloom_filter_with_rate(expected_items, false_positive_rate)
      BloomFilter.with_rate(expected_items, false_positive_rate)
    end
  end
end
