# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "monitor"

module Proven
  # Safe monotonic sequence generation.
  #
  # Provides monotonically increasing counters and timestamps
  # that cannot go backwards.
  module SafeMonotonic
    # Monotonically increasing counter (thread-safe).
    class MonotonicCounter
      include MonitorMixin

      def initialize(initial = 0)
        super()
        @value = initial
      end

      # Get the current value.
      #
      # @return [Integer]
      def get
        synchronize { @value }
      end

      # Get and increment (returns the value before increment).
      #
      # @return [Integer]
      def next
        synchronize do
          current = @value
          @value += 1
          current
        end
      end

      # Increment by a specific amount.
      #
      # @param amount [Integer]
      # @return [Integer] previous value
      def advance(amount)
        synchronize do
          current = @value
          @value += amount
          current
        end
      end

      # Try to set a new value (only succeeds if new > current).
      #
      # @param new_value [Integer]
      # @return [Boolean]
      def try_set(new_value)
        synchronize do
          return false if new_value <= @value

          @value = new_value
          true
        end
      end

      # Ensure value is at least the given minimum.
      #
      # @param min [Integer]
      def ensure_at_least(min)
        synchronize do
          @value = min if @value < min
        end
      end
    end

    # High-water mark tracker (tracks maximum seen value).
    class HighWaterMark
      include MonitorMixin

      def initialize
        super()
        @value = 0
      end

      # Get the current high-water mark.
      #
      # @return [Integer]
      def get
        synchronize { @value }
      end

      # Update with a new value, returning true if it's a new maximum.
      #
      # @param value [Integer]
      # @return [Boolean]
      def update(value)
        synchronize do
          return false if value <= @value

          @value = value
          true
        end
      end
    end

    # Sequence ID generator with prefix.
    class SequenceGenerator
      def initialize(prefix)
        @counter = MonotonicCounter.new
        @prefix = prefix
      end

      # Generate the next ID.
      #
      # @return [String]
      def next_id
        "#{@prefix}-#{@counter.next}"
      end

      # Generate the next ID with padding.
      #
      # @param width [Integer]
      # @return [String]
      def next_id_padded(width)
        format("#{@prefix}-%0#{width}d", @counter.next)
      end
    end

    # Epoch-based ID generator (timestamp + sequence).
    class EpochGenerator
      include MonitorMixin

      def initialize(epoch)
        super()
        @epoch = epoch
        @sequence = MonotonicCounter.new
        @last_timestamp = 0
      end

      # Generate an ID from timestamp and sequence.
      #
      # @param current_timestamp [Integer]
      # @return [Integer]
      def next_id(current_timestamp)
        synchronize do
          adjusted = [current_timestamp - @epoch, 0].max

          # Ensure monotonicity
          ts = [adjusted, @last_timestamp].max
          @last_timestamp = ts

          # Combine timestamp and sequence (16 bits for sequence)
          seq = @sequence.next & 0xFFFF
          (ts << 16) | seq
        end
      end
    end

    # Create a new monotonic counter.
    #
    # @param initial [Integer]
    # @return [MonotonicCounter]
    def self.counter(initial = 0)
      MonotonicCounter.new(initial)
    end

    # Create a new high-water mark tracker.
    #
    # @return [HighWaterMark]
    def self.high_water_mark
      HighWaterMark.new
    end

    # Create a new sequence generator.
    #
    # @param prefix [String]
    # @return [SequenceGenerator]
    def self.sequence_generator(prefix)
      SequenceGenerator.new(prefix)
    end

    # Create a new epoch-based ID generator.
    #
    # @param epoch [Integer]
    # @return [EpochGenerator]
    def self.epoch_generator(epoch)
      EpochGenerator.new(epoch)
    end
  end
end
