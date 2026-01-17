# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe buffer operations with bounds checking.
  #
  # Provides bounded buffers and ring buffers with safe operations
  # that cannot overflow or underflow.
  module SafeBuffer
    # A bounded buffer with safe operations.
    class BoundedBuffer
      attr_reader :capacity

      def initialize(capacity)
        @data = []
        @capacity = capacity
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

      def remaining
        [@capacity - @data.length, 0].max
      end

      # Push an element (fails if full).
      #
      # @param value [Object]
      # @return [Result]
      def push(value)
        return Result.error(OutOfRangeError.new("Buffer full")) if full?

        @data.push(value)
        Result.ok(nil)
      end

      # Pop an element (fails if empty).
      #
      # @return [Result]
      def pop
        return Result.error(OutOfRangeError.new("Buffer empty")) if empty?

        Result.ok(@data.pop)
      end

      # Get element at index (fails if out of bounds).
      #
      # @param index [Integer]
      # @return [Result]
      def get(index)
        return Result.error(OutOfRangeError.new("Index out of bounds")) if index < 0 || index >= @data.length

        Result.ok(@data[index])
      end

      # Set element at index (fails if out of bounds).
      #
      # @param index [Integer]
      # @param value [Object]
      # @return [Result]
      def set(index, value)
        return Result.error(OutOfRangeError.new("Index out of bounds")) if index < 0 || index >= @data.length

        @data[index] = value
        Result.ok(nil)
      end

      # Clear all elements.
      def clear
        @data.clear
      end

      # Get copy of data as array.
      #
      # @return [Array]
      def to_a
        @data.dup
      end
    end

    # Ring buffer for FIFO operations.
    class RingBuffer
      attr_reader :capacity

      def initialize(capacity)
        @data = Array.new(capacity)
        @head = 0
        @tail = 0
        @len = 0
        @capacity = capacity
      end

      def length
        @len
      end

      alias size length

      def empty?
        @len.zero?
      end

      def full?
        @len >= @capacity
      end

      # Enqueue an element (fails if full).
      #
      # @param value [Object]
      # @return [Result]
      def enqueue(value)
        return Result.error(OutOfRangeError.new("Ring buffer full")) if full?

        @data[@tail] = value
        @tail = (@tail + 1) % @capacity
        @len += 1
        Result.ok(nil)
      end

      # Dequeue an element (fails if empty).
      #
      # @return [Result]
      def dequeue
        return Result.error(OutOfRangeError.new("Ring buffer empty")) if empty?

        value = @data[@head]
        @data[@head] = nil
        @head = (@head + 1) % @capacity
        @len -= 1
        Result.ok(value)
      end

      # Peek at front without removing.
      #
      # @return [Result]
      def peek
        return Result.error(OutOfRangeError.new("Ring buffer empty")) if empty?

        Result.ok(@data[@head])
      end

      # Clear all elements.
      def clear
        @data = Array.new(@capacity)
        @head = 0
        @tail = 0
        @len = 0
      end
    end

    # Create a new bounded buffer.
    #
    # @param capacity [Integer]
    # @return [BoundedBuffer]
    def self.bounded_buffer(capacity)
      BoundedBuffer.new(capacity)
    end

    # Create a new ring buffer.
    #
    # @param capacity [Integer]
    # @return [RingBuffer]
    def self.ring_buffer(capacity)
      RingBuffer.new(capacity)
    end
  end
end
