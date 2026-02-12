# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe bounded queue operations.
  #
  # Provides FIFO queues and priority queues with bounded capacity
  # and safe operations.
  module SafeQueue
    # Bounded FIFO queue with safe operations.
    class BoundedQueue
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

      def remaining
        [@capacity - @len, 0].max
      end

      # Enqueue an element (fails if full).
      #
      # @param value [Object]
      # @return [Result]
      def enqueue(value)
        return Result.error(OutOfRangeError.new("Queue full")) if full?

        @data[@tail] = value
        @tail = (@tail + 1) % @capacity
        @len += 1
        Result.ok(nil)
      end

      # Enqueue, dropping oldest if full.
      #
      # @param value [Object]
      def enqueue_drop_oldest(value)
        dequeue if full?
        enqueue(value)
      end

      # Dequeue an element (fails if empty).
      #
      # @return [Result]
      def dequeue
        return Result.error(OutOfRangeError.new("Queue empty")) if empty?

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
        return Result.error(OutOfRangeError.new("Queue empty")) if empty?

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

    # Priority queue (min-heap).
    class PriorityQueue
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

      # Push an element.
      #
      # @param value [Comparable]
      # @return [Result]
      def push(value)
        return Result.error(OutOfRangeError.new("Queue full")) if full?

        @data.push(value)
        sift_up(@data.length - 1)
        Result.ok(nil)
      end

      # Pop the minimum element.
      #
      # @return [Result]
      def pop
        return Result.error(OutOfRangeError.new("Queue empty")) if empty?

        result = @data[0]
        last = @data.pop
        unless @data.empty?
          @data[0] = last
          sift_down(0)
        end
        Result.ok(result)
      end

      # Peek at minimum without removing.
      #
      # @return [Result]
      def peek
        return Result.error(OutOfRangeError.new("Queue empty")) if empty?

        Result.ok(@data[0])
      end

      private

      def sift_up(idx)
        while idx.positive?
          parent = (idx - 1) / 2
          break if @data[idx] >= @data[parent]

          @data[idx], @data[parent] = @data[parent], @data[idx]
          idx = parent
        end
      end

      def sift_down(idx)
        loop do
          left = 2 * idx + 1
          right = 2 * idx + 2
          smallest = idx

          smallest = left if left < @data.length && @data[left] < @data[smallest]
          smallest = right if right < @data.length && @data[right] < @data[smallest]

          break if smallest == idx

          @data[idx], @data[smallest] = @data[smallest], @data[idx]
          idx = smallest
        end
      end
    end

    # Create a new bounded queue.
    #
    # @param capacity [Integer]
    # @return [BoundedQueue]
    def self.bounded_queue(capacity)
      BoundedQueue.new(capacity)
    end

    # Create a new priority queue.
    #
    # @param capacity [Integer]
    # @return [PriorityQueue]
    def self.priority_queue(capacity)
      PriorityQueue.new(capacity)
    end
  end
end
