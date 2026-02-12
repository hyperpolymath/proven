# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe rate limiting algorithms.
  #
  # Provides token bucket, sliding window, and fixed window
  # rate limiters for controlling request rates.
  module SafeRateLimiter
    # Result of a rate limit check.
    class RateLimitResult
      attr_reader :retry_after

      def initialize(allowed, retry_after = nil)
        @allowed = allowed
        @retry_after = retry_after
      end

      def allowed?
        @allowed
      end

      def denied?
        !@allowed
      end

      def self.allowed
        new(true)
      end

      def self.denied(retry_after)
        new(false, retry_after)
      end
    end

    # Token bucket rate limiter.
    class TokenBucket
      attr_reader :capacity, :refill_rate

      def initialize(capacity, refill_rate)
        @capacity = capacity
        @tokens = capacity
        @refill_rate = refill_rate
        @last_refill = 0
      end

      # Refill tokens based on elapsed time.
      #
      # @param current_time [Integer]
      def refill(current_time)
        elapsed = [current_time - @last_refill, 0].max
        new_tokens = @refill_rate * elapsed
        @tokens = [@capacity, @tokens + new_tokens].min
        @last_refill = current_time
      end

      # Try to acquire tokens.
      #
      # @param count [Integer]
      # @param current_time [Integer]
      # @return [RateLimitResult]
      def try_acquire(count, current_time)
        refill(current_time)

        if @tokens >= count
          @tokens -= count
          return RateLimitResult.allowed
        end

        needed = count - @tokens
        wait_time = (needed + @refill_rate - 1) / @refill_rate
        RateLimitResult.denied(wait_time)
      end

      # Check if request would be allowed (without consuming).
      #
      # @param count [Integer]
      # @param current_time [Integer]
      # @return [Boolean]
      def would_allow?(count, current_time)
        tokens_copy = @tokens
        last_refill_copy = @last_refill

        refill(current_time)
        result = @tokens >= count

        @tokens = tokens_copy
        @last_refill = last_refill_copy
        result
      end

      # Get current token count.
      #
      # @param current_time [Integer]
      # @return [Integer]
      def current_tokens(current_time)
        refill(current_time)
        @tokens
      end
    end

    # Sliding window rate limiter.
    class SlidingWindow
      attr_reader :max_requests, :window_size

      def initialize(max_requests, window_size)
        @max_requests = max_requests
        @window_size = window_size
        @requests = []
      end

      # Try to make a request.
      #
      # @param current_time [Integer]
      # @return [RateLimitResult]
      def try_request(current_time)
        prune(current_time)

        if @requests.length < @max_requests
          @requests.push(current_time)
          return RateLimitResult.allowed
        end

        return RateLimitResult.denied(@window_size) if @requests.empty?

        oldest = @requests.min || current_time
        retry_after = [@window_size - (current_time - oldest), 0].max
        RateLimitResult.denied(retry_after)
      end

      # Get current request count in window.
      #
      # @param current_time [Integer]
      # @return [Integer]
      def current_count(current_time)
        prune(current_time)
        @requests.length
      end

      # Get remaining allowed requests.
      #
      # @param current_time [Integer]
      # @return [Integer]
      def remaining(current_time)
        prune(current_time)
        [@max_requests - @requests.length, 0].max
      end

      private

      def prune(current_time)
        cutoff = current_time - @window_size
        @requests.reject! { |ts| ts < cutoff }
      end
    end

    # Fixed window counter.
    class FixedWindow
      attr_reader :max_requests, :window_size

      def initialize(max_requests, window_size)
        @max_requests = max_requests
        @window_size = window_size
        @window_start = 0
        @count = 0
      end

      # Try to make a request.
      #
      # @param current_time [Integer]
      # @return [RateLimitResult]
      def try_request(current_time)
        window_end = @window_start + @window_size

        # New window?
        if current_time >= window_end
          @window_start = (current_time / @window_size) * @window_size
          @count = 0
        end

        if @count < @max_requests
          @count += 1
          return RateLimitResult.allowed
        end

        retry_after = [@window_start + @window_size - current_time, 0].max
        RateLimitResult.denied(retry_after)
      end

      # Get remaining allowed requests in current window.
      #
      # @return [Integer]
      def remaining
        [@max_requests - @count, 0].max
      end
    end

    # Create a new token bucket rate limiter.
    #
    # @param capacity [Integer]
    # @param refill_rate [Integer]
    # @return [TokenBucket]
    def self.token_bucket(capacity, refill_rate)
      TokenBucket.new(capacity, refill_rate)
    end

    # Create a new sliding window rate limiter.
    #
    # @param max_requests [Integer]
    # @param window_size [Integer]
    # @return [SlidingWindow]
    def self.sliding_window(max_requests, window_size)
      SlidingWindow.new(max_requests, window_size)
    end

    # Create a new fixed window rate limiter.
    #
    # @param max_requests [Integer]
    # @param window_size [Integer]
    # @return [FixedWindow]
    def self.fixed_window(max_requests, window_size)
      FixedWindow.new(max_requests, window_size)
    end
  end
end
