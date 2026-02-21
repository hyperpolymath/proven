# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe retry mechanisms with exponential backoff.
  #
  # Provides configurable retry strategies with jitter
  # for resilient distributed systems.
  module SafeRetry
    # Retry strategy configuration.
    class RetryConfig
      attr_accessor :max_attempts, :initial_delay_ms, :max_delay_ms, :multiplier, :use_jitter

      def initialize(
        max_attempts: 3,
        initial_delay_ms: 100,
        max_delay_ms: 10_000,
        multiplier: 2.0,
        use_jitter: true
      )
        @max_attempts = max_attempts
        @initial_delay_ms = initial_delay_ms
        @max_delay_ms = max_delay_ms
        @multiplier = multiplier
        @use_jitter = use_jitter
      end
    end

    # Retry state tracker.
    class RetryState
      attr_reader :config, :attempts

      def initialize(config = RetryConfig.new)
        @config = config
        @attempts = 0
        @current_delay_ms = config.initial_delay_ms
      end

      # Check if more retries are available.
      #
      # @return [Boolean]
      def should_retry?
        @attempts < @config.max_attempts
      end

      # Get current attempt number (0-indexed).
      #
      # @return [Integer]
      def current_attempt
        @attempts
      end

      # Get remaining attempts.
      #
      # @return [Integer]
      def remaining_attempts
        [@config.max_attempts - @attempts, 0].max
      end

      # Record an attempt and calculate next delay.
      #
      # @return [Integer, nil] delay in milliseconds or nil if no more retries
      def record_attempt
        return nil unless should_retry?

        @attempts += 1
        delay = @current_delay_ms

        # Calculate next delay with exponential backoff
        next_delay = (@current_delay_ms * @config.multiplier).to_i
        @current_delay_ms = [next_delay, @config.max_delay_ms].min

        delay
      end

      # Get delay with optional jitter.
      #
      # @param jitter_factor [Float]
      # @return [Integer]
      def get_delay_with_jitter(jitter_factor = 0.1)
        return @current_delay_ms unless @config.use_jitter

        # Simple deterministic jitter based on attempt count
        jitter = (@attempts * jitter_factor * @current_delay_ms).to_i
        jittered_delay = @current_delay_ms + (jitter % ((@current_delay_ms / 2) + 1))
        [jittered_delay, @config.max_delay_ms].min
      end

      # Reset the retry state.
      def reset
        @attempts = 0
        @current_delay_ms = @config.initial_delay_ms
      end
    end

    # Calculate exponential backoff delay.
    #
    # @param attempt [Integer]
    # @param base_ms [Integer]
    # @param max_ms [Integer]
    # @param multiplier [Float]
    # @return [Integer]
    def self.exponential_backoff(attempt, base_ms, max_ms, multiplier)
      delay = (base_ms * (multiplier**attempt)).to_i
      [delay, max_ms].min
    end

    # Calculate delay with full jitter (Amazon style).
    #
    # @param base_delay_ms [Integer]
    # @param seed [Integer]
    # @return [Integer]
    def self.full_jitter(base_delay_ms, seed)
      # Deterministic pseudo-random based on seed
      hash = (seed * 6_364_136_223_846_793_005 + 1) & 0xFFFFFFFFFFFFFFFF
      hash % (base_delay_ms + 1)
    end

    # Calculate delay with equal jitter.
    #
    # @param base_delay_ms [Integer]
    # @param seed [Integer]
    # @return [Integer]
    def self.equal_jitter(base_delay_ms, seed)
      half = base_delay_ms / 2
      hash = (seed * 6_364_136_223_846_793_005 + 1) & 0xFFFFFFFFFFFFFFFF
      half + (hash % (half + 1))
    end

    # Calculate delay with decorrelated jitter.
    #
    # @param prev_delay_ms [Integer]
    # @param base_ms [Integer]
    # @param cap_ms [Integer]
    # @param seed [Integer]
    # @return [Integer]
    def self.decorrelated_jitter(prev_delay_ms, base_ms, cap_ms, seed)
      hash = (seed * 6_364_136_223_846_793_005 + 1) & 0xFFFFFFFFFFFFFFFF
      range = [prev_delay_ms * 3 - base_ms, 0].max
      delay = base_ms + (hash % (range + 1))
      [delay, cap_ms].min
    end

    # Create a new retry state with default configuration.
    #
    # @return [RetryState]
    def self.retry_state
      RetryState.new
    end

    # Create a new retry state with custom configuration.
    #
    # @param config [RetryConfig]
    # @return [RetryState]
    def self.retry_state_with_config(config)
      RetryState.new(config)
    end

    # Execute a block with retries.
    #
    # @param config [RetryConfig]
    # @yield block to execute
    # @return [Result]
    def self.with_retries(config = RetryConfig.new)
      state = RetryState.new(config)
      last_error = nil

      while state.should_retry?
        delay = state.record_attempt
        begin
          result = yield
          return Result.ok(result)
        rescue StandardError => e
          last_error = e
          sleep(delay / 1000.0) if delay && state.should_retry?
        end
      end

      Result.error(last_error || ValidationError.new("All retries exhausted"))
    end
  end
end
