# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe circuit breaker pattern for fault tolerance.
  #
  # Provides automatic failure detection and recovery
  # to prevent cascading failures in distributed systems.
  module SafeCircuitBreaker
    # Circuit breaker states.
    module CircuitState
      CLOSED = :closed       # Normal operation - requests are allowed
      OPEN = :open           # Failing - requests are rejected
      HALF_OPEN = :half_open # Testing recovery - limited requests allowed
    end

    # Circuit breaker configuration.
    class CircuitConfig
      attr_accessor :failure_threshold, :success_threshold, :timeout, :half_open_max_calls

      def initialize(
        failure_threshold: 5,
        success_threshold: 2,
        timeout: 30,
        half_open_max_calls: 3
      )
        @failure_threshold = failure_threshold
        @success_threshold = success_threshold
        @timeout = timeout
        @half_open_max_calls = half_open_max_calls
      end
    end

    # Circuit breaker for fault tolerance.
    class CircuitBreaker
      attr_reader :config, :state

      def initialize(config = CircuitConfig.new)
        @config = config
        @state = CircuitState::CLOSED
        @failures = 0
        @successes = 0
        @last_failure_time = 0
        @half_open_calls = 0
      end

      # Update state based on current time.
      #
      # @param current_time [Integer]
      def update_state(current_time)
        return unless should_transition_to_half_open?(current_time)

        @state = CircuitState::HALF_OPEN
        @successes = 0
        @half_open_calls = 0
      end

      # Check if a request can be executed.
      #
      # @param current_time [Integer]
      # @return [Boolean]
      def can_execute?(current_time)
        update_state(current_time)

        case @state
        when CircuitState::CLOSED
          true
        when CircuitState::OPEN
          false
        when CircuitState::HALF_OPEN
          @half_open_calls < @config.half_open_max_calls
        end
      end

      # Record a successful call.
      def record_success
        case @state
        when CircuitState::CLOSED
          @failures = 0
        when CircuitState::HALF_OPEN
          @successes += 1
          if @successes >= @config.success_threshold
            @state = CircuitState::CLOSED
            @failures = 0
            @successes = 0
          end
        end
      end

      # Record a failed call.
      #
      # @param current_time [Integer]
      def record_failure(current_time)
        @last_failure_time = current_time

        case @state
        when CircuitState::CLOSED
          @failures += 1
          @state = CircuitState::OPEN if @failures >= @config.failure_threshold
        when CircuitState::HALF_OPEN
          @state = CircuitState::OPEN
          @failures += 1
        when CircuitState::OPEN
          @failures += 1
        end
      end

      # Record an attempt (for half-open tracking).
      def record_attempt
        @half_open_calls += 1 if @state == CircuitState::HALF_OPEN
      end

      # Execute with circuit breaker logic.
      #
      # @param current_time [Integer]
      # @param success [Boolean]
      # @return [Boolean] whether execution was allowed
      def execute(current_time, success)
        update_state(current_time)

        return false unless can_execute?(current_time)

        record_attempt

        if success
          record_success
        else
          record_failure(current_time)
        end

        true
      end

      # Check if circuit is healthy.
      #
      # @return [Boolean]
      def healthy?
        @state == CircuitState::CLOSED
      end

      # Time until circuit might close.
      #
      # @param current_time [Integer]
      # @return [Integer]
      def time_until_retry(current_time)
        return 0 unless @state == CircuitState::OPEN

        retry_time = @last_failure_time + @config.timeout
        current_time >= retry_time ? 0 : retry_time - current_time
      end

      # Force reset.
      def reset
        @state = CircuitState::CLOSED
        @failures = 0
        @successes = 0
        @half_open_calls = 0
      end

      private

      def should_transition_to_half_open?(current_time)
        @state == CircuitState::OPEN &&
          current_time >= @last_failure_time + @config.timeout
      end
    end

    # Create a new circuit breaker with default configuration.
    #
    # @return [CircuitBreaker]
    def self.circuit_breaker
      CircuitBreaker.new
    end

    # Create a new circuit breaker with custom configuration.
    #
    # @param config [CircuitConfig]
    # @return [CircuitBreaker]
    def self.circuit_breaker_with_config(config)
      CircuitBreaker.new(config)
    end
  end
end
