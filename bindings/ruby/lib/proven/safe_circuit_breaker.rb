# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe circuit breaker operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.
# Circuit breakers are opaque pointers managed by libproven.

module Proven
  module SafeCircuitBreaker
    # Circuit breaker state enum values (must match ffi/zig/src/main.zig CircuitState).
    module State
      CLOSED    = 0
      OPEN      = 1
      HALF_OPEN = 2
    end

    STATE_NAMES = {
      State::CLOSED => :closed,
      State::OPEN => :open,
      State::HALF_OPEN => :half_open,
    }.freeze

    class << self
      # Create a circuit breaker.
      # Returns an opaque handle or nil on error.
      # Caller MUST call #free when done.
      #
      # @param failure_threshold [Integer]
      # @param success_threshold [Integer]
      # @param timeout_ms [Integer]
      # @return [Fiddle::Pointer, nil]
      def create(failure_threshold, success_threshold, timeout_ms)
        FFI.invoke_ptr(
          "proven_circuit_breaker_create",
          [Fiddle::TYPE_INT, Fiddle::TYPE_INT, Fiddle::TYPE_LONG_LONG],
          [failure_threshold, success_threshold, timeout_ms]
        )
      end

      # Check if the circuit breaker allows a request.
      #
      # @param cb [Fiddle::Pointer]
      # @return [Boolean, nil]
      def allow?(cb)
        return nil if cb.nil?
        FFI.invoke_bool(
          "proven_circuit_breaker_allow",
          [Fiddle::TYPE_VOIDP],
          [cb]
        )
      end

      # Record a successful call.
      #
      # @param cb [Fiddle::Pointer]
      def record_success(cb)
        return if cb.nil?
        FFI.invoke_void(
          "proven_circuit_breaker_success",
          [Fiddle::TYPE_VOIDP],
          [cb]
        )
      end

      # Record a failed call.
      #
      # @param cb [Fiddle::Pointer]
      def record_failure(cb)
        return if cb.nil?
        FFI.invoke_void(
          "proven_circuit_breaker_failure",
          [Fiddle::TYPE_VOIDP],
          [cb]
        )
      end

      # Get current state.
      # Returns :closed, :open, or :half_open.
      #
      # @param cb [Fiddle::Pointer]
      # @return [Symbol, nil]
      def state(cb)
        return nil if cb.nil?
        val = FFI.invoke_i32(
          "proven_circuit_breaker_state",
          [Fiddle::TYPE_VOIDP],
          [cb]
        )
        return nil if val.nil?
        STATE_NAMES.fetch(val, :closed)
      end

      # Free a circuit breaker handle.
      #
      # @param cb [Fiddle::Pointer]
      def free(cb)
        return if cb.nil?
        FFI.invoke_void(
          "proven_circuit_breaker_free",
          [Fiddle::TYPE_VOIDP],
          [cb]
        )
      end
    end
  end
end
