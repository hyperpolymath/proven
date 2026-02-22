# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe rate limiter operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.
# Rate limiters are opaque pointers managed by libproven.

module Proven
  module SafeRateLimiter
    class << self
      # Create a token bucket rate limiter.
      # Returns an opaque handle or nil on error.
      # Caller MUST call #free when done.
      #
      # @param capacity [Float]
      # @param refill_rate [Float]
      # @return [Fiddle::Pointer, nil]
      def create(capacity, refill_rate)
        FFI.invoke_ptr(
          "proven_rate_limiter_create",
          [Fiddle::TYPE_DOUBLE, Fiddle::TYPE_DOUBLE],
          [capacity.to_f, refill_rate.to_f]
        )
      end

      # Try to acquire tokens from the rate limiter.
      # Returns true if allowed, false if denied.
      #
      # @param limiter [Fiddle::Pointer]
      # @param tokens [Float]
      # @return [Boolean, nil]
      def try_acquire(limiter, tokens)
        return nil if limiter.nil?
        FFI.invoke_bool(
          "proven_rate_limiter_try_acquire",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_DOUBLE],
          [limiter, tokens.to_f]
        )
      end

      # Free a rate limiter handle.
      #
      # @param limiter [Fiddle::Pointer]
      def free(limiter)
        return if limiter.nil?
        FFI.invoke_void(
          "proven_rate_limiter_free",
          [Fiddle::TYPE_VOIDP],
          [limiter]
        )
      end
    end
  end
end
