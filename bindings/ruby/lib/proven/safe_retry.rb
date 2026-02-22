# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe retry operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeRetry
    class << self
      # Calculate the delay for a given retry attempt using exponential backoff.
      # Returns delay in milliseconds, or nil on error.
      #
      # The config is passed as a struct: { u32 max_attempts, u64 initial_delay_ms,
      #   u64 max_delay_ms, f64 multiplier }
      #
      # @param max_attempts [Integer]
      # @param initial_delay_ms [Integer]
      # @param max_delay_ms [Integer]
      # @param multiplier [Float]
      # @param attempt [Integer]
      # @return [Integer, nil]
      def delay(max_attempts:, initial_delay_ms:, max_delay_ms:, multiplier:, attempt:)
        # RetryConfig = { u32 max_attempts, u64 initial_delay_ms, u64 max_delay_ms, f64 multiplier }
        # Pack the struct and pass by value. On x86_64, 32 bytes > 16, so sret.
        # Actually Fiddle can pass flattened struct fields as separate args.
        result = FFI.invoke_i64(
          "proven_retry_delay",
          [Fiddle::TYPE_INT, Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG,
           Fiddle::TYPE_DOUBLE, Fiddle::TYPE_INT],
          [max_attempts, initial_delay_ms, max_delay_ms, multiplier, attempt]
        )
        # The return is u64 - non-negative
        result
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Check if a retry should be attempted for the given attempt number.
      #
      # @param max_attempts [Integer]
      # @param initial_delay_ms [Integer]
      # @param max_delay_ms [Integer]
      # @param multiplier [Float]
      # @param attempt [Integer]
      # @return [Boolean, nil]
      def should_retry?(max_attempts:, initial_delay_ms:, max_delay_ms:, multiplier:, attempt:)
        FFI.invoke_bool(
          "proven_retry_should_retry",
          [Fiddle::TYPE_INT, Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG,
           Fiddle::TYPE_DOUBLE, Fiddle::TYPE_INT],
          [max_attempts, initial_delay_ms, max_delay_ms, multiplier, attempt]
        )
      rescue Fiddle::DLError, TypeError
        nil
      end
    end
  end
end
