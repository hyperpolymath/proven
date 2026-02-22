# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe arithmetic operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeMath
    class << self
      # Safely divide two 64-bit integers.
      # Returns nil on division by zero or error.
      #
      # @param numerator [Integer]
      # @param denominator [Integer]
      # @return [Integer, nil]
      def div(numerator, denominator)
        result = FFI.invoke_int_result(
          "proven_math_div",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [numerator, denominator]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Safely compute modulo of two 64-bit integers.
      # Returns nil on division by zero or error.
      #
      # @param numerator [Integer]
      # @param denominator [Integer]
      # @return [Integer, nil]
      def mod(numerator, denominator)
        result = FFI.invoke_int_result(
          "proven_math_mod",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [numerator, denominator]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Checked addition of two 64-bit integers.
      # Returns nil on overflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer, nil]
      def add(a, b)
        result = FFI.invoke_int_result(
          "proven_math_add_checked",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [a, b]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Checked subtraction of two 64-bit integers.
      # Returns nil on underflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer, nil]
      def sub(a, b)
        result = FFI.invoke_int_result(
          "proven_math_sub_checked",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [a, b]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Checked multiplication of two 64-bit integers.
      # Returns nil on overflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer, nil]
      def mul(a, b)
        result = FFI.invoke_int_result(
          "proven_math_mul_checked",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [a, b]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Safe absolute value (handles MIN_INT correctly).
      # Returns nil on error (e.g., |MIN_INT| overflows).
      #
      # @param n [Integer]
      # @return [Integer, nil]
      def abs(n)
        result = FFI.invoke_int_result(
          "proven_math_abs_safe",
          [Fiddle::TYPE_LONG_LONG],
          [n]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Clamp value to range [lo, hi].
      #
      # @param lo [Integer]
      # @param hi [Integer]
      # @param value [Integer]
      # @return [Integer, nil]
      def clamp(lo, hi, value)
        FFI.invoke_i64(
          "proven_math_clamp",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_LONG_LONG],
          [lo, hi, value]
        )
      end

      # Integer power with overflow checking.
      # Returns nil on overflow.
      #
      # @param base [Integer]
      # @param exp [Integer]
      # @return [Integer, nil]
      def pow(base, exp)
        result = FFI.invoke_int_result(
          "proven_math_pow_checked",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_INT],
          [base, exp]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end
    end
  end
end
