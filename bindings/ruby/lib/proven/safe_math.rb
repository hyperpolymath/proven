# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe arithmetic operations that cannot crash or overflow unexpectedly.
  module SafeMath
    # 64-bit integer limits for bounded operations
    MAX_INT = 2**63 - 1
    MIN_INT = -(2**63)

    class << self
      # Safely divide two integers, returning nil on division by zero.
      #
      # @param numerator [Integer]
      # @param denominator [Integer]
      # @return [Integer, nil]
      def div(numerator, denominator)
        return nil if denominator.zero?

        numerator / denominator
      end

      # Safely compute modulo, returning nil on division by zero.
      #
      # @param numerator [Integer]
      # @param denominator [Integer]
      # @return [Integer, nil]
      def mod(numerator, denominator)
        return nil if denominator.zero?

        numerator % denominator
      end

      # Safely add two integers, returning nil on overflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer, nil]
      def add(a, b)
        result = a + b
        return nil if result > MAX_INT || result < MIN_INT

        result
      end

      # Safely subtract two integers, returning nil on overflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer, nil]
      def sub(a, b)
        result = a - b
        return nil if result > MAX_INT || result < MIN_INT

        result
      end

      # Safely multiply two integers, returning nil on overflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer, nil]
      def mul(a, b)
        result = a * b
        return nil if result > MAX_INT || result < MIN_INT

        result
      end

      # Checked add that raises on overflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer]
      # @raise [RangeError] on overflow
      def checked_add(a, b)
        result = add(a, b)
        raise RangeError, "integer overflow" if result.nil?

        result
      end

      # Checked subtract that raises on overflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer]
      # @raise [RangeError] on overflow
      def checked_sub(a, b)
        result = sub(a, b)
        raise RangeError, "integer overflow" if result.nil?

        result
      end

      # Checked multiply that raises on overflow.
      #
      # @param a [Integer]
      # @param b [Integer]
      # @return [Integer]
      # @raise [RangeError] on overflow
      def checked_mul(a, b)
        result = mul(a, b)
        raise RangeError, "integer overflow" if result.nil?

        result
      end
    end
  end
end
