# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe floating-point operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeFloat
    class << self
      # Check if a float is finite (not NaN or Infinity).
      #
      # @param value [Float]
      # @return [Boolean, nil]
      def finite?(value)
        FFI.invoke_bool(
          "proven_float_is_finite",
          [Fiddle::TYPE_DOUBLE],
          [value.to_f]
        )
      end

      # Check if value is NaN.
      #
      # @param value [Float]
      # @return [Boolean, nil]
      def nan?(value)
        FFI.invoke_bool(
          "proven_float_is_nan",
          [Fiddle::TYPE_DOUBLE],
          [value.to_f]
        )
      end

      # Safe division. Returns nil for division by zero or non-finite result.
      #
      # @param numerator [Float]
      # @param denominator [Float]
      # @return [Float, nil]
      def div(numerator, denominator)
        result = FFI.invoke_float_result(
          "proven_float_div",
          [Fiddle::TYPE_DOUBLE, Fiddle::TYPE_DOUBLE],
          [numerator.to_f, denominator.to_f]
        )
        return nil unless result
        status, value = result
        status == FFI::STATUS_OK ? value : nil
      end

      # Safe square root (returns nil for negative input).
      #
      # @param value [Float]
      # @return [Float, nil]
      def sqrt(value)
        result = FFI.invoke_float_result(
          "proven_float_sqrt",
          [Fiddle::TYPE_DOUBLE],
          [value.to_f]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end

      # Safe natural logarithm (returns nil for non-positive input).
      #
      # @param value [Float]
      # @return [Float, nil]
      def ln(value)
        result = FFI.invoke_float_result(
          "proven_float_ln",
          [Fiddle::TYPE_DOUBLE],
          [value.to_f]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end
    end
  end
end
