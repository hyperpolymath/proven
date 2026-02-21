# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe floating point operations with validation.
  #
  # Provides validated float operations with protection against
  # NaN, Infinity, and precision issues.
  module SafeFloat
    # Default tolerance for comparisons
    DEFAULT_EPSILON = 1e-10

    # Check if a float is finite (not NaN or Infinity).
    #
    # @param value [Float]
    # @return [Boolean]
    def self.finite?(value)
      value.is_a?(Numeric) && value.to_f.finite?
    end

    # Check if value is NaN.
    #
    # @param value [Float]
    # @return [Boolean]
    def self.nan?(value)
      value.is_a?(Float) && value.nan?
    end

    # Check if value is Infinity.
    #
    # @param value [Float]
    # @return [Boolean]
    def self.infinite?(value)
      value.is_a?(Float) && value.infinite?
    end

    # Safely parse string to float.
    #
    # @param string [String]
    # @return [Result]
    def self.parse(string)
      return Result.error(InvalidInputError.new("Input cannot be nil")) if string.nil?

      trimmed = string.to_s.strip
      return Result.error(InvalidInputError.new("Input cannot be empty")) if trimmed.empty?

      begin
        value = Float(trimmed)
        if finite?(value)
          Result.ok(value)
        else
          Result.error(InvalidFormatError.new("Parsed value is not finite"))
        end
      rescue ArgumentError
        Result.error(InvalidFormatError.new("Cannot parse as float"))
      end
    end

    # Compare two floats with tolerance.
    #
    # @param a [Float]
    # @param b [Float]
    # @param epsilon [Float]
    # @return [Boolean]
    def self.approximately_equal?(a, b, epsilon: DEFAULT_EPSILON)
      return false unless finite?(a) && finite?(b)

      (a - b).abs <= epsilon
    end

    # Safe division that returns nil for division by zero.
    #
    # @param numerator [Float]
    # @param denominator [Float]
    # @return [Float, nil]
    def self.div(numerator, denominator)
      return nil if denominator.zero?

      result = numerator.to_f / denominator.to_f
      finite?(result) ? result : nil
    end

    # Clamp a float to a range.
    #
    # @param value [Float]
    # @param min [Float]
    # @param max [Float]
    # @return [Float]
    def self.clamp(value, min, max)
      [[value, min].max, max].min
    end

    # Round to specified decimal places.
    #
    # @param value [Float]
    # @param decimals [Integer]
    # @return [Float]
    def self.round(value, decimals)
      return nil unless finite?(value)

      value.round(decimals)
    end

    # Truncate to specified decimal places.
    #
    # @param value [Float]
    # @param decimals [Integer]
    # @return [Float]
    def self.truncate(value, decimals)
      return nil unless finite?(value)

      multiplier = 10**decimals
      (value * multiplier).truncate.to_f / multiplier
    end

    # Linear interpolation between two values.
    #
    # @param a [Float] start value
    # @param b [Float] end value
    # @param t [Float] interpolation factor (0-1)
    # @return [Float]
    def self.lerp(a, b, t)
      a + (b - a) * clamp(t, 0.0, 1.0)
    end

    # Inverse linear interpolation.
    #
    # @param a [Float] start value
    # @param b [Float] end value
    # @param value [Float] value to find t for
    # @return [Float, nil]
    def self.inverse_lerp(a, b, value)
      return nil if (b - a).abs < DEFAULT_EPSILON

      clamp((value - a) / (b - a), 0.0, 1.0)
    end

    # Remap a value from one range to another.
    #
    # @param value [Float]
    # @param from_min [Float]
    # @param from_max [Float]
    # @param to_min [Float]
    # @param to_max [Float]
    # @return [Float]
    def self.remap(value, from_min, from_max, to_min, to_max)
      t = inverse_lerp(from_min, from_max, value)
      return nil if t.nil?

      lerp(to_min, to_max, t)
    end

    # Safe square root (returns nil for negative).
    #
    # @param value [Float]
    # @return [Float, nil]
    def self.sqrt(value)
      return nil if value.negative?

      Math.sqrt(value)
    end

    # Safe logarithm (returns nil for non-positive).
    #
    # @param value [Float]
    # @param base [Float]
    # @return [Float, nil]
    def self.log(value, base: Math::E)
      return nil if value <= 0 || base <= 0 || base == 1

      Math.log(value) / Math.log(base)
    end

    # Convert to percentage string.
    #
    # @param value [Float]
    # @param decimals [Integer]
    # @return [String]
    def self.to_percentage(value, decimals: 2)
      return "N/A" unless finite?(value)

      format("%.#{decimals}f%%", value * 100)
    end

    # Sign of a number (-1, 0, or 1).
    #
    # @param value [Float]
    # @return [Integer]
    def self.sign(value)
      return 0 if value.zero?

      value.positive? ? 1 : -1
    end

    # Check if value is between bounds (inclusive).
    #
    # @param value [Float]
    # @param min [Float]
    # @param max [Float]
    # @return [Boolean]
    def self.between?(value, min, max)
      value >= min && value <= max
    end
  end
end
