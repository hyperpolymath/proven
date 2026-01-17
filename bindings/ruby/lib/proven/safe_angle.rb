# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe angle operations with degree/radian conversions.
  #
  # Provides angle types with automatic normalization and
  # safe trigonometric operations.
  module SafeAngle
    # Pi constant
    PI = Math::PI

    # Two pi
    TWO_PI = 2.0 * PI

    # Degrees to radians factor
    DEG_TO_RAD = PI / 180.0

    # Radians to degrees factor
    RAD_TO_DEG = 180.0 / PI

    # Angle in degrees.
    class Degrees
      attr_reader :value

      def initialize(value, normalize: true)
        @value = normalize ? SafeAngle.normalize_degrees(value) : value.to_f
      end

      def to_radians
        Radians.new(@value * DEG_TO_RAD, normalize: false)
      end

      def normalize
        Degrees.new(@value)
      end

      def normalize_signed
        v = SafeAngle.normalize_degrees(@value)
        v -= 360.0 if v >= 180.0
        Degrees.new(v, normalize: false)
      end

      def +(other)
        Degrees.new(@value + other.value)
      end

      def -(other)
        Degrees.new(@value - other.value)
      end

      def sin
        to_radians.sin
      end

      def cos
        to_radians.cos
      end

      def tan
        to_radians.tan
      end

      def ==(other)
        return false unless other.is_a?(Degrees)

        (SafeAngle.normalize_degrees(@value) - SafeAngle.normalize_degrees(other.value)).abs < 1e-9
      end
    end

    # Angle in radians.
    class Radians
      attr_reader :value

      def initialize(value, normalize: true)
        @value = normalize ? SafeAngle.normalize_radians(value) : value.to_f
      end

      def to_degrees
        Degrees.new(@value * RAD_TO_DEG, normalize: false)
      end

      def normalize
        Radians.new(@value)
      end

      def +(other)
        Radians.new(@value + other.value)
      end

      def -(other)
        Radians.new(@value - other.value)
      end

      def sin
        Math.sin(@value)
      end

      def cos
        Math.cos(@value)
      end

      def tan
        Math.tan(@value)
      end

      def ==(other)
        return false unless other.is_a?(Radians)

        (SafeAngle.normalize_radians(@value) - SafeAngle.normalize_radians(other.value)).abs < 1e-9
      end
    end

    # Normalize degrees to [0, 360).
    #
    # @param deg [Float]
    # @return [Float]
    def self.normalize_degrees(deg)
      result = deg % 360.0
      result += 360.0 if result.negative?
      result
    end

    # Normalize radians to [0, 2pi).
    #
    # @param rad [Float]
    # @return [Float]
    def self.normalize_radians(rad)
      result = rad % TWO_PI
      result += TWO_PI if result.negative?
      result
    end

    # Convert degrees to radians.
    #
    # @param deg [Float]
    # @return [Float]
    def self.deg_to_rad(deg)
      deg * DEG_TO_RAD
    end

    # Convert radians to degrees.
    #
    # @param rad [Float]
    # @return [Float]
    def self.rad_to_deg(rad)
      rad * RAD_TO_DEG
    end

    # Calculate angle difference (shortest path).
    #
    # @param a [Float] first angle in degrees
    # @param b [Float] second angle in degrees
    # @return [Float] signed difference in degrees
    def self.angle_diff_degrees(a, b)
      diff = normalize_degrees(b - a)
      diff -= 360.0 if diff > 180.0
      diff
    end

    # Linear interpolation between angles.
    #
    # @param a [Float] start angle in degrees
    # @param b [Float] end angle in degrees
    # @param t [Float] interpolation factor (0-1)
    # @return [Float]
    def self.lerp_angle_degrees(a, b, t)
      diff = angle_diff_degrees(a, b)
      normalize_degrees(a + diff * t)
    end

    # Create degrees from value.
    #
    # @param value [Float]
    # @return [Degrees]
    def self.degrees(value)
      Degrees.new(value)
    end

    # Create radians from value.
    #
    # @param value [Float]
    # @return [Radians]
    def self.radians(value)
      Radians.new(value)
    end

    # Arcsine (returns radians).
    #
    # @param value [Float] must be in [-1, 1]
    # @return [Result]
    def self.asin(value)
      return Result.error(OutOfRangeError.new("asin input must be in [-1, 1]")) if value.abs > 1

      Result.ok(Radians.new(Math.asin(value), normalize: false))
    end

    # Arccosine (returns radians).
    #
    # @param value [Float] must be in [-1, 1]
    # @return [Result]
    def self.acos(value)
      return Result.error(OutOfRangeError.new("acos input must be in [-1, 1]")) if value.abs > 1

      Result.ok(Radians.new(Math.acos(value), normalize: false))
    end

    # Arctangent (returns radians).
    #
    # @param value [Float]
    # @return [Radians]
    def self.atan(value)
      Radians.new(Math.atan(value), normalize: false)
    end

    # Two-argument arctangent (returns radians).
    #
    # @param y [Float]
    # @param x [Float]
    # @return [Radians]
    def self.atan2(y, x)
      Radians.new(Math.atan2(y, x))
    end
  end
end
