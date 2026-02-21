# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Safe angle operations with degree/radian conversions.
  module SafeAngle
    # Pi constant.
    PI     = Math::PI
    TWO_PI = Math::PI * 2

    # Angle in degrees with normalization.
    struct Degrees
      getter value : Float64

      def initialize(@value : Float64)
      end

      # Create normalized to [0, 360).
      def self.normalized(value : Float64) : Degrees
        Degrees.new(SafeAngle.normalize_degrees(value))
      end

      # Convert to radians.
      def to_radians : Radians
        Radians.new(@value * PI / 180.0)
      end

      # Normalize to [0, 360).
      def normalize : Degrees
        Degrees.new(SafeAngle.normalize_degrees(@value))
      end

      # Normalize to [-180, 180).
      def normalize_signed : Degrees
        v = SafeAngle.normalize_degrees(@value)
        v -= 360.0 if v >= 180.0
        Degrees.new(v)
      end

      # Add angles.
      def +(other : Degrees) : Degrees
        Degrees.normalized(@value + other.value)
      end

      # Subtract angles.
      def -(other : Degrees) : Degrees
        Degrees.normalized(@value - other.value)
      end

      # Sine of angle.
      def sin : Float64
        to_radians.sin
      end

      # Cosine of angle.
      def cos : Float64
        to_radians.cos
      end

      # Tangent of angle.
      def tan : Float64
        to_radians.tan
      end

      def to_s : String
        "#{@value}°"
      end
    end

    # Angle in radians.
    struct Radians
      getter value : Float64

      def initialize(@value : Float64)
      end

      # Create normalized to [0, 2π).
      def self.normalized(value : Float64) : Radians
        Radians.new(SafeAngle.normalize_radians(value))
      end

      # Convert to degrees.
      def to_degrees : Degrees
        Degrees.new(@value * 180.0 / PI)
      end

      # Normalize to [0, 2π).
      def normalize : Radians
        Radians.new(SafeAngle.normalize_radians(@value))
      end

      # Add angles.
      def +(other : Radians) : Radians
        Radians.normalized(@value + other.value)
      end

      # Subtract angles.
      def -(other : Radians) : Radians
        Radians.normalized(@value - other.value)
      end

      # Sine of angle.
      def sin : Float64
        Math.sin(@value)
      end

      # Cosine of angle.
      def cos : Float64
        Math.cos(@value)
      end

      # Tangent of angle.
      def tan : Float64
        Math.tan(@value)
      end

      def to_s : String
        "#{@value} rad"
      end
    end

    # Normalize degrees to [0, 360).
    def self.normalize_degrees(deg : Float64) : Float64
      result = deg % 360.0
      result += 360.0 if result < 0.0
      result
    end

    # Normalize radians to [0, 2π).
    def self.normalize_radians(rad : Float64) : Float64
      result = rad % TWO_PI
      result += TWO_PI if result < 0.0
      result
    end

    # Convert degrees to radians.
    def self.deg_to_rad(deg : Float64) : Float64
      deg * PI / 180.0
    end

    # Convert radians to degrees.
    def self.rad_to_deg(rad : Float64) : Float64
      rad * 180.0 / PI
    end

    # Calculate angle difference (shortest path).
    def self.angle_diff_degrees(a : Float64, b : Float64) : Float64
      diff = normalize_degrees(b - a)
      diff > 180.0 ? diff - 360.0 : diff
    end

    # Calculate angle difference in radians.
    def self.angle_diff_radians(a : Float64, b : Float64) : Float64
      diff = normalize_radians(b - a)
      diff > PI ? diff - TWO_PI : diff
    end

    # Linear interpolation between angles (degrees).
    def self.lerp_degrees(a : Float64, b : Float64, t : Float64) : Float64
      diff = angle_diff_degrees(a, b)
      normalize_degrees(a + diff * t.clamp(0.0, 1.0))
    end

    # Linear interpolation between angles (radians).
    def self.lerp_radians(a : Float64, b : Float64, t : Float64) : Float64
      diff = angle_diff_radians(a, b)
      normalize_radians(a + diff * t.clamp(0.0, 1.0))
    end

    # Arc sine (returns radians).
    def self.asin(value : Float64) : Float64?
      return nil if value < -1.0 || value > 1.0
      Math.asin(value)
    end

    # Arc cosine (returns radians).
    def self.acos(value : Float64) : Float64?
      return nil if value < -1.0 || value > 1.0
      Math.acos(value)
    end

    # Arc tangent (returns radians).
    def self.atan(value : Float64) : Float64
      Math.atan(value)
    end

    # Two-argument arc tangent (returns radians).
    def self.atan2(y : Float64, x : Float64) : Float64
      Math.atan2(y, x)
    end

    # Check if angle is between two others (going clockwise).
    def self.between_degrees?(angle : Float64, start_angle : Float64, end_angle : Float64) : Bool
      a = normalize_degrees(angle)
      s = normalize_degrees(start_angle)
      e = normalize_degrees(end_angle)

      if s <= e
        a >= s && a <= e
      else
        a >= s || a <= e
      end
    end

    # Compass direction from bearing (in degrees).
    def self.compass_direction(bearing : Float64) : String
      normalized = normalize_degrees(bearing)
      case normalized
      when 337.5..360.0, 0.0...22.5    then "N"
      when 22.5...67.5                 then "NE"
      when 67.5...112.5                then "E"
      when 112.5...157.5               then "SE"
      when 157.5...202.5               then "S"
      when 202.5...247.5               then "SW"
      when 247.5...292.5               then "W"
      when 292.5...337.5               then "NW"
      else                                  "N"
      end
    end
  end
end
