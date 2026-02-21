# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Safe floating-point operations with NaN and Infinity handling.
  module SafeFloat
    # Parse float from string safely.
    def self.parse(input : String) : Float64?
      return nil if input.empty?

      begin
        value = input.to_f64
        return nil if value.nan? || value.infinite?
        value
      rescue
        nil
      end
    end

    # Check if float is finite (not NaN or Infinity).
    def self.finite?(value : Float64) : Bool
      value.finite?
    end

    # Check if float is NaN.
    def self.nan?(value : Float64) : Bool
      value.nan?
    end

    # Check if float is infinite.
    def self.infinite?(value : Float64) : Bool
      value.infinite? != nil
    end

    # Safe add (returns nil on non-finite result).
    def self.add(a : Float64, b : Float64) : Float64?
      result = a + b
      result.finite? ? result : nil
    end

    # Safe subtract.
    def self.sub(a : Float64, b : Float64) : Float64?
      result = a - b
      result.finite? ? result : nil
    end

    # Safe multiply.
    def self.mul(a : Float64, b : Float64) : Float64?
      result = a * b
      result.finite? ? result : nil
    end

    # Safe divide.
    def self.div(a : Float64, b : Float64) : Float64?
      return nil if b == 0.0
      result = a / b
      result.finite? ? result : nil
    end

    # Safe modulo.
    def self.mod(a : Float64, b : Float64) : Float64?
      return nil if b == 0.0
      result = a % b
      result.finite? ? result : nil
    end

    # Safe power.
    def self.pow(base : Float64, exp : Float64) : Float64?
      result = base ** exp
      result.finite? ? result : nil
    end

    # Safe square root.
    def self.sqrt(value : Float64) : Float64?
      return nil if value < 0.0
      result = Math.sqrt(value)
      result.finite? ? result : nil
    end

    # Safe logarithm.
    def self.log(value : Float64) : Float64?
      return nil if value <= 0.0
      result = Math.log(value)
      result.finite? ? result : nil
    end

    # Safe log base 10.
    def self.log10(value : Float64) : Float64?
      return nil if value <= 0.0
      result = Math.log10(value)
      result.finite? ? result : nil
    end

    # Round to specified decimal places.
    def self.round(value : Float64, decimals : Int32 = 0) : Float64?
      return nil unless value.finite?
      multiplier = 10.0 ** decimals
      (value * multiplier).round / multiplier
    end

    # Floor to specified decimal places.
    def self.floor(value : Float64, decimals : Int32 = 0) : Float64?
      return nil unless value.finite?
      multiplier = 10.0 ** decimals
      (value * multiplier).floor / multiplier
    end

    # Ceil to specified decimal places.
    def self.ceil(value : Float64, decimals : Int32 = 0) : Float64?
      return nil unless value.finite?
      multiplier = 10.0 ** decimals
      (value * multiplier).ceil / multiplier
    end

    # Truncate to specified decimal places.
    def self.truncate(value : Float64, decimals : Int32 = 0) : Float64?
      return nil unless value.finite?
      multiplier = 10.0 ** decimals
      (value * multiplier).trunc / multiplier
    end

    # Clamp value to range.
    def self.clamp(value : Float64, min_val : Float64, max_val : Float64) : Float64?
      return nil unless value.finite? && min_val.finite? && max_val.finite?
      value.clamp(min_val, max_val)
    end

    # Check if value is in range.
    def self.in_range?(value : Float64, min_val : Float64, max_val : Float64) : Bool
      value.finite? && value >= min_val && value <= max_val
    end

    # Compare with epsilon tolerance.
    def self.approx_equal?(a : Float64, b : Float64, epsilon : Float64 = 1e-10) : Bool
      return false unless a.finite? && b.finite?
      (a - b).abs < epsilon
    end

    # Linear interpolation.
    def self.lerp(a : Float64, b : Float64, t : Float64) : Float64?
      return nil unless a.finite? && b.finite? && t.finite?
      a + (b - a) * t.clamp(0.0, 1.0)
    end

    # Inverse linear interpolation.
    def self.inv_lerp(a : Float64, b : Float64, value : Float64) : Float64?
      return nil unless a.finite? && b.finite? && value.finite?
      return nil if a == b
      (value - a) / (b - a)
    end

    # Map value from one range to another.
    def self.map_range(
      value : Float64,
      in_min : Float64,
      in_max : Float64,
      out_min : Float64,
      out_max : Float64
    ) : Float64?
      t = inv_lerp(in_min, in_max, value)
      return nil if t.nil?
      lerp(out_min, out_max, t)
    end

    # Sign of value (-1, 0, or 1).
    def self.sign(value : Float64) : Int32?
      return nil unless value.finite?
      return 0 if value == 0.0
      value > 0.0 ? 1 : -1
    end

    # Safe sum of array.
    def self.sum(values : Array(Float64)) : Float64?
      result = 0.0
      values.each do |v|
        return nil unless v.finite?
        result += v
        return nil unless result.finite?
      end
      result
    end

    # Safe mean of array.
    def self.mean(values : Array(Float64)) : Float64?
      return nil if values.empty?
      total = sum(values)
      return nil if total.nil?
      total / values.size
    end
  end
end
