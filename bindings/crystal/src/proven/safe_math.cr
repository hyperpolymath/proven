# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Safe arithmetic operations with overflow checking.
  module SafeMath
    # Add two Int64 values with overflow checking.
    def self.add(a : Int64, b : Int64) : Int64?
      result = a &+ b
      # Check for overflow
      if b > 0 && result < a
        nil
      elsif b < 0 && result > a
        nil
      else
        result
      end
    end

    # Subtract two Int64 values with overflow checking.
    def self.sub(a : Int64, b : Int64) : Int64?
      result = a &- b
      # Check for overflow
      if b > 0 && result > a
        nil
      elsif b < 0 && result < a
        nil
      else
        result
      end
    end

    # Multiply two Int64 values with overflow checking.
    def self.mul(a : Int64, b : Int64) : Int64?
      return 0_i64 if a == 0 || b == 0

      result = a &* b
      # Check for overflow
      if a != 0 && result // a != b
        nil
      else
        result
      end
    end

    # Divide two Int64 values safely.
    def self.div(a : Int64, b : Int64) : Int64?
      return nil if b == 0
      # Check for overflow (MIN_VALUE / -1)
      return nil if a == Int64::MIN && b == -1
      a // b
    end

    # Modulo operation with safety checks.
    def self.mod(a : Int64, b : Int64) : Int64?
      return nil if b == 0
      a % b
    end

    # Absolute value with overflow checking.
    def self.abs(a : Int64) : Int64?
      return nil if a == Int64::MIN
      a.abs
    end

    # Negate with overflow checking.
    def self.neg(a : Int64) : Int64?
      return nil if a == Int64::MIN
      -a
    end

    # Power operation with overflow checking.
    def self.pow(base : Int64, exp : Int64) : Int64?
      return nil if exp < 0
      return 1_i64 if exp == 0
      return base if exp == 1

      result = 1_i64
      b = base
      e = exp

      while e > 0
        if e & 1 == 1
          new_result = mul(result, b)
          return nil if new_result.nil?
          result = new_result.not_nil!
        end
        e >>= 1
        if e > 0
          new_b = mul(b, b)
          return nil if new_b.nil?
          b = new_b.not_nil!
        end
      end

      result
    end

    # Safe sum of an array.
    def self.safe_sum(values : Array(Int64)) : Int64?
      result = 0_i64
      values.each do |v|
        new_result = add(result, v)
        return nil if new_result.nil?
        result = new_result.not_nil!
      end
      result
    end

    # Safe product of an array.
    def self.safe_product(values : Array(Int64)) : Int64?
      result = 1_i64
      values.each do |v|
        new_result = mul(result, v)
        return nil if new_result.nil?
        result = new_result.not_nil!
      end
      result
    end

    # Clamp value to range.
    def self.clamp(value : Int64, min_val : Int64, max_val : Int64) : Int64
      return min_val if value < min_val
      return max_val if value > max_val
      value
    end

    # Check if value is in range.
    def self.in_range?(value : Int64, min_val : Int64, max_val : Int64) : Bool
      value >= min_val && value <= max_val
    end
  end
end
