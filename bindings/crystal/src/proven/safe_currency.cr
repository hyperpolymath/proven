# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeCurrency -- thin FFI wrapper around libproven's currency operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeCurrency
    # Parse a 3-letter currency code string.  Returns the enum value or nil.
    def self.parse_code(input : String) : Int32?
      slice = input.to_slice
      result = LibProven.currency_parse_code(slice.to_unsafe, slice.size)
      return nil unless result.status == 0
      result.code
    end

    # Check if a string is a valid currency code.
    def self.valid_code?(input : String) : Bool
      slice = input.to_slice
      LibProven.currency_is_valid_code(slice.to_unsafe, slice.size)
    end

    # Get decimal places for a currency code.
    def self.decimals(code : Int32) : UInt8
      LibProven.currency_get_decimals(code)
    end

    # Create money from major units (e.g. dollars).
    def self.from_major(amount : Int64, currency : Int32) : LibProven::Money
      LibProven.money_from_major(amount, currency)
    end

    # Create money from minor units (e.g. cents).
    def self.from_minor(amount : Int64, currency : Int32) : LibProven::Money
      LibProven.money_from_minor(amount, currency)
    end

    # Create zero money for a currency.
    def self.zero(currency : Int32) : LibProven::Money
      LibProven.money_zero(currency)
    end

    # Add two money values.  Returns nil on currency mismatch or overflow.
    def self.add(a : LibProven::Money, b : LibProven::Money) : LibProven::Money?
      result = LibProven.money_add(a, b)
      return nil unless result.status == 0
      result.value
    end

    # Subtract two money values.  Returns nil on currency mismatch or underflow.
    def self.sub(a : LibProven::Money, b : LibProven::Money) : LibProven::Money?
      result = LibProven.money_sub(a, b)
      return nil unless result.status == 0
      result.value
    end

    # Multiply money by a scalar.  Returns nil on overflow.
    def self.mul(m : LibProven::Money, scalar : Int64) : LibProven::Money?
      result = LibProven.money_mul(m, scalar)
      return nil unless result.status == 0
      result.value
    end

    # Divide money by a scalar.  Returns nil on division by zero.
    def self.div(m : LibProven::Money, divisor : Int64) : LibProven::Money?
      result = LibProven.money_div(m, divisor)
      return nil unless result.status == 0
      result.value
    end

    # Get absolute value of money.
    def self.abs(m : LibProven::Money) : LibProven::Money?
      result = LibProven.money_abs(m)
      return nil unless result.status == 0
      result.value
    end

    # Negate money value.
    def self.negate(m : LibProven::Money) : LibProven::Money?
      result = LibProven.money_negate(m)
      return nil unless result.status == 0
      result.value
    end

    # Get major units (truncated).
    def self.major(m : LibProven::Money) : Int64
      LibProven.money_get_major(m)
    end

    # Get minor units.
    def self.minor(m : LibProven::Money) : Int64
      LibProven.money_get_minor(m)
    end

    # Check if money is zero.
    def self.zero?(m : LibProven::Money) : Bool
      LibProven.money_is_zero(m)
    end

    # Check if money is positive.
    def self.positive?(m : LibProven::Money) : Bool
      LibProven.money_is_positive(m)
    end

    # Check if money is negative.
    def self.negative?(m : LibProven::Money) : Bool
      LibProven.money_is_negative(m)
    end

    # Compare two money values.
    def self.compare(a : LibProven::Money, b : LibProven::Money) : Int32
      LibProven.money_compare(a, b)
    end

    # Check if two money values have the same currency.
    def self.same_currency?(a : LibProven::Money, b : LibProven::Money) : Bool
      LibProven.money_same_currency(a, b)
    end

    # Format money with symbol (e.g. "$123.45").
    def self.format(m : LibProven::Money) : String?
      buf = Bytes.new(64)
      status = LibProven.money_format(m, buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe)
    end

    # Format money as plain decimal (e.g. "123.45").
    def self.format_plain(m : LibProven::Money) : String?
      buf = Bytes.new(64)
      status = LibProven.money_format_plain(m, buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe)
    end

    # Format money with ISO code (e.g. "123.45 USD").
    def self.format_iso(m : LibProven::Money) : String?
      buf = Bytes.new(64)
      status = LibProven.money_format_iso(m, buf.to_unsafe, buf.size)
      return nil unless status == 0
      String.new(buf.to_unsafe)
    end
  end
end
