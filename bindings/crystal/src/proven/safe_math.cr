# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeMath -- thin FFI wrapper around libproven's arithmetic operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeMath
    # Safe integer division.  Returns nil on division by zero or MIN/-1 overflow.
    def self.div(a : Int64, b : Int64) : Int64?
      result = LibProven.proven_math_div(a, b)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Safe modulo.  Returns nil on division by zero.
    def self.mod(a : Int64, b : Int64) : Int64?
      result = LibProven.proven_math_mod(a, b)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Checked addition.  Returns nil on overflow.
    def self.add(a : Int64, b : Int64) : Int64?
      result = LibProven.proven_math_add_checked(a, b)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Checked subtraction.  Returns nil on underflow.
    def self.sub(a : Int64, b : Int64) : Int64?
      result = LibProven.proven_math_sub_checked(a, b)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Checked multiplication.  Returns nil on overflow.
    def self.mul(a : Int64, b : Int64) : Int64?
      result = LibProven.proven_math_mul_checked(a, b)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Safe absolute value.  Returns nil for Int64::MIN.
    def self.abs(a : Int64) : Int64?
      result = LibProven.proven_math_abs_safe(a)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Clamp value to [lo, hi].
    def self.clamp(value : Int64, lo : Int64, hi : Int64) : Int64
      LibProven.proven_math_clamp(lo, hi, value)
    end

    # Integer exponentiation with overflow checking.  Returns nil on overflow.
    def self.pow(base : Int64, exp : UInt32) : Int64?
      result = LibProven.proven_math_pow_checked(base, exp)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end
  end
end
