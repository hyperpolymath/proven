# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeFloat -- thin FFI wrapper around libproven's floating-point operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeFloat
    # Safe floating-point division.  Returns nil on division by zero, NaN, or infinity.
    def self.div(a : Float64, b : Float64) : Float64?
      result = LibProven.proven_float_div(a, b)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Safe square root.  Returns nil for negative or NaN input.
    def self.sqrt(x : Float64) : Float64?
      result = LibProven.proven_float_sqrt(x)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Safe natural logarithm.  Returns nil for non-positive or NaN input.
    def self.ln(x : Float64) : Float64?
      result = LibProven.proven_float_ln(x)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Check if a float is finite (not NaN or Infinity).
    def self.finite?(x : Float64) : Bool
      LibProven.proven_float_is_finite(x)
    end

    # Check if a float is NaN.
    def self.nan?(x : Float64) : Bool
      LibProven.proven_float_is_nan(x)
    end
  end
end
