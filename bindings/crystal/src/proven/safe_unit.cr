# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeUnit -- thin FFI wrapper around libproven's unit conversion operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeUnit
    # Convert length between units.  Returns nil on NaN input.
    def self.convert_length(value : Float64, from : LibProven::LengthUnit, to : LibProven::LengthUnit) : Float64?
      result = LibProven.proven_unit_convert_length(value, from, to)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end

    # Convert temperature between units.
    # Returns nil on NaN input or temperatures below absolute zero.
    def self.convert_temp(value : Float64, from : LibProven::TempUnit, to : LibProven::TempUnit) : Float64?
      result = LibProven.proven_unit_convert_temp(value, from, to)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.value
    end
  end
end
