# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeDateTime -- thin FFI wrapper around libproven's datetime operations.
# All computation happens in the Idris2/Zig core; this file only marshals data.

module Proven
  module SafeDateTime
    # Parse an ISO 8601 date/time string.  Returns the DateTime struct or nil.
    def self.parse(input : String) : LibProven::DateTime?
      slice = input.to_slice
      result = LibProven.proven_datetime_parse(slice.to_unsafe, slice.size)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      result.datetime
    end

    # Format a DateTime as an ISO 8601 string.
    def self.format_iso8601(dt : LibProven::DateTime) : String?
      result = LibProven.proven_datetime_format_iso8601(dt)
      return nil unless result.status == LibProven::ProvenStatus::Ok
      return nil if result.value.null?
      str = String.new(result.value, result.length)
      LibProven.proven_free_string(result.value)
      str
    end

    # Check if a year is a leap year.
    def self.leap_year?(year : Int32) : Bool
      LibProven.proven_datetime_is_leap_year(year)
    end

    # Get the number of days in a given month.
    def self.days_in_month(year : Int32, month : UInt8) : UInt8
      LibProven.proven_datetime_days_in_month(year, month)
    end
  end
end
