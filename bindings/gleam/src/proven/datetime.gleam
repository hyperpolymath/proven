// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// SafeDateTime - Date and time operations that cannot crash.
////
//// Thin FFI wrapper over libproven proven_datetime_* functions.
//// All computation happens in the Idris2 core via Zig FFI.

/// Parsed datetime components from ISO 8601.
pub type DateTime {
  DateTime(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: Int,
    nanosecond: Int,
    timezone_offset_minutes: Int,
  )
}

/// Parse an ISO 8601 date/time string.
/// Supports YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, with optional timezone.
@external(erlang, "proven_nif", "datetime_parse")
pub fn parse(input: String) -> Result(DateTime, String)

/// Format a DateTime as ISO 8601 string.
@external(erlang, "proven_nif", "datetime_format_iso8601")
pub fn format_iso8601(
  year: Int,
  month: Int,
  day: Int,
  hour: Int,
  minute: Int,
  second: Int,
  timezone_offset_minutes: Int,
) -> Result(String, String)

/// Check if a year is a leap year.
@external(erlang, "proven_nif", "datetime_is_leap_year")
pub fn is_leap_year(year: Int) -> Bool

/// Get the number of days in a month (0 if invalid month).
@external(erlang, "proven_nif", "datetime_days_in_month")
pub fn days_in_month(year: Int, month: Int) -> Int
