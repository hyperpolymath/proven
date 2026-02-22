// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeDateTime - Date/time operations via libproven FFI.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.

module SafeDateTime {

  public use LibProven;

  /**
   * Parse ISO 8601 date string.
   *
   * Supports: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, YYYY-MM-DDTHH:MM:SSZ,
   * YYYY-MM-DDTHH:MM:SS+HH:MM.
   *
   * :arg s: ISO 8601 date/time string.
   * :returns: ``none`` on parse error, otherwise a DateTime record.
   */
  proc parse(s: string): DateTime? {
    var (ptr, len) = toCBytes(s);
    var r = provenDatetimeParse(ptr, len);
    if isOk(r.status) then return r.datetime;
    return none;
  }

  /**
   * Format DateTime as ISO 8601 string.
   *
   * :arg dt: DateTime record.
   * :returns: ``none`` on error, otherwise the formatted string.
   */
  proc formatIso8601(dt: DateTime): string? {
    var r = provenDatetimeFormatIso8601(dt);
    if isOk(r.status) {
      var result = extractString(r);
      return result;
    }
    return none;
  }

  /**
   * Check if year is a leap year.
   *
   * :arg year: Year to check.
   * :returns: true if leap year, false otherwise.
   */
  proc isLeapYear(year: int(32)): bool {
    return provenDatetimeIsLeapYear(year);
  }

  /**
   * Get number of days in a month.
   *
   * :arg year: Year.
   * :arg month: Month (1-12).
   * :returns: Number of days (0 if invalid month).
   */
  proc daysInMonth(year: int(32), month: uint(8)): uint(8) {
    return provenDatetimeDaysInMonth(year, month);
  }

}
