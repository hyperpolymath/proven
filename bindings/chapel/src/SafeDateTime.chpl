// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// SafeDateTime - Date/time operations via libproven FFI.
//
// Status: GATED on proven#88.

module SafeDateTime {

  public use LibProven;

  /** Parse ISO 8601 date string. */
  proc parse(s: string): Maybe(DateTime) {
    var (ptr, len) = toCBytes(s);
    var r = provenDatetimeParse(ptr, len);
    if isOk(r.status) then return some(r.datetime);
    return absent(DateTime);
  }

  /** Format DateTime as ISO 8601 string. */
  proc formatIso8601(dt: DateTime): Maybe(string) {
    var r = provenDatetimeFormatIso8601(dt);
    if isOk(r.status) then return some(extractString(r));
    return absent(string);
  }

  /** Check if year is a leap year. */
  proc isLeapYear(year: int(32)): bool {
    return provenDatetimeIsLeapYear(year);
  }

  /** Number of days in a month (0 for invalid month). */
  proc daysInMonth(year: int(32), month: uint(8)): uint(8) {
    return provenDatetimeDaysInMonth(year, month);
  }

}
