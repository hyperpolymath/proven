// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeDatetime - Typed wrapper for datetime operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 */

/** JavaScript bindings to the SafeDateTime FFI wrapper. */
module SafeDateTimeJs = {
  @module("../../javascript/src/safe_datetime.js") @scope("SafeDateTime")
  external isLeapYear: int => bool = "isLeapYear"

  @module("../../javascript/src/safe_datetime.js") @scope("SafeDateTime")
  external daysInMonth: (int, int) => int = "daysInMonth"
}

/**
 * Check if a year is a leap year.
 * Delegates to proven_datetime_is_leap_year via FFI.
 *
 * @param year The year to check.
 * @returns true if the year is a leap year.
 */
let isLeapYear = SafeDateTimeJs.isLeapYear

/**
 * Get the number of days in a month.
 * Delegates to proven_datetime_days_in_month via FFI.
 *
 * @param year The year.
 * @param month The month (1-12).
 * @returns The number of days in the given month.
 */
let daysInMonth = SafeDateTimeJs.daysInMonth
