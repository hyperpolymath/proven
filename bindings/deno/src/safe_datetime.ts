// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeDateTime - ISO 8601 date/time handling.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_datetime_parse and proven_datetime_format_iso8601 require
 * struct-by-value FFI which needs buffer marshaling. The simpler utility
 * functions (is_leap_year, days_in_month) are directly callable.
 *
 * @module
 */

import { getLib } from './ffi.ts';

/**
 * Safe date/time operations backed by formally verified Idris 2 code.
 */
export class SafeDateTime {
  /**
   * Check if a year is a leap year.
   *
   * @param year - The year to check.
   * @returns True if the year is a leap year.
   */
  static isLeapYear(year: number): boolean {
    const symbols = getLib();
    return symbols.proven_datetime_is_leap_year(year);
  }

  /**
   * Get the number of days in a month.
   *
   * @param year - The year (needed for February).
   * @param month - The month (1-12).
   * @returns Number of days in the month.
   */
  static daysInMonth(year: number, month: number): number {
    const symbols = getLib();
    return symbols.proven_datetime_days_in_month(year, month);
  }
}
