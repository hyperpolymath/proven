// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeDatetime - Typed wrapper for datetime operations that cannot crash.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import { SafeDateTime as JsSafeDateTime } from '../../javascript/src/safe_datetime.js';

/**
 * Safe datetime operations backed by formally verified Idris 2 code.
 * All methods delegate to the JavaScript FFI wrapper.
 */
export class SafeDatetime {
  /**
   * Check if a year is a leap year.
   * Delegates to proven_datetime_is_leap_year via FFI.
   *
   * @param year - The year to check.
   * @returns true if the year is a leap year.
   */
  static isLeapYear(year: number): boolean {
    return JsSafeDateTime.isLeapYear(year);
  }

  /**
   * Get the number of days in a month.
   * Delegates to proven_datetime_days_in_month via FFI.
   *
   * @param year - The year.
   * @param month - The month (1-12).
   * @returns The number of days in the given month.
   */
  static daysInMonth(year: number, month: number): number {
    return JsSafeDateTime.daysInMonth(year, month);
  }
}

// Re-export DateTime and Duration classes from the JavaScript FFI layer
export { DateTime } from '../../javascript/src/safe_datetime.js';
export { Duration } from '../../javascript/src/safe_datetime.js';
