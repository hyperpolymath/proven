// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeDateTime - ISO 8601 date/time handling.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * Note: proven_datetime_parse and proven_datetime_format_iso8601 require
 * struct-by-value FFI which needs buffer marshaling. The simpler utility
 * functions (is_leap_year, days_in_month) are directly callable.
 * @module
 */

import { getLib } from './ffi.js';
import { ok, err } from './result.js';

/**
 * DateTime representation.
 */
export class DateTime {
  /** @type {number} */ year;
  /** @type {number} */ month;
  /** @type {number} */ day;
  /** @type {number} */ hour;
  /** @type {number} */ minute;
  /** @type {number} */ second;
  /** @type {number} */ nanosecond;
  /** @type {number} */ tzOffsetMinutes;

  /**
   * @param {object} fields - DateTime fields.
   * @param {number} fields.year
   * @param {number} fields.month
   * @param {number} fields.day
   * @param {number} [fields.hour=0]
   * @param {number} [fields.minute=0]
   * @param {number} [fields.second=0]
   * @param {number} [fields.nanosecond=0]
   * @param {number} [fields.tzOffsetMinutes=0]
   */
  constructor({ year, month, day, hour = 0, minute = 0, second = 0, nanosecond = 0, tzOffsetMinutes = 0 }) {
    this.year = year;
    this.month = month;
    this.day = day;
    this.hour = hour;
    this.minute = minute;
    this.second = second;
    this.nanosecond = nanosecond;
    this.tzOffsetMinutes = tzOffsetMinutes;
  }
}

/**
 * Safe date/time operations backed by formally verified Idris 2 code.
 */
export class SafeDateTime {
  /**
   * Check if a year is a leap year.
   *
   * @param {number} year - The year to check.
   * @returns {boolean}
   */
  static isLeapYear(year) {
    const symbols = getLib();
    return symbols.proven_datetime_is_leap_year(year);
  }

  /**
   * Get the number of days in a month.
   *
   * @param {number} year - The year (needed for February).
   * @param {number} month - The month (1-12).
   * @returns {number} Number of days in the month.
   */
  static daysInMonth(year, month) {
    const symbols = getLib();
    return symbols.proven_datetime_days_in_month(year, month);
  }
}
