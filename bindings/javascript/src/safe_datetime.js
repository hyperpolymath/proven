// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeDateTime - Date/time parsing that cannot crash.
 *
 * Provides safe ISO 8601 parsing and date manipulation.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Parsed date/time components.
 * @typedef {Object} DateTime
 * @property {number} year - Year
 * @property {number} month - Month (1-12)
 * @property {number} day - Day (1-31)
 * @property {number} hour - Hour (0-23)
 * @property {number} minute - Minute (0-59)
 * @property {number} second - Second (0-59)
 * @property {number} millisecond - Millisecond (0-999)
 * @property {number} offsetMinutes - Timezone offset in minutes
 */

/**
 * Safe date/time operations.
 */
export class SafeDateTime {
  /**
   * Parse an ISO 8601 date string.
   *
   * @param {string} isoString - ISO 8601 date string
   * @returns {{ ok: true, value: DateTime } | { ok: false, error: string }}
   *
   * @example
   * SafeDateTime.parse("2024-01-15T10:30:00Z")
   */
  static parse(isoString) {
    if (typeof isoString !== 'string') {
      return err('Input must be a string');
    }

    const date = new Date(isoString);
    if (isNaN(date.getTime())) {
      return err('Invalid date format');
    }

    // Parse timezone offset
    let offsetMinutes = 0;
    const offsetMatch = isoString.match(/([+-])(\d{2}):?(\d{2})$/);
    if (offsetMatch) {
      const hours = parseInt(offsetMatch[2], 10);
      const mins = parseInt(offsetMatch[3], 10);
      offsetMinutes = (hours * 60 + mins) * (offsetMatch[1] === '-' ? -1 : 1);
    } else if (isoString.endsWith('Z')) {
      offsetMinutes = 0;
    } else {
      offsetMinutes = -date.getTimezoneOffset();
    }

    return ok({
      year: date.getUTCFullYear(),
      month: date.getUTCMonth() + 1,
      day: date.getUTCDate(),
      hour: date.getUTCHours(),
      minute: date.getUTCMinutes(),
      second: date.getUTCSeconds(),
      millisecond: date.getUTCMilliseconds(),
      offsetMinutes,
    });
  }

  /**
   * Parse a date-only string (YYYY-MM-DD).
   *
   * @param {string} dateString - Date string
   * @returns {{ ok: true, value: DateTime } | { ok: false, error: string }}
   */
  static parseDate(dateString) {
    if (typeof dateString !== 'string') {
      return err('Input must be a string');
    }

    const match = dateString.match(/^(\d{4})-(\d{2})-(\d{2})$/);
    if (!match) {
      return err('Invalid date format. Expected YYYY-MM-DD');
    }

    const year = parseInt(match[1], 10);
    const month = parseInt(match[2], 10);
    const day = parseInt(match[3], 10);

    if (!SafeDateTime.isValidDate(year, month, day)) {
      return err('Invalid date values');
    }

    return ok({
      year,
      month,
      day,
      hour: 0,
      minute: 0,
      second: 0,
      millisecond: 0,
      offsetMinutes: 0,
    });
  }

  /**
   * Format a DateTime to ISO 8601.
   *
   * @param {DateTime} dt - DateTime to format
   * @returns {string}
   */
  static toIso(dt) {
    const pad = (n, width = 2) => String(n).padStart(width, '0');
    let result = `${dt.year}-${pad(dt.month)}-${pad(dt.day)}`;
    result += `T${pad(dt.hour)}:${pad(dt.minute)}:${pad(dt.second)}`;
    if (dt.millisecond > 0) {
      result += `.${pad(dt.millisecond, 3)}`;
    }
    if (dt.offsetMinutes === 0) {
      result += 'Z';
    } else {
      const sign = dt.offsetMinutes >= 0 ? '+' : '-';
      const absOffset = Math.abs(dt.offsetMinutes);
      result += `${sign}${pad(Math.floor(absOffset / 60))}:${pad(absOffset % 60)}`;
    }
    return result;
  }

  /**
   * Format a DateTime to date-only string.
   *
   * @param {DateTime} dt - DateTime to format
   * @returns {string}
   */
  static toDateString(dt) {
    const pad = (n) => String(n).padStart(2, '0');
    return `${dt.year}-${pad(dt.month)}-${pad(dt.day)}`;
  }

  /**
   * Check if a date is valid.
   *
   * @param {number} year - Year
   * @param {number} month - Month (1-12)
   * @param {number} day - Day (1-31)
   * @returns {boolean}
   */
  static isValidDate(year, month, day) {
    if (month < 1 || month > 12) return false;
    if (day < 1) return false;

    const daysInMonth = SafeDateTime.getDaysInMonth(year, month);
    return day <= daysInMonth;
  }

  /**
   * Get days in a month.
   *
   * @param {number} year - Year
   * @param {number} month - Month (1-12)
   * @returns {number}
   */
  static getDaysInMonth(year, month) {
    const days = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    if (month === 2 && SafeDateTime.isLeapYear(year)) {
      return 29;
    }
    return days[month];
  }

  /**
   * Check if year is a leap year.
   *
   * @param {number} year - Year
   * @returns {boolean}
   */
  static isLeapYear(year) {
    return (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
  }

  /**
   * Add days to a DateTime.
   *
   * @param {DateTime} dt - DateTime
   * @param {number} days - Days to add (can be negative)
   * @returns {DateTime}
   */
  static addDays(dt, days) {
    const date = new Date(Date.UTC(dt.year, dt.month - 1, dt.day, dt.hour, dt.minute, dt.second, dt.millisecond));
    date.setUTCDate(date.getUTCDate() + days);
    return {
      year: date.getUTCFullYear(),
      month: date.getUTCMonth() + 1,
      day: date.getUTCDate(),
      hour: date.getUTCHours(),
      minute: date.getUTCMinutes(),
      second: date.getUTCSeconds(),
      millisecond: date.getUTCMilliseconds(),
      offsetMinutes: dt.offsetMinutes,
    };
  }

  /**
   * Add months to a DateTime.
   *
   * @param {DateTime} dt - DateTime
   * @param {number} months - Months to add (can be negative)
   * @returns {DateTime}
   */
  static addMonths(dt, months) {
    const date = new Date(Date.UTC(dt.year, dt.month - 1, dt.day, dt.hour, dt.minute, dt.second, dt.millisecond));
    date.setUTCMonth(date.getUTCMonth() + months);
    return {
      year: date.getUTCFullYear(),
      month: date.getUTCMonth() + 1,
      day: date.getUTCDate(),
      hour: date.getUTCHours(),
      minute: date.getUTCMinutes(),
      second: date.getUTCSeconds(),
      millisecond: date.getUTCMilliseconds(),
      offsetMinutes: dt.offsetMinutes,
    };
  }

  /**
   * Calculate difference in days between two DateTimes.
   *
   * @param {DateTime} dt1 - First DateTime
   * @param {DateTime} dt2 - Second DateTime
   * @returns {number}
   */
  static diffDays(dt1, dt2) {
    const date1 = new Date(Date.UTC(dt1.year, dt1.month - 1, dt1.day));
    const date2 = new Date(Date.UTC(dt2.year, dt2.month - 1, dt2.day));
    const diffMs = date2.getTime() - date1.getTime();
    return Math.round(diffMs / (1000 * 60 * 60 * 24));
  }

  /**
   * Get day of week (0 = Sunday, 6 = Saturday).
   *
   * @param {DateTime} dt - DateTime
   * @returns {number}
   */
  static getDayOfWeek(dt) {
    const date = new Date(Date.UTC(dt.year, dt.month - 1, dt.day));
    return date.getUTCDay();
  }

  /**
   * Compare two DateTimes.
   *
   * @param {DateTime} dt1 - First DateTime
   * @param {DateTime} dt2 - Second DateTime
   * @returns {number} -1 if dt1 < dt2, 0 if equal, 1 if dt1 > dt2
   */
  static compare(dt1, dt2) {
    const date1 = new Date(
      Date.UTC(dt1.year, dt1.month - 1, dt1.day, dt1.hour, dt1.minute, dt1.second, dt1.millisecond),
    );
    const date2 = new Date(
      Date.UTC(dt2.year, dt2.month - 1, dt2.day, dt2.hour, dt2.minute, dt2.second, dt2.millisecond),
    );

    const t1 = date1.getTime() - dt1.offsetMinutes * 60000;
    const t2 = date2.getTime() - dt2.offsetMinutes * 60000;

    if (t1 < t2) return -1;
    if (t1 > t2) return 1;
    return 0;
  }

  /**
   * Get current DateTime in UTC.
   *
   * @returns {DateTime}
   */
  static now() {
    const date = new Date();
    return {
      year: date.getUTCFullYear(),
      month: date.getUTCMonth() + 1,
      day: date.getUTCDate(),
      hour: date.getUTCHours(),
      minute: date.getUTCMinutes(),
      second: date.getUTCSeconds(),
      millisecond: date.getUTCMilliseconds(),
      offsetMinutes: 0,
    };
  }

  /**
   * Get Unix timestamp from DateTime.
   *
   * @param {DateTime} dt - DateTime
   * @returns {number}
   */
  static toTimestamp(dt) {
    const date = new Date(Date.UTC(dt.year, dt.month - 1, dt.day, dt.hour, dt.minute, dt.second, dt.millisecond));
    return Math.floor(date.getTime() / 1000) - dt.offsetMinutes * 60;
  }

  /**
   * Create DateTime from Unix timestamp.
   *
   * @param {number} timestamp - Unix timestamp in seconds
   * @returns {DateTime}
   */
  static fromTimestamp(timestamp) {
    const date = new Date(timestamp * 1000);
    return {
      year: date.getUTCFullYear(),
      month: date.getUTCMonth() + 1,
      day: date.getUTCDate(),
      hour: date.getUTCHours(),
      minute: date.getUTCMinutes(),
      second: date.getUTCSeconds(),
      millisecond: date.getUTCMilliseconds(),
      offsetMinutes: 0,
    };
  }
}
