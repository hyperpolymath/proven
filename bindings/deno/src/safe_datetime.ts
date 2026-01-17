// SPDX-License-Identifier: PMPL-1.0
/**
 * Safe datetime operations.
 */

import { err, ok, type Result } from './result.ts';

export interface DateComponents {
  year: number;
  month: number;
  day: number;
}

export interface TimeComponents {
  hour: number;
  minute: number;
  second: number;
}

/**
 * Safe datetime operations.
 */
export class SafeDateTime {
  /** Check if a year is a leap year. */
  static isLeapYear(year: number): boolean {
    return (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
  }

  /** Get days in a month. */
  static daysInMonth(year: number, month: number): number | undefined {
    const days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    if (month < 1 || month > 12) return undefined;
    if (month === 2 && this.isLeapYear(year)) return 29;
    return days[month - 1];
  }

  /** Validate date components. */
  static validateDate(year: number, month: number, day: number): Result<DateComponents> {
    if (month < 1 || month > 12) {
      return err(`Invalid month: ${month}`);
    }

    const maxDay = this.daysInMonth(year, month);
    if (!maxDay) {
      return err(`Invalid month: ${month}`);
    }

    if (day < 1 || day > maxDay) {
      return err(`Invalid day ${day} for month ${month}`);
    }

    return ok({ year, month, day });
  }

  /** Validate time components. */
  static validateTime(hour: number, minute: number, second: number): Result<TimeComponents> {
    if (hour < 0 || hour > 23) {
      return err(`Invalid hour: ${hour}`);
    }
    if (minute < 0 || minute > 59) {
      return err(`Invalid minute: ${minute}`);
    }
    if (second < 0 || second > 59) {
      return err(`Invalid second: ${second}`);
    }
    return ok({ hour, minute, second });
  }

  /** Parse ISO 8601 date (YYYY-MM-DD). */
  static parseIsoDate(s: string): Result<DateComponents> {
    const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s);
    if (!match) {
      return err('Invalid date format');
    }

    const year = parseInt(match[1], 10);
    const month = parseInt(match[2], 10);
    const day = parseInt(match[3], 10);

    return this.validateDate(year, month, day);
  }

  /** Parse ISO 8601 time (HH:MM:SS). */
  static parseIsoTime(s: string): Result<TimeComponents> {
    const match = /^(\d{2}):(\d{2})(?::(\d{2}))?$/.exec(s);
    if (!match) {
      return err('Invalid time format');
    }

    const hour = parseInt(match[1], 10);
    const minute = parseInt(match[2], 10);
    const second = match[3] ? parseInt(match[3], 10) : 0;

    return this.validateTime(hour, minute, second);
  }
}
