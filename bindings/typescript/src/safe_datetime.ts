// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * DateTime wrapper providing safe date operations.
 */
export class DateTime {
  private readonly timestamp: number;

  private constructor(timestamp: number) {
    this.timestamp = timestamp;
  }

  static now(): DateTime {
    return new DateTime(Date.now());
  }

  static fromTimestamp(ms: number): DateTime {
    return new DateTime(ms);
  }

  static fromDate(date: Date): DateTime {
    return new DateTime(date.getTime());
  }

  static parse(input: string): Result<DateTime> {
    const timestamp = Date.parse(input);
    if (isNaN(timestamp)) {
      return { ok: false, error: 'Invalid date string' };
    }
    return { ok: true, value: new DateTime(timestamp) };
  }

  static parseISO(input: string): Result<DateTime> {
    // ISO 8601 format validation
    const isoRegex = /^\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}:\d{2}(\.\d{3})?(Z|[+-]\d{2}:\d{2})?)?$/;
    if (!isoRegex.test(input)) {
      return { ok: false, error: 'Invalid ISO 8601 format' };
    }
    return DateTime.parse(input);
  }

  static fromParts(
    year: number,
    month: number,
    day: number,
    hour: number = 0,
    minute: number = 0,
    second: number = 0,
    ms: number = 0
  ): Result<DateTime> {
    if (month < 1 || month > 12) {
      return { ok: false, error: 'Month must be 1-12' };
    }
    if (day < 1 || day > 31) {
      return { ok: false, error: 'Day must be 1-31' };
    }
    if (hour < 0 || hour > 23) {
      return { ok: false, error: 'Hour must be 0-23' };
    }
    if (minute < 0 || minute > 59) {
      return { ok: false, error: 'Minute must be 0-59' };
    }
    if (second < 0 || second > 59) {
      return { ok: false, error: 'Second must be 0-59' };
    }

    const date = new Date(year, month - 1, day, hour, minute, second, ms);
    if (date.getMonth() !== month - 1) {
      return { ok: false, error: 'Invalid day for month' };
    }

    return { ok: true, value: new DateTime(date.getTime()) };
  }

  toTimestamp(): number {
    return this.timestamp;
  }

  toDate(): Date {
    return new Date(this.timestamp);
  }

  toISO(): string {
    return new Date(this.timestamp).toISOString();
  }

  toDateString(): string {
    return new Date(this.timestamp).toISOString().split('T')[0];
  }

  toTimeString(): string {
    return new Date(this.timestamp).toISOString().split('T')[1].replace('Z', '');
  }

  year(): number {
    return new Date(this.timestamp).getFullYear();
  }

  month(): number {
    return new Date(this.timestamp).getMonth() + 1;
  }

  day(): number {
    return new Date(this.timestamp).getDate();
  }

  hour(): number {
    return new Date(this.timestamp).getHours();
  }

  minute(): number {
    return new Date(this.timestamp).getMinutes();
  }

  second(): number {
    return new Date(this.timestamp).getSeconds();
  }

  millisecond(): number {
    return new Date(this.timestamp).getMilliseconds();
  }

  dayOfWeek(): number {
    return new Date(this.timestamp).getDay();
  }

  dayOfYear(): number {
    const start = new Date(this.year(), 0, 0);
    const diff = this.timestamp - start.getTime();
    return Math.floor(diff / (1000 * 60 * 60 * 24));
  }

  weekOfYear(): number {
    const date = new Date(this.timestamp);
    date.setHours(0, 0, 0, 0);
    date.setDate(date.getDate() + 3 - ((date.getDay() + 6) % 7));
    const week1 = new Date(date.getFullYear(), 0, 4);
    return 1 + Math.round(((date.getTime() - week1.getTime()) / 86400000 - 3 + ((week1.getDay() + 6) % 7)) / 7);
  }

  isLeapYear(): boolean {
    const year = this.year();
    return (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
  }

  addMilliseconds(ms: number): DateTime {
    return new DateTime(this.timestamp + ms);
  }

  addSeconds(seconds: number): DateTime {
    return this.addMilliseconds(seconds * 1000);
  }

  addMinutes(minutes: number): DateTime {
    return this.addMilliseconds(minutes * 60 * 1000);
  }

  addHours(hours: number): DateTime {
    return this.addMilliseconds(hours * 60 * 60 * 1000);
  }

  addDays(days: number): DateTime {
    return this.addMilliseconds(days * 24 * 60 * 60 * 1000);
  }

  addWeeks(weeks: number): DateTime {
    return this.addDays(weeks * 7);
  }

  addMonths(months: number): DateTime {
    const date = new Date(this.timestamp);
    date.setMonth(date.getMonth() + months);
    return new DateTime(date.getTime());
  }

  addYears(years: number): DateTime {
    const date = new Date(this.timestamp);
    date.setFullYear(date.getFullYear() + years);
    return new DateTime(date.getTime());
  }

  startOfDay(): DateTime {
    const date = new Date(this.timestamp);
    date.setHours(0, 0, 0, 0);
    return new DateTime(date.getTime());
  }

  endOfDay(): DateTime {
    const date = new Date(this.timestamp);
    date.setHours(23, 59, 59, 999);
    return new DateTime(date.getTime());
  }

  startOfMonth(): DateTime {
    const date = new Date(this.timestamp);
    date.setDate(1);
    date.setHours(0, 0, 0, 0);
    return new DateTime(date.getTime());
  }

  endOfMonth(): DateTime {
    const date = new Date(this.timestamp);
    date.setMonth(date.getMonth() + 1, 0);
    date.setHours(23, 59, 59, 999);
    return new DateTime(date.getTime());
  }

  isBefore(other: DateTime): boolean {
    return this.timestamp < other.timestamp;
  }

  isAfter(other: DateTime): boolean {
    return this.timestamp > other.timestamp;
  }

  equals(other: DateTime): boolean {
    return this.timestamp === other.timestamp;
  }

  diff(other: DateTime): Duration {
    return new Duration(Math.abs(this.timestamp - other.timestamp));
  }
}

/**
 * Duration represents a time span.
 */
export class Duration {
  private readonly ms: number;

  constructor(milliseconds: number) {
    this.ms = Math.abs(milliseconds);
  }

  static fromMilliseconds(ms: number): Duration {
    return new Duration(ms);
  }

  static fromSeconds(seconds: number): Duration {
    return new Duration(seconds * 1000);
  }

  static fromMinutes(minutes: number): Duration {
    return new Duration(minutes * 60 * 1000);
  }

  static fromHours(hours: number): Duration {
    return new Duration(hours * 60 * 60 * 1000);
  }

  static fromDays(days: number): Duration {
    return new Duration(days * 24 * 60 * 60 * 1000);
  }

  toMilliseconds(): number {
    return this.ms;
  }

  toSeconds(): number {
    return this.ms / 1000;
  }

  toMinutes(): number {
    return this.ms / (60 * 1000);
  }

  toHours(): number {
    return this.ms / (60 * 60 * 1000);
  }

  toDays(): number {
    return this.ms / (24 * 60 * 60 * 1000);
  }

  add(other: Duration): Duration {
    return new Duration(this.ms + other.ms);
  }

  subtract(other: Duration): Duration {
    return new Duration(Math.max(0, this.ms - other.ms));
  }
}

export const SafeDateTime = {
  DateTime,
  Duration,
};
