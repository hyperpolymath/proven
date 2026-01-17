// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeDateTime - Date and time operations that cannot crash.
////
//// Provides safe date/time parsing and manipulation following ISO 8601.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/string

/// A validated date.
pub opaque type Date {
  Date(year: Int, month: Int, day: Int)
}

/// A validated time.
pub opaque type Time {
  Time(hour: Int, minute: Int, second: Int, millisecond: Int)
}

/// A validated datetime.
pub opaque type DateTime {
  DateTime(date: Date, time: Time, timezone_offset_minutes: Int)
}

/// A duration in milliseconds.
pub opaque type Duration {
  Duration(milliseconds: Int)
}

/// Error types for datetime operations.
pub type DateTimeError {
  InvalidYear(year: Int)
  InvalidMonth(month: Int)
  InvalidDay(day: Int)
  InvalidHour(hour: Int)
  InvalidMinute(minute: Int)
  InvalidSecond(second: Int)
  InvalidFormat(message: String)
  InvalidTimezone(offset: String)
}

/// Weekday enumeration.
pub type Weekday {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday
  Sunday
}

/// Month enumeration.
pub type Month {
  January
  February
  March
  April
  May
  June
  July
  August
  September
  October
  November
  December
}

/// Check if a year is a leap year.
pub fn is_leap_year(year: Int) -> Bool {
  { year % 4 == 0 && year % 100 != 0 } || year % 400 == 0
}

/// Get the number of days in a month.
pub fn days_in_month(year: Int, month: Int) -> Result(Int, DateTimeError) {
  case month {
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> Ok(31)
    4 | 6 | 9 | 11 -> Ok(30)
    2 ->
      case is_leap_year(year) {
        True -> Ok(29)
        False -> Ok(28)
      }
    _ -> Error(InvalidMonth(month: month))
  }
}

/// Create a date from year, month, and day.
pub fn date(year: Int, month: Int, day: Int) -> Result(Date, DateTimeError) {
  case month < 1 || month > 12 {
    True -> Error(InvalidMonth(month: month))
    False ->
      case days_in_month(year, month) {
        Error(err) -> Error(err)
        Ok(max_days) ->
          case day < 1 || day > max_days {
            True -> Error(InvalidDay(day: day))
            False -> Ok(Date(year: year, month: month, day: day))
          }
      }
  }
}

/// Create a time from hour, minute, second, and optional millisecond.
pub fn time(
  hour: Int,
  minute: Int,
  second: Int,
  millisecond: Int,
) -> Result(Time, DateTimeError) {
  case hour < 0 || hour > 23 {
    True -> Error(InvalidHour(hour: hour))
    False ->
      case minute < 0 || minute > 59 {
        True -> Error(InvalidMinute(minute: minute))
        False ->
          case second < 0 || second > 59 {
            True -> Error(InvalidSecond(second: second))
            False -> {
              let clamped_ms = int.clamp(millisecond, 0, 999)
              Ok(Time(
                hour: hour,
                minute: minute,
                second: second,
                millisecond: clamped_ms,
              ))
            }
          }
      }
  }
}

/// Create a datetime from date and time.
pub fn datetime(
  dt_date: Date,
  dt_time: Time,
  timezone_offset_minutes: Int,
) -> DateTime {
  DateTime(
    date: dt_date,
    time: dt_time,
    timezone_offset_minutes: timezone_offset_minutes,
  )
}

/// Get the year from a date.
pub fn get_year(dt: Date) -> Int {
  dt.year
}

/// Get the month from a date.
pub fn get_month(dt: Date) -> Int {
  dt.month
}

/// Get the day from a date.
pub fn get_day(dt: Date) -> Int {
  dt.day
}

/// Get the hour from a time.
pub fn get_hour(tm: Time) -> Int {
  tm.hour
}

/// Get the minute from a time.
pub fn get_minute(tm: Time) -> Int {
  tm.minute
}

/// Get the second from a time.
pub fn get_second(tm: Time) -> Int {
  tm.second
}

/// Get the millisecond from a time.
pub fn get_millisecond(tm: Time) -> Int {
  tm.millisecond
}

/// Get the date from a datetime.
pub fn get_date(dt: DateTime) -> Date {
  dt.date
}

/// Get the time from a datetime.
pub fn get_time(dt: DateTime) -> Time {
  dt.time
}

/// Get the timezone offset in minutes from a datetime.
pub fn get_timezone_offset(dt: DateTime) -> Int {
  dt.timezone_offset_minutes
}

/// Parse an ISO 8601 date string (YYYY-MM-DD).
pub fn parse_date(input: String) -> Result(Date, DateTimeError) {
  let trimmed = string.trim(input)
  case string.split(trimmed, "-") {
    [year_str, month_str, day_str] ->
      case int.parse(year_str), int.parse(month_str), int.parse(day_str) {
        Ok(year), Ok(month), Ok(day) -> date(year, month, day)
        _, _, _ -> Error(InvalidFormat(message: "Invalid date format"))
      }
    _ -> Error(InvalidFormat(message: "Expected YYYY-MM-DD format"))
  }
}

/// Parse an ISO 8601 time string (HH:MM:SS or HH:MM:SS.mmm).
pub fn parse_time(input: String) -> Result(Time, DateTimeError) {
  let trimmed = string.trim(input)
  case string.split(trimmed, ":") {
    [hour_str, minute_str, second_part] ->
      case int.parse(hour_str), int.parse(minute_str) {
        Ok(hour), Ok(minute) -> {
          let #(second, millisecond) = parse_seconds_with_fraction(second_part)
          time(hour, minute, second, millisecond)
        }
        _, _ -> Error(InvalidFormat(message: "Invalid time format"))
      }
    [hour_str, minute_str] ->
      case int.parse(hour_str), int.parse(minute_str) {
        Ok(hour), Ok(minute) -> time(hour, minute, 0, 0)
        _, _ -> Error(InvalidFormat(message: "Invalid time format"))
      }
    _ -> Error(InvalidFormat(message: "Expected HH:MM:SS format"))
  }
}

fn parse_seconds_with_fraction(input: String) -> #(Int, Int) {
  case string.split(input, ".") {
    [sec_str, ms_str] ->
      case int.parse(sec_str), int.parse(ms_str) {
        Ok(sec), Ok(ms) -> #(sec, ms)
        _, _ -> #(0, 0)
      }
    [sec_str] ->
      case int.parse(sec_str) {
        Ok(sec) -> #(sec, 0)
        Error(_) -> #(0, 0)
      }
    _ -> #(0, 0)
  }
}

/// Parse an ISO 8601 datetime string.
pub fn parse_datetime(input: String) -> Result(DateTime, DateTimeError) {
  let trimmed = string.trim(input)
  // Split on T or space
  let #(date_part, time_and_tz) = case string.split_once(trimmed, "T") {
    Ok(#(d, t)) -> #(d, t)
    Error(_) ->
      case string.split_once(trimmed, " ") {
        Ok(#(d, t)) -> #(d, t)
        Error(_) -> #(trimmed, "00:00:00")
      }
  }

  // Parse timezone
  let #(time_part, tz_offset) = extract_timezone(time_and_tz)

  case parse_date(date_part), parse_time(time_part) {
    Ok(dt_date), Ok(dt_time) -> Ok(datetime(dt_date, dt_time, tz_offset))
    Error(err), _ -> Error(err)
    _, Error(err) -> Error(err)
  }
}

fn extract_timezone(input: String) -> #(String, Int) {
  case string.ends_with(input, "Z") {
    True -> #(string.drop_end(input, 1), 0)
    False ->
      case string.split_once(input, "+") {
        Ok(#(time, tz)) -> #(time, parse_tz_offset(tz))
        Error(_) ->
          case find_last_minus(input) {
            Some(index) -> {
              let time = string.slice(input, 0, index)
              let tz = string.slice(input, index + 1, string.length(input) - index - 1)
              #(time, -parse_tz_offset(tz))
            }
            None -> #(input, 0)
          }
      }
  }
}

fn find_last_minus(input: String) -> Option(Int) {
  let chars = string.to_graphemes(input)
  find_last_minus_helper(chars, 0, None)
}

fn find_last_minus_helper(
  chars: List(String),
  index: Int,
  last_found: Option(Int),
) -> Option(Int) {
  case chars {
    [] -> last_found
    ["-", ..rest] -> find_last_minus_helper(rest, index + 1, Some(index))
    [_, ..rest] -> find_last_minus_helper(rest, index + 1, last_found)
  }
}

fn parse_tz_offset(input: String) -> Int {
  case string.split(input, ":") {
    [hour_str, minute_str] ->
      case int.parse(hour_str), int.parse(minute_str) {
        Ok(h), Ok(m) -> h * 60 + m
        _, _ -> 0
      }
    [hour_str] ->
      case int.parse(hour_str) {
        Ok(h) -> h * 60
        Error(_) -> 0
      }
    _ -> 0
  }
}

/// Format a date as ISO 8601 (YYYY-MM-DD).
pub fn format_date(dt: Date) -> String {
  pad_int(dt.year, 4) <> "-" <> pad_int(dt.month, 2) <> "-" <> pad_int(dt.day, 2)
}

/// Format a time as ISO 8601 (HH:MM:SS).
pub fn format_time(tm: Time) -> String {
  pad_int(tm.hour, 2)
  <> ":"
  <> pad_int(tm.minute, 2)
  <> ":"
  <> pad_int(tm.second, 2)
}

/// Format a time with milliseconds (HH:MM:SS.mmm).
pub fn format_time_with_ms(tm: Time) -> String {
  format_time(tm) <> "." <> pad_int(tm.millisecond, 3)
}

/// Format a datetime as ISO 8601.
pub fn format_datetime(dt: DateTime) -> String {
  let date_str = format_date(dt.date)
  let time_str = format_time(dt.time)
  let tz_str = format_timezone_offset(dt.timezone_offset_minutes)
  date_str <> "T" <> time_str <> tz_str
}

fn format_timezone_offset(offset_minutes: Int) -> String {
  case offset_minutes {
    0 -> "Z"
    _ -> {
      let sign = case offset_minutes >= 0 {
        True -> "+"
        False -> "-"
      }
      let abs_offset = int.absolute_value(offset_minutes)
      let hours = abs_offset / 60
      let minutes = abs_offset % 60
      sign <> pad_int(hours, 2) <> ":" <> pad_int(minutes, 2)
    }
  }
}

fn pad_int(value: Int, width: Int) -> String {
  let str = int.to_string(value)
  let len = string.length(str)
  case len >= width {
    True -> str
    False -> string.repeat("0", width - len) <> str
  }
}

/// Create a duration from milliseconds.
pub fn duration_from_ms(milliseconds: Int) -> Duration {
  Duration(milliseconds: milliseconds)
}

/// Create a duration from seconds.
pub fn duration_from_seconds(seconds: Int) -> Duration {
  Duration(milliseconds: seconds * 1000)
}

/// Create a duration from minutes.
pub fn duration_from_minutes(minutes: Int) -> Duration {
  Duration(milliseconds: minutes * 60 * 1000)
}

/// Create a duration from hours.
pub fn duration_from_hours(hours: Int) -> Duration {
  Duration(milliseconds: hours * 60 * 60 * 1000)
}

/// Create a duration from days.
pub fn duration_from_days(days: Int) -> Duration {
  Duration(milliseconds: days * 24 * 60 * 60 * 1000)
}

/// Get duration in milliseconds.
pub fn duration_to_ms(d: Duration) -> Int {
  d.milliseconds
}

/// Get duration in seconds.
pub fn duration_to_seconds(d: Duration) -> Int {
  d.milliseconds / 1000
}

/// Add a duration to a time.
pub fn add_duration_to_time(tm: Time, d: Duration) -> Time {
  let total_ms =
    tm.hour * 3_600_000
    + tm.minute * 60_000
    + tm.second * 1000
    + tm.millisecond
    + d.milliseconds

  let normalized_ms = total_ms % 86_400_000
  let positive_ms = case normalized_ms < 0 {
    True -> normalized_ms + 86_400_000
    False -> normalized_ms
  }

  let new_hour = positive_ms / 3_600_000
  let remaining_after_hour = positive_ms % 3_600_000
  let new_minute = remaining_after_hour / 60_000
  let remaining_after_minute = remaining_after_hour % 60_000
  let new_second = remaining_after_minute / 1000
  let new_ms = remaining_after_minute % 1000

  Time(hour: new_hour, minute: new_minute, second: new_second, millisecond: new_ms)
}

/// Compare two dates.
pub fn compare_dates(date_a: Date, date_b: Date) -> Order {
  case int.compare(date_a.year, date_b.year) {
    order.Eq ->
      case int.compare(date_a.month, date_b.month) {
        order.Eq -> int.compare(date_a.day, date_b.day)
        other -> other
      }
    other -> other
  }
}

/// Compare two times.
pub fn compare_times(time_a: Time, time_b: Time) -> Order {
  case int.compare(time_a.hour, time_b.hour) {
    order.Eq ->
      case int.compare(time_a.minute, time_b.minute) {
        order.Eq ->
          case int.compare(time_a.second, time_b.second) {
            order.Eq -> int.compare(time_a.millisecond, time_b.millisecond)
            other -> other
          }
        other -> other
      }
    other -> other
  }
}

/// Get the weekday for a date using Zeller's congruence.
pub fn weekday(dt: Date) -> Weekday {
  let year = dt.year
  let month = dt.month
  let day = dt.day

  // Adjust for Zeller's formula
  let #(adjusted_year, adjusted_month) = case month < 3 {
    True -> #(year - 1, month + 12)
    False -> #(year, month)
  }

  let q = day
  let m = adjusted_month
  let k = adjusted_year % 100
  let j = adjusted_year / 100

  let h = { q + { 13 * { m + 1 } / 5 } + k + { k / 4 } + { j / 4 } - 2 * j } % 7

  // Convert to our Weekday type (0 = Saturday, 1 = Sunday, etc.)
  case { h + 6 } % 7 {
    0 -> Monday
    1 -> Tuesday
    2 -> Wednesday
    3 -> Thursday
    4 -> Friday
    5 -> Saturday
    _ -> Sunday
  }
}

/// Get month name.
pub fn month_name(month: Int) -> Option(String) {
  case month {
    1 -> Some("January")
    2 -> Some("February")
    3 -> Some("March")
    4 -> Some("April")
    5 -> Some("May")
    6 -> Some("June")
    7 -> Some("July")
    8 -> Some("August")
    9 -> Some("September")
    10 -> Some("October")
    11 -> Some("November")
    12 -> Some("December")
    _ -> None
  }
}

/// Get weekday name.
pub fn weekday_name(day: Weekday) -> String {
  case day {
    Monday -> "Monday"
    Tuesday -> "Tuesday"
    Wednesday -> "Wednesday"
    Thursday -> "Thursday"
    Friday -> "Friday"
    Saturday -> "Saturday"
    Sunday -> "Sunday"
  }
}
