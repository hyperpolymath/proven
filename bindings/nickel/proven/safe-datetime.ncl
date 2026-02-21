# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeDatetime - Date and time handling for Nickel
#
# Provides date/time validation and operations following
# ISO 8601 standard. All operations are timezone-aware.

{
  # Result type
  Result = {
    value | Dyn,
    ok | Bool,
  },

  # DateTime representation
  DateTime = {
    year | Number,
    month | Number,
    day | Number,
    hour | Number,
    minute | Number,
    second | Number,
    timezone_offset_minutes | Number,
  },

  # Date only representation
  Date = {
    year | Number,
    month | Number,
    day | Number,
  },

  # Time only representation
  Time = {
    hour | Number,
    minute | Number,
    second | Number,
  },

  # Duration representation
  Duration = {
    days | Number,
    hours | Number,
    minutes | Number,
    seconds | Number,
  },

  # Days in each month (non-leap year)
  DAYS_IN_MONTH = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],

  # Check if year is a leap year
  is_leap_year
    : Number -> Bool
    = fun year =>
      (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0),

  # Get days in month
  days_in_month
    : Number -> Number -> Number
    = fun year month =>
      if month < 1 || month > 12 then
        0
      else if month == 2 && is_leap_year year then
        29
      else
        std.array.at (month - 1) DAYS_IN_MONTH,

  # Validate year
  is_valid_year
    : Number -> Bool
    = fun year =>
      year >= 1 && year <= 9999,

  # Validate month
  is_valid_month
    : Number -> Bool
    = fun month =>
      month >= 1 && month <= 12,

  # Validate day for given year/month
  is_valid_day
    : Number -> Number -> Number -> Bool
    = fun year month day =>
      day >= 1 && day <= days_in_month year month,

  # Validate hour
  is_valid_hour
    : Number -> Bool
    = fun hour =>
      hour >= 0 && hour <= 23,

  # Validate minute
  is_valid_minute
    : Number -> Bool
    = fun minute =>
      minute >= 0 && minute <= 59,

  # Validate second
  is_valid_second
    : Number -> Bool
    = fun second =>
      second >= 0 && second <= 59,

  # Validate complete date
  is_valid_date
    : Date -> Bool
    = fun date =>
      is_valid_year date.year &&
      is_valid_month date.month &&
      is_valid_day date.year date.month date.day,

  # Validate complete time
  is_valid_time
    : Time -> Bool
    = fun time =>
      is_valid_hour time.hour &&
      is_valid_minute time.minute &&
      is_valid_second time.second,

  # Validate complete datetime
  is_valid_datetime
    : DateTime -> Bool
    = fun dt =>
      is_valid_year dt.year &&
      is_valid_month dt.month &&
      is_valid_day dt.year dt.month dt.day &&
      is_valid_hour dt.hour &&
      is_valid_minute dt.minute &&
      is_valid_second dt.second &&
      dt.timezone_offset_minutes >= -720 && dt.timezone_offset_minutes <= 840,

  # Create date safely
  make_date
    : Number -> Number -> Number -> { value : Date, ok : Bool }
    = fun year month day =>
      let date = { year = year, month = month, day = day } in
      if is_valid_date date then
        { value = date, ok = true }
      else
        { value = { year = 1970, month = 1, day = 1 }, ok = false },

  # Create time safely
  make_time
    : Number -> Number -> Number -> { value : Time, ok : Bool }
    = fun hour minute second =>
      let time = { hour = hour, minute = minute, second = second } in
      if is_valid_time time then
        { value = time, ok = true }
      else
        { value = { hour = 0, minute = 0, second = 0 }, ok = false },

  # Create datetime safely
  make_datetime
    : Date -> Time -> Number -> { value : DateTime, ok : Bool }
    = fun date time tz_offset =>
      let dt = {
        year = date.year,
        month = date.month,
        day = date.day,
        hour = time.hour,
        minute = time.minute,
        second = time.second,
        timezone_offset_minutes = tz_offset,
      } in
      if is_valid_datetime dt then
        { value = dt, ok = true }
      else
        { value = {
          year = 1970, month = 1, day = 1,
          hour = 0, minute = 0, second = 0,
          timezone_offset_minutes = 0,
        }, ok = false },

  # Compare two dates (returns -1, 0, 1)
  compare_dates
    : Date -> Date -> Number
    = fun a b =>
      if a.year != b.year then
        if a.year < b.year then -1 else 1
      else if a.month != b.month then
        if a.month < b.month then -1 else 1
      else if a.day != b.day then
        if a.day < b.day then -1 else 1
      else
        0,

  # Check if date is in the past (relative to reference)
  is_before
    : Date -> Date -> Bool
    = fun date reference =>
      compare_dates date reference < 0,

  # Check if date is in the future (relative to reference)
  is_after
    : Date -> Date -> Bool
    = fun date reference =>
      compare_dates date reference > 0,

  # Add days to date (simplified - doesn't handle month overflow)
  add_days
    : Date -> Number -> { value : Date, ok : Bool }
    = fun date days =>
      # Simplified implementation - just validates result
      let new_day = date.day + days in
      let max_days = days_in_month date.year date.month in
      if new_day >= 1 && new_day <= max_days then
        { value = { year = date.year, month = date.month, day = new_day }, ok = true }
      else
        { value = date, ok = false },

  # Format date as ISO 8601 string
  format_iso_date
    : Date -> String
    = fun date =>
      let year_str = std.string.from_number date.year in
      let month_str =
        if date.month < 10 then "0" ++ std.string.from_number date.month
        else std.string.from_number date.month
      in
      let day_str =
        if date.day < 10 then "0" ++ std.string.from_number date.day
        else std.string.from_number date.day
      in
      year_str ++ "-" ++ month_str ++ "-" ++ day_str,

  # Format time as ISO 8601 string
  format_iso_time
    : Time -> String
    = fun time =>
      let hour_str =
        if time.hour < 10 then "0" ++ std.string.from_number time.hour
        else std.string.from_number time.hour
      in
      let minute_str =
        if time.minute < 10 then "0" ++ std.string.from_number time.minute
        else std.string.from_number time.minute
      in
      let second_str =
        if time.second < 10 then "0" ++ std.string.from_number time.second
        else std.string.from_number time.second
      in
      hour_str ++ ":" ++ minute_str ++ ":" ++ second_str,

  # Contracts
  ValidDate = std.contract.from_predicate is_valid_date,
  ValidTime = std.contract.from_predicate is_valid_time,
  ValidDateTime = std.contract.from_predicate is_valid_datetime,

  # Year range contract
  Year = std.contract.from_predicate is_valid_year,
  Month = std.contract.from_predicate is_valid_month,
  Hour = std.contract.from_predicate is_valid_hour,
  Minute = std.contract.from_predicate is_valid_minute,
  Second = std.contract.from_predicate is_valid_second,
}
