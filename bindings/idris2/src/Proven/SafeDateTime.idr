-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Safe date/time operations via libproven FFI.
|||
||| Provides ISO 8601 date/time parsing, formatting, and calendar
||| utilities (leap year, days in month). All logic is performed by
||| the formally verified Idris 2 core through the precompiled shared
||| library.
module Proven.SafeDateTime

import Proven.FFI

%default total

-- ============================================================================
-- DateTime record
-- ============================================================================

||| A date-time value with timezone offset.
|||
||| Represents a point in time with year, month, day, hour, minute,
||| second, nanosecond, and timezone offset. All fields are validated
||| by the libproven parser.
public export
record DateTime where
  constructor MkDateTime
  ||| Year (e.g. 2026)
  year             : Int
  ||| Month (1-12)
  month            : Int
  ||| Day of month (1-31)
  day              : Int
  ||| Hour (0-23)
  hour             : Int
  ||| Minute (0-59)
  minute           : Int
  ||| Second (0-59)
  second           : Int
  ||| Nanosecond (0-999999999)
  nanosecond       : Int
  ||| Timezone offset in minutes from UTC (0 for UTC, negative for west)
  tzOffsetMinutes  : Int

||| Display a DateTime in a human-readable format.
public export
Show DateTime where
  show dt = show dt.year ++ "-"
         ++ padTwo dt.month ++ "-"
         ++ padTwo dt.day ++ "T"
         ++ padTwo dt.hour ++ ":"
         ++ padTwo dt.minute ++ ":"
         ++ padTwo dt.second
         ++ if dt.tzOffsetMinutes == 0 then "Z"
            else let sign = if dt.tzOffsetMinutes >= 0 then "+" else "-"
                     absOff = if dt.tzOffsetMinutes >= 0
                              then dt.tzOffsetMinutes
                              else negate dt.tzOffsetMinutes
                     offH = absOff `div` 60
                     offM = absOff `mod` 60
                 in sign ++ padTwo offH ++ ":" ++ padTwo offM
  where
    padTwo : Int -> String
    padTwo n = if n < 10 then "0" ++ show n else show n

-- ============================================================================
-- Parsing
-- ============================================================================

||| Parse an ISO 8601 date/time string.
|||
||| Supports the following formats:
|||   - YYYY-MM-DD
|||   - YYYY-MM-DDTHH:MM:SS
|||   - YYYY-MM-DDTHH:MM:SSZ
|||   - YYYY-MM-DDTHH:MM:SS+HH:MM
|||   - YYYY-MM-DDTHH:MM:SS-HH:MM
|||
||| Returns `Just dt` on success, `Nothing` if the string is not a
||| valid ISO 8601 date/time.
|||
||| Note: The C function returns a ProvenDateTimeResult struct via an
||| opaque pointer. Full field extraction requires C accessor shims.
||| @ str The ISO 8601 date/time string to parse
public export
parseDatetime : HasIO io => (str : String) -> io (Maybe DateTime)
parseDatetime str = do
  let len = cast {to=Int} (length str)
  resultPtr <- primIO $ prim__proven_datetime_parse str len
  -- The result pointer contains the ProvenDateTimeResult struct.
  -- Full extraction requires C struct accessor helpers. For now,
  -- we indicate success/failure based on pointer validity.
  if prim__nullAnyPtr resultPtr /= 0
    then pure Nothing
    else do
      -- Placeholder: a production implementation would extract fields
      -- via C accessor shims.
      pure Nothing

-- ============================================================================
-- Calendar utilities (pure functions)
-- ============================================================================

||| Check if a year is a leap year.
|||
||| This is a pure function: it cannot fail. The computation is performed
||| by the verified Idris 2 core in libproven.
||| @ year The year to check
public export
isLeapYear : (year : Int) -> Bool
isLeapYear year = prim__proven_datetime_is_leap_year year /= 0

||| Get the number of days in a given month.
|||
||| Returns 0 for an invalid month number. This is a pure function.
||| @ year  The year (needed for February in leap years)
||| @ month The month number (1-12)
public export
daysInMonth : (year : Int) -> (month : Int) -> Int
daysInMonth = prim__proven_datetime_days_in_month

-- ============================================================================
-- Validation helpers
-- ============================================================================

||| Check whether a date is valid (correct year/month/day combination).
|||
||| Validates that the month is 1-12 and the day is within the correct
||| range for that month and year.
||| @ year  The year
||| @ month The month (1-12)
||| @ day   The day (1-31)
public export
isValidDate : (year : Int) -> (month : Int) -> (day : Int) -> Bool
isValidDate year month day =
  month >= 1 && month <= 12
  && day >= 1
  && day <= daysInMonth year month
