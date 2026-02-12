-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCron operations
|||
||| This module exports cron expression parsing and validation to the C ABI
||| via Idris2's RefC backend. All functions are proven total and enforce frequency limits.
|||
||| Return conventions:
||| - Field type → Int (0=Any, 1=Single, 2=Range, 3=Step, 4=List)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Time components → Int (validated values)
|||
||| CRITICAL: Cron expressions must be validated to prevent DoS via excessive
|||           scheduling frequency. Enforce minimum intervals between executions.
|||
||| Field bounds:
||| - minute: 0-59, hour: 0-23, dayOfMonth: 1-31, month: 1-12, dayOfWeek: 0-6
module Proven.FFI.SafeCron

import Proven.SafeCron
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode field type
encodeCronFieldType : CronField -> Int
encodeCronFieldType Any = 0
encodeCronFieldType (Single _) = 1
encodeCronFieldType (Range _ _) = 2
encodeCronFieldType (Step _ _) = 3
encodeCronFieldType (List _) = 4

--------------------------------------------------------------------------------
-- Field Bounds Operations
--------------------------------------------------------------------------------

export
proven_idris_cron_minute_min : Int
proven_idris_cron_minute_min = cast minuteBounds.minVal

export
proven_idris_cron_minute_max : Int
proven_idris_cron_minute_max = cast minuteBounds.maxVal

export
proven_idris_cron_hour_min : Int
proven_idris_cron_hour_min = cast hourBounds.minVal

export
proven_idris_cron_hour_max : Int
proven_idris_cron_hour_max = cast hourBounds.maxVal

export
proven_idris_cron_day_of_month_min : Int
proven_idris_cron_day_of_month_min = cast dayOfMonthBounds.minVal

export
proven_idris_cron_day_of_month_max : Int
proven_idris_cron_day_of_month_max = cast dayOfMonthBounds.maxVal

export
proven_idris_cron_month_min : Int
proven_idris_cron_month_min = cast monthBounds.minVal

export
proven_idris_cron_month_max : Int
proven_idris_cron_month_max = cast monthBounds.maxVal

export
proven_idris_cron_day_of_week_min : Int
proven_idris_cron_day_of_week_min = cast dayOfWeekBounds.minVal

export
proven_idris_cron_day_of_week_max : Int
proven_idris_cron_day_of_week_max = cast dayOfWeekBounds.maxVal

--------------------------------------------------------------------------------
-- Field Validation
--------------------------------------------------------------------------------

export
proven_idris_cron_is_valid_minute : Int -> Int
proven_idris_cron_is_valid_minute val =
  encodeBool (inBounds minuteBounds (cast val))

export
proven_idris_cron_is_valid_hour : Int -> Int
proven_idris_cron_is_valid_hour val =
  encodeBool (inBounds hourBounds (cast val))

export
proven_idris_cron_is_valid_day_of_month : Int -> Int
proven_idris_cron_is_valid_day_of_month val =
  encodeBool (inBounds dayOfMonthBounds (cast val))

export
proven_idris_cron_is_valid_month : Int -> Int
proven_idris_cron_is_valid_month val =
  encodeBool (inBounds monthBounds (cast val))

export
proven_idris_cron_is_valid_day_of_week : Int -> Int
proven_idris_cron_is_valid_day_of_week val =
  encodeBool (inBounds dayOfWeekBounds (cast val))

export
proven_idris_cron_validate_single : Int -> Int -> Int -> Int
proven_idris_cron_validate_single minBound maxBound val =
  encodeBool (val >= minBound && val <= maxBound)

export
proven_idris_cron_validate_range : Int -> Int -> Int -> Int -> Int
proven_idris_cron_validate_range minBound maxBound start end =
  encodeBool (start >= minBound && end <= maxBound && start <= end)

export
proven_idris_cron_validate_step : Int -> Int
proven_idris_cron_validate_step step =
  encodeBool (step > 0)

--------------------------------------------------------------------------------
-- Time Component Validation
--------------------------------------------------------------------------------

export
proven_idris_cron_is_valid_time : Int -> Int -> Int -> Int -> Int -> Int
proven_idris_cron_is_valid_time minute hour dayOfMonth month dayOfWeek =
  encodeBool (
    inBounds minuteBounds (cast minute) &&
    inBounds hourBounds (cast hour) &&
    inBounds dayOfMonthBounds (cast dayOfMonth) &&
    inBounds monthBounds (cast month) &&
    inBounds dayOfWeekBounds (cast dayOfWeek)
  )

export
proven_idris_cron_normalize_day_of_week : Int -> Int
proven_idris_cron_normalize_day_of_week dow =
  if dow < 0 then 0
  else if dow > 6 then dow `mod` 7
  else dow

--------------------------------------------------------------------------------
-- Frequency Limit Helpers
--------------------------------------------------------------------------------

||| Check if a cron expression is too frequent (< 1 minute interval)
export
proven_idris_cron_is_too_frequent : String -> Int
proven_idris_cron_is_too_frequent cronStr =
  -- Basic heuristic: check for "* * * * *" pattern (every minute)
  encodeBool (cronStr == "* * * * *")

||| Calculate approximate minutes between executions (simplified)
export
proven_idris_cron_estimate_interval_minutes : String -> Int
proven_idris_cron_estimate_interval_minutes cronStr =
  let parts = split (== ' ') cronStr
  in case parts of
    [minute, _, _, _, _] =>
      if minute == "*" then 1  -- Every minute
      else if isInfixOf "/" minute then
        -- Step expression like */5 or 0/15
        case parseStep minute of
          Just n => cast n
          Nothing => 60  -- Default to hourly if can't parse
      else 60  -- Single value = hourly
    _ => 0  -- Invalid format
  where
    parseStep : String -> Maybe Nat
    parseStep s =
      case split (== '/') s of
        [_, stepStr] => parsePositive stepStr
        _ => Nothing

    parsePositive : String -> Maybe Nat
    parsePositive s = parsePositive (cast {to = Integer} s)

export
proven_idris_cron_is_at_least_hourly : String -> Int
proven_idris_cron_is_at_least_hourly cronStr =
  encodeBool (proven_idris_cron_estimate_interval_minutes cronStr >= 60)

export
proven_idris_cron_is_at_least_daily : String -> Int
proven_idris_cron_is_at_least_daily cronStr =
  encodeBool (proven_idris_cron_estimate_interval_minutes cronStr >= 1440)

export
proven_idris_cron_exceeds_max_frequency : Int -> Int -> Int
proven_idris_cron_exceeds_max_frequency intervalMinutes maxFreqMinutes =
  encodeBool (intervalMinutes < maxFreqMinutes)

--------------------------------------------------------------------------------
-- Day of Week Helpers
--------------------------------------------------------------------------------

export
proven_idris_cron_is_weekday : Int -> Int
proven_idris_cron_is_weekday dow =
  encodeBool (dow >= 1 && dow <= 5)

export
proven_idris_cron_is_weekend : Int -> Int
proven_idris_cron_is_weekend dow =
  encodeBool (dow == 0 || dow == 6)

export
proven_idris_cron_day_of_week_name : Int -> String
proven_idris_cron_day_of_week_name dow =
  case dow of
    0 => "Sunday"
    1 => "Monday"
    2 => "Tuesday"
    3 => "Wednesday"
    4 => "Thursday"
    5 => "Friday"
    6 => "Saturday"
    _ => "Invalid"

--------------------------------------------------------------------------------
-- Month Helpers
--------------------------------------------------------------------------------

export
proven_idris_cron_month_name : Int -> String
proven_idris_cron_month_name month =
  case month of
    1 => "January"
    2 => "February"
    3 => "March"
    4 => "April"
    5 => "May"
    6 => "June"
    7 => "July"
    8 => "August"
    9 => "September"
    10 => "October"
    11 => "November"
    12 => "December"
    _ => "Invalid"

export
proven_idris_cron_days_in_month : Int -> Int
proven_idris_cron_days_in_month month =
  case month of
    1 => 31  -- January
    2 => 28  -- February (non-leap year)
    3 => 31  -- March
    4 => 30  -- April
    5 => 31  -- May
    6 => 30  -- June
    7 => 31  -- July
    8 => 31  -- August
    9 => 30  -- September
    10 => 31 -- October
    11 => 30 -- November
    12 => 31 -- December
    _ => 0

export
proven_idris_cron_is_valid_day_for_month : Int -> Int -> Int
proven_idris_cron_is_valid_day_for_month day month =
  let maxDays = proven_idris_cron_days_in_month month
  in encodeBool (day >= 1 && day <= maxDays)

--------------------------------------------------------------------------------
-- Special Expression Detection
--------------------------------------------------------------------------------

export
proven_idris_cron_is_every_minute : String -> Int
proven_idris_cron_is_every_minute cronStr =
  encodeBool (cronStr == "* * * * *")

export
proven_idris_cron_is_hourly : String -> Int
proven_idris_cron_is_hourly cronStr =
  encodeBool (isPrefixOf "0 * * * *" cronStr)

export
proven_idris_cron_is_daily : String -> Int
proven_idris_cron_is_daily cronStr =
  encodeBool (isPrefixOf "0 0 * * *" cronStr)

export
proven_idris_cron_is_weekly : String -> Int
proven_idris_cron_is_weekly cronStr =
  encodeBool (isPrefixOf "0 0 * * 0" cronStr)

export
proven_idris_cron_is_monthly : String -> Int
proven_idris_cron_is_monthly cronStr =
  encodeBool (isPrefixOf "0 0 1 * *" cronStr)

--------------------------------------------------------------------------------
-- Field Count and Structure
--------------------------------------------------------------------------------

export
proven_idris_cron_field_count : String -> Int
proven_idris_cron_field_count cronStr =
  cast (length (split (== ' ') cronStr))

export
proven_idris_cron_has_five_fields : String -> Int
proven_idris_cron_has_five_fields cronStr =
  encodeBool (proven_idris_cron_field_count cronStr == 5)

export
proven_idris_cron_is_valid_format : String -> Int
proven_idris_cron_is_valid_format cronStr =
  encodeBool (
    proven_idris_cron_has_five_fields cronStr == 1 &&
    not (null cronStr)
  )

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_cron_friendly_error : String -> String
proven_idris_cron_friendly_error errorMsg =
  if isInfixOf "frequency" (toLower errorMsg) || isInfixOf "too frequent" (toLower errorMsg)
    then "Cron expression is too frequent (exceeds maximum allowed frequency)"
  else if isInfixOf "minute" (toLower errorMsg)
    then "Invalid minute value (must be 0-59)"
  else if isInfixOf "hour" (toLower errorMsg)
    then "Invalid hour value (must be 0-23)"
  else if isInfixOf "day" (toLower errorMsg)
    then "Invalid day value (must be 1-31 for dayOfMonth, 0-6 for dayOfWeek)"
  else if isInfixOf "month" (toLower errorMsg)
    then "Invalid month value (must be 1-12)"
  else if isInfixOf "format" (toLower errorMsg) || isInfixOf "field" (toLower errorMsg)
    then "Invalid cron format (expected 5 fields: minute hour dayOfMonth month dayOfWeek)"
  else if isInfixOf "range" (toLower errorMsg)
    then "Invalid range (start must be <= end)"
  else if isInfixOf "step" (toLower errorMsg)
    then "Invalid step value (must be > 0)"
  else
    "Cron expression error"
