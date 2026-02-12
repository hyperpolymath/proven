-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeDateTime operations
|||
||| This module exports safe date/time operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and handle validation, leap years, timezones.
|||
||| Return conventions:
||| - Date/Time/DateTime → String (ISO 8601 format)
||| - Maybe Date → (status: Int, isoString: String)
||| - Month → Int (1-12)
||| - DayOfWeek → Int (1-7, Monday=1)
||| - Duration → Integer (nanoseconds)
||| - Instant → Integer (epoch milliseconds)
||| - Bool → Int (0 = false, 1 = true)
module Proven.FFI.SafeDateTime

import Proven.SafeDateTime
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

||| Encode Maybe Date as (status, isoString)
encodeDateResult : Maybe Date -> (Int, String)
encodeDateResult Nothing = (1, "")
encodeDateResult (Just d) = (0, show d)

||| Encode Maybe Time as (status, isoString)
encodeTimeResult : Maybe Time -> (Int, String)
encodeTimeResult Nothing = (1, "")
encodeTimeResult (Just t) = (0, show t)

||| Encode DayOfWeek as Int (Monday=1, Sunday=7)
encodeDayOfWeek : DayOfWeek -> Int
encodeDayOfWeek Monday = 1
encodeDayOfWeek Tuesday = 2
encodeDayOfWeek Wednesday = 3
encodeDayOfWeek Thursday = 4
encodeDayOfWeek Friday = 5
encodeDayOfWeek Saturday = 6
encodeDayOfWeek Sunday = 7

||| Decode Int to Nat (clamp negative to 0)
decodeNat : Int -> Nat
decodeNat n = if n < 0 then Z else fromInteger (cast n)

--------------------------------------------------------------------------------
-- Date Construction
--------------------------------------------------------------------------------

export
proven_idris_date_make : Int -> Int -> Int -> (Int, String)
proven_idris_date_make year month day =
  encodeDateResult (makeDate (decodeNat year) (decodeNat month) (decodeNat day))

export
proven_idris_date_from_days : Integer -> (Int, String)
proven_idris_date_from_days days =
  encodeDateResult (dateFromDays days)

--------------------------------------------------------------------------------
-- Time Construction
--------------------------------------------------------------------------------

export
proven_idris_time_make : Int -> Int -> Int -> (Int, String)
proven_idris_time_make hour minute second =
  encodeTimeResult (makeTime (decodeNat hour) (decodeNat minute) (decodeNat second))

export
proven_idris_time_make_nano : Int -> Int -> Int -> Int -> (Int, String)
proven_idris_time_make_nano hour minute second nano =
  encodeTimeResult (makeTimeNano (decodeNat hour) (decodeNat minute) (decodeNat second) (decodeNat nano))

--------------------------------------------------------------------------------
-- Month Operations
--------------------------------------------------------------------------------

export
proven_idris_month_to_nat : Int -> Int
proven_idris_month_to_nat monthNum =
  case natToMonth (decodeNat monthNum) of
    Nothing => 0  -- Invalid month
    Just m => cast (monthToNat m)

export
proven_idris_month_from_nat : Int -> (Int, Int)
proven_idris_month_from_nat n =
  case natToMonth (decodeNat n) of
    Nothing => (1, 0)  -- Error
    Just m => (0, cast (monthToNat m))

--------------------------------------------------------------------------------
-- Date Validation and Utilities
--------------------------------------------------------------------------------

export
proven_idris_date_is_leap_year : Int -> Int
proven_idris_date_is_leap_year year =
  encodeBool (isLeapYear (decodeNat year))

export
proven_idris_date_days_in_month : Int -> Int -> Int
proven_idris_date_days_in_month year monthNum =
  case natToMonth (decodeNat monthNum) of
    Nothing => 0  -- Invalid month
    Just m => cast (daysInMonth (decodeNat year) m)

export
proven_idris_date_day_of_week : Int -> Int -> Int -> Int
proven_idris_date_day_of_week year month day =
  case makeDate (decodeNat year) (decodeNat month) (decodeNat day) of
    Nothing => 0  -- Invalid date
    Just d => encodeDayOfWeek (dayOfWeek d)

export
proven_idris_date_is_weekend : Int -> Int -> Int -> Int
proven_idris_date_is_weekend year month day =
  case makeDate (decodeNat year) (decodeNat month) (decodeNat day) of
    Nothing => 0  -- Invalid date
    Just d => encodeBool (isWeekend d)

export
proven_idris_date_days_since_epoch : Int -> Int -> Int -> (Int, Integer)
proven_idris_date_days_since_epoch year month day =
  case makeDate (decodeNat year) (decodeNat month) (decodeNat day) of
    Nothing => (1, 0)  -- Invalid date
    Just d => (0, daysSinceEpoch d)

--------------------------------------------------------------------------------
-- Instant Operations (Unix Timestamps)
--------------------------------------------------------------------------------

export
proven_idris_instant_from_epoch_second : Integer -> Integer
proven_idris_instant_from_epoch_second secs =
  toEpochMilli (fromEpochSecond secs)

export
proven_idris_instant_from_epoch_milli : Integer -> Integer
proven_idris_instant_from_epoch_milli millis =
  toEpochMilli (fromEpochMilli millis)

export
proven_idris_instant_to_epoch_second : Integer -> Integer
proven_idris_instant_to_epoch_second millis =
  toEpochSecond (fromEpochMilli millis)

export
proven_idris_instant_to_epoch_milli : Integer -> Integer
proven_idris_instant_to_epoch_milli millis =
  millis  -- Identity function (already in millis)

export
proven_idris_instant_plus : Integer -> Integer -> Integer
proven_idris_instant_plus instantMillis durationNanos =
  let inst = fromEpochMilli instantMillis
      dur = MkDuration durationNanos
      result = instantPlus inst dur
  in toEpochMilli result

export
proven_idris_instant_minus : Integer -> Integer -> Integer
proven_idris_instant_minus instantMillis durationNanos =
  let inst = fromEpochMilli instantMillis
      dur = MkDuration durationNanos
      result = instantMinus inst dur
  in toEpochMilli result

export
proven_idris_instant_duration_between : Integer -> Integer -> Integer
proven_idris_instant_duration_between startMillis endMillis =
  let start = fromEpochMilli startMillis
      end = fromEpochMilli endMillis
      MkDuration nanos = durationBetween start end
  in nanos

--------------------------------------------------------------------------------
-- Duration Operations
--------------------------------------------------------------------------------

export
proven_idris_duration_seconds : Integer -> Integer
proven_idris_duration_seconds n =
  let MkDuration nanos = seconds n
  in nanos

export
proven_idris_duration_minutes : Integer -> Integer
proven_idris_duration_minutes n =
  let MkDuration nanos = minutes n
  in nanos

export
proven_idris_duration_hours : Integer -> Integer
proven_idris_duration_hours n =
  let MkDuration nanos = hours n
  in nanos

export
proven_idris_duration_days : Integer -> Integer
proven_idris_duration_days n =
  let MkDuration nanos = days n
  in nanos

export
proven_idris_duration_add : Integer -> Integer -> Integer
proven_idris_duration_add nanos1 nanos2 =
  let MkDuration result = addDuration (MkDuration nanos1) (MkDuration nanos2)
  in result

export
proven_idris_duration_sub : Integer -> Integer -> Integer
proven_idris_duration_sub nanos1 nanos2 =
  let MkDuration result = subDuration (MkDuration nanos1) (MkDuration nanos2)
  in result

export
proven_idris_duration_negate : Integer -> Integer
proven_idris_duration_negate nanos =
  let MkDuration result = negateDuration (MkDuration nanos)
  in result

export
proven_idris_duration_to_seconds : Integer -> Integer
proven_idris_duration_to_seconds nanos =
  toSeconds (MkDuration nanos)

export
proven_idris_duration_to_millis : Integer -> Integer
proven_idris_duration_to_millis nanos =
  toMilliseconds (MkDuration nanos)
