-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeDateTime - Type-safe date and time operations
|||
||| This module provides:
||| - Type-safe date/time representations
||| - Timezone-aware operations
||| - Safe parsing without exceptions
||| - Duration arithmetic with overflow protection
|||
||| Note: Actual implementations may require FFI for system time
module Proven.SafeDateTime

import public Proven.Core
import public Proven.SafeDateTime.Types
import public Proven.SafeDateTime.Parse
import public Proven.SafeDateTime.Zones

import Data.List
import Data.String
import Data.So

%default total

--------------------------------------------------------------------------------
-- Core Date/Time Types
--------------------------------------------------------------------------------

||| Month of the year
public export
data Month : Type where
  January   : Month
  February  : Month
  March     : Month
  April     : Month
  May       : Month
  June      : Month
  July      : Month
  August    : Month
  September : Month
  October   : Month
  November  : Month
  December  : Month

public export
Show Month where
  show January = "January"
  show February = "February"
  show March = "March"
  show April = "April"
  show May = "May"
  show June = "June"
  show July = "July"
  show August = "August"
  show September = "September"
  show October = "October"
  show November = "November"
  show December = "December"

public export
Eq Month where
  January == January = True
  February == February = True
  March == March = True
  April == April = True
  May == May = True
  June == June = True
  July == July = True
  August == August = True
  September == September = True
  October == October = True
  November == November = True
  December == December = True
  _ == _ = False

||| Convert month to its 1-based number
public export
monthToNat : Month -> Nat
monthToNat January = 1
monthToNat February = 2
monthToNat March = 3
monthToNat April = 4
monthToNat May = 5
monthToNat June = 6
monthToNat July = 7
monthToNat August = 8
monthToNat September = 9
monthToNat October = 10
monthToNat November = 11
monthToNat December = 12

||| Convert 1-based number to month
public export
natToMonth : Nat -> Maybe Month
natToMonth 1 = Just January
natToMonth 2 = Just February
natToMonth 3 = Just March
natToMonth 4 = Just April
natToMonth 5 = Just May
natToMonth 6 = Just June
natToMonth 7 = Just July
natToMonth 8 = Just August
natToMonth 9 = Just September
natToMonth 10 = Just October
natToMonth 11 = Just November
natToMonth 12 = Just December
natToMonth _ = Nothing

||| Day of week
public export
data DayOfWeek : Type where
  Monday    : DayOfWeek
  Tuesday   : DayOfWeek
  Wednesday : DayOfWeek
  Thursday  : DayOfWeek
  Friday    : DayOfWeek
  Saturday  : DayOfWeek
  Sunday    : DayOfWeek

public export
Show DayOfWeek where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"

public export
Eq DayOfWeek where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Friday == Friday = True
  Saturday == Saturday = True
  Sunday == Sunday = True
  _ == _ = False

--------------------------------------------------------------------------------
-- Leap Year
--------------------------------------------------------------------------------

||| Check if a year is a leap year
public export
isLeapYear : Nat -> Bool
isLeapYear year =
  (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

||| Days in a month for a given year
public export
daysInMonth : Nat -> Month -> Nat
daysInMonth year February = if isLeapYear year then 29 else 28
daysInMonth _ January = 31
daysInMonth _ March = 31
daysInMonth _ April = 30
daysInMonth _ May = 31
daysInMonth _ June = 30
daysInMonth _ July = 31
daysInMonth _ August = 31
daysInMonth _ September = 30
daysInMonth _ October = 31
daysInMonth _ November = 30
daysInMonth _ December = 31

--------------------------------------------------------------------------------
-- Date Type (validated via smart constructors)
--------------------------------------------------------------------------------

||| A date in the proleptic Gregorian calendar.
||| Fields are plain Nat — use makeDate for validated construction.
public export
record Date where
  constructor MkDate
  year  : Nat
  month : Month
  day   : Nat

||| Construct date safely (validates ranges)
public export
makeDate : (year : Nat) -> (monthNum : Nat) -> (day : Nat) -> Maybe Date
makeDate year monthNum day = do
  month <- natToMonth monthNum
  if year >= 1 && year <= 9999 && day >= 1 && day <= daysInMonth year month
    then Just (MkDate year month day)
    else Nothing

||| Helper for internal padded display
padZero : Nat -> String
padZero n = if n < 10 then "0" ++ show n else show n

public export
Show Date where
  show d = show d.year ++ "-" ++ padZero (monthToNat d.month) ++ "-" ++ padZero d.day

public export
Eq Date where
  d1 == d2 = d1.year == d2.year && d1.month == d2.month && d1.day == d2.day

public export
Ord Date where
  compare d1 d2 =
    case compare d1.year d2.year of
      EQ => case compare (monthToNat d1.month) (monthToNat d2.month) of
              EQ => compare d1.day d2.day
              other => other
      other => other

--------------------------------------------------------------------------------
-- Time Type (validated via smart constructors)
--------------------------------------------------------------------------------

||| A time of day. Use makeTime for validated construction.
public export
record Time where
  constructor MkTime
  hour       : Nat
  minute     : Nat
  second     : Nat
  nanosecond : Nat

||| Construct time safely
public export
makeTime : (hour : Nat) -> (minute : Nat) -> (second : Nat) -> Maybe Time
makeTime hour minute second =
  if hour < 24 && minute < 60 && second < 60
    then Just (MkTime hour minute second 0)
    else Nothing

||| Construct time with nanoseconds
public export
makeTimeNano : (hour : Nat) -> (minute : Nat) -> (second : Nat) -> (nano : Nat) -> Maybe Time
makeTimeNano hour minute second nano =
  if hour < 24 && minute < 60 && second < 60 && nano < 1000000000
    then Just (MkTime hour minute second nano)
    else Nothing

public export
Show Time where
  show t = padZero t.hour ++ ":" ++ padZero t.minute ++ ":" ++ padZero t.second

public export
Eq Time where
  t1 == t2 = t1.hour == t2.hour && t1.minute == t2.minute &&
             t1.second == t2.second && t1.nanosecond == t2.nanosecond

public export
Ord Time where
  compare t1 t2 =
    case compare t1.hour t2.hour of
      EQ => case compare t1.minute t2.minute of
              EQ => case compare t1.second t2.second of
                      EQ => compare t1.nanosecond t2.nanosecond
                      other => other
              other => other
      other => other

--------------------------------------------------------------------------------
-- DateTime Type
--------------------------------------------------------------------------------

||| Combined date and time (timezone-naive)
public export
record DateTime where
  constructor MkDateTime
  date : Date
  time : Time

||| Construct datetime safely
public export
makeDateTime : Date -> Time -> DateTime
makeDateTime = MkDateTime

public export
Show DateTime where
  show dt = show dt.date ++ "T" ++ show dt.time

public export
Eq DateTime where
  dt1 == dt2 = dt1.date == dt2.date && dt1.time == dt2.time

public export
Ord DateTime where
  compare dt1 dt2 =
    case compare dt1.date dt2.date of
      EQ => compare dt1.time dt2.time
      other => other

--------------------------------------------------------------------------------
-- Zoned DateTime
--------------------------------------------------------------------------------

||| DateTime with timezone information
public export
record ZonedDateTime where
  constructor MkZonedDateTime
  datetime      : DateTime
  timezone      : Timezone
  offsetSeconds : Integer

public export
Show ZonedDateTime where
  show zdt = show zdt.datetime ++ formatOffset zdt.offsetSeconds
    where
      formatOffset : Integer -> String
      formatOffset 0 = "Z"
      formatOffset secs =
        let absSeconds = abs secs
            hrs = absSeconds `div` 3600
            mins = (absSeconds `mod` 3600) `div` 60
            sign = if secs >= 0 then "+" else "-"
        in sign ++ padZero (cast hrs) ++ ":" ++ padZero (cast mins)

--------------------------------------------------------------------------------
-- Duration Type
--------------------------------------------------------------------------------

||| Duration between two points in time (nanosecond precision)
public export
record Duration where
  constructor MkDuration
  totalNanoseconds : Integer

||| Create duration from components
public export
makeDuration : (days : Integer) -> (hours : Integer) -> (minutes : Integer) ->
               (seconds : Integer) -> (nanos : Integer) -> Duration
makeDuration d h m s n =
  MkDuration $ d * 86400000000000 + h * 3600000000000 +
               m * 60000000000 + s * 1000000000 + n

||| Duration in seconds
public export
seconds : Integer -> Duration
seconds n = MkDuration (n * 1000000000)

||| Duration in minutes
public export
minutes : Integer -> Duration
minutes n = seconds (n * 60)

||| Duration in hours
public export
hours : Integer -> Duration
hours n = minutes (n * 60)

||| Duration in days
public export
days : Integer -> Duration
days n = hours (n * 24)

||| Add durations
public export
addDuration : Duration -> Duration -> Duration
addDuration (MkDuration a) (MkDuration b) = MkDuration (a + b)

||| Subtract durations
public export
subDuration : Duration -> Duration -> Duration
subDuration (MkDuration a) (MkDuration b) = MkDuration (a - b)

||| Negate duration
public export
negateDuration : Duration -> Duration
negateDuration (MkDuration n) = MkDuration (negate n)

||| Duration to seconds
public export
toSeconds : Duration -> Integer
toSeconds (MkDuration n) = n `div` 1000000000

||| Duration to milliseconds
public export
toMilliseconds : Duration -> Integer
toMilliseconds (MkDuration n) = n `div` 1000000

public export
Show Duration where
  show (MkDuration nanos) =
    let absNanos = abs nanos
        secs = absNanos `div` 1000000000
        d = secs `div` 86400
        h = (secs `mod` 86400) `div` 3600
        m = (secs `mod` 3600) `div` 60
        s = secs `mod` 60
        sign = if nanos < 0 then "-" else ""
    in sign ++ "P" ++
       (if d > 0 then show d ++ "D" else "") ++
       "T" ++
       (if h > 0 then show h ++ "H" else "") ++
       (if m > 0 then show m ++ "M" else "") ++
       show s ++ "S"

public export
Eq Duration where
  (MkDuration a) == (MkDuration b) = a == b

public export
Ord Duration where
  compare (MkDuration a) (MkDuration b) = compare a b

--------------------------------------------------------------------------------
-- Instant (Unix timestamp)
--------------------------------------------------------------------------------

||| Unix timestamp (seconds + nanoseconds since 1970-01-01T00:00:00Z)
public export
record Instant where
  constructor MkInstant
  epochSecond : Integer
  nano        : Nat

||| Current time (stub — requires FFI for real system clock)
public export
now : IO Instant
now = pure $ MkInstant 0 0

||| Create instant from epoch seconds
public export
fromEpochSecond : Integer -> Instant
fromEpochSecond secs = MkInstant secs 0

||| Create instant from epoch milliseconds
public export
fromEpochMilli : Integer -> Instant
fromEpochMilli millis =
  MkInstant (millis `div` 1000) (cast ((millis `mod` 1000) * 1000000))

||| Get epoch seconds
public export
toEpochSecond : Instant -> Integer
toEpochSecond = epochSecond

||| Get epoch milliseconds
public export
toEpochMilli : Instant -> Integer
toEpochMilli inst = inst.epochSecond * 1000 + cast (inst.nano `div` 1000000)

||| Add duration to instant
public export
instantPlus : Instant -> Duration -> Instant
instantPlus inst (MkDuration nanos) =
  let totalNanos = cast inst.epochSecond * 1000000000 + cast inst.nano + nanos
      newSecs = totalNanos `div` 1000000000
      newNanos = totalNanos `mod` 1000000000
  in MkInstant newSecs (cast (abs newNanos))

||| Subtract duration from instant
public export
instantMinus : Instant -> Duration -> Instant
instantMinus inst dur = instantPlus inst (negateDuration dur)

||| Duration between two instants
public export
durationBetween : Instant -> Instant -> Duration
durationBetween start end =
  let startNanos = cast start.epochSecond * 1000000000 + cast start.nano
      endNanos = cast end.epochSecond * 1000000000 + cast end.nano
  in MkDuration (endNanos - startNanos)

public export
Eq Instant where
  i1 == i2 = i1.epochSecond == i2.epochSecond && i1.nano == i2.nano

public export
Ord Instant where
  compare i1 i2 =
    case compare i1.epochSecond i2.epochSecond of
      EQ => compare i1.nano i2.nano
      other => other

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Get day of week for a date (Zeller's congruence)
public export
dayOfWeek : Date -> DayOfWeek
dayOfWeek d =
  let y = if monthToNat d.month < 3 then d.year `minus` 1 else d.year
      m = if monthToNat d.month < 3 then monthToNat d.month + 12 else monthToNat d.month
      k = y `mod` 100
      j = y `div` 100
      q = d.day
      h = (q + (13 * (m + 1)) `div` 5 + k + k `div` 4 + j `div` 4 + 5 * j) `mod` 7
  in case h of
       0 => Saturday
       1 => Sunday
       2 => Monday
       3 => Tuesday
       4 => Wednesday
       5 => Thursday
       _ => Friday

||| Check if date is weekend
public export
isWeekend : Date -> Bool
isWeekend d = case dayOfWeek d of
  Saturday => True
  Sunday => True
  _ => False

||| Days since epoch (1970-01-01)
public export
daysSinceEpoch : Date -> Integer
daysSinceEpoch d =
  let y = cast d.year - (if monthToNat d.month <= 2 then 1 else 0)
      era = (if y >= 0 then y else y - 399) `div` 400
      yoe = y - era * 400
      m = cast (monthToNat d.month)
      doy = (153 * (m + (if m > 2 then -3 else 9)) + 2) `div` 5 + cast d.day - 1
      doe = yoe * 365 + yoe `div` 4 - yoe `div` 100 + doy
  in era * 146097 + doe - 719468

||| Date from days since epoch
public export
dateFromDays : Integer -> Maybe Date
dateFromDays numDays =
  let z = numDays + 719468
      era = (if z >= 0 then z else z - 146096) `div` 146097
      doe = z - era * 146097
      yoe = (doe - doe `div` 1460 + doe `div` 36524 - doe `div` 146096) `div` 365
      y = yoe + era * 400
      doy = doe - (365 * yoe + yoe `div` 4 - yoe `div` 100)
      mp = (5 * doy + 2) `div` 153
      d = doy - (153 * mp + 2) `div` 5 + 1
      m = mp + (if mp < 10 then 3 else -9)
      yr = y + (if m <= 2 then 1 else 0)
  in makeDate (cast yr) (cast m) (cast d)
