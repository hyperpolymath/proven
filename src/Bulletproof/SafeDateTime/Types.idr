-- SPDX-License-Identifier: Palimpsest-MPL
||| Extended DateTime Types
|||
||| Additional date/time related types and utilities.
module Bulletproof.SafeDateTime.Types

import Bulletproof.Core
import Data.List

%default total

--------------------------------------------------------------------------------
-- Period Type (Calendar-based duration)
--------------------------------------------------------------------------------

||| A period of time in calendar terms (years, months, days)
public export
record Period where
  constructor MkPeriod
  years : Integer
  months : Integer
  days : Integer

||| Zero period
public export
zeroPeriod : Period
zeroPeriod = MkPeriod 0 0 0

||| Create period from years
public export
ofYears : Integer -> Period
ofYears n = MkPeriod n 0 0

||| Create period from months
public export
ofMonths : Integer -> Period
ofMonths n = MkPeriod 0 n 0

||| Create period from days
public export
ofDays : Integer -> Period
ofDays n = MkPeriod 0 0 n

||| Add periods
public export
addPeriod : Period -> Period -> Period
addPeriod p1 p2 = MkPeriod
  (p1.years + p2.years)
  (p1.months + p2.months)
  (p1.days + p2.days)

||| Negate period
public export
negatePeriod : Period -> Period
negatePeriod p = MkPeriod (negate p.years) (negate p.months) (negate p.days)

||| Normalize period (convert months > 12 to years)
public export
normalizePeriod : Period -> Period
normalizePeriod p =
  let totalMonths = p.years * 12 + p.months
      years = totalMonths `div` 12
      months = totalMonths `mod` 12
  in MkPeriod years months p.days

public export
Show Period where
  show p = "P" ++
    (if p.years /= 0 then show p.years ++ "Y" else "") ++
    (if p.months /= 0 then show p.months ++ "M" else "") ++
    (if p.days /= 0 then show p.days ++ "D" else "")

--------------------------------------------------------------------------------
-- Year-Month Type
--------------------------------------------------------------------------------

||| Year and month combination
public export
record YearMonth where
  constructor MkYearMonth
  year : Nat
  month : Nat
  {auto validYear : (year >= 1, year <= 9999)}
  {auto validMonth : (month >= 1, month <= 12)}

||| Create year-month safely
public export
makeYearMonth : Nat -> Nat -> Maybe YearMonth
makeYearMonth y m =
  if y >= 1 && y <= 9999 && m >= 1 && m <= 12
    then Just (MkYearMonth y m)
    else Nothing

public export
Show YearMonth where
  show ym = show ym.year ++ "-" ++ (if ym.month < 10 then "0" else "") ++ show ym.month

public export
Eq YearMonth where
  ym1 == ym2 = ym1.year == ym2.year && ym1.month == ym2.month

public export
Ord YearMonth where
  compare ym1 ym2 =
    case compare ym1.year ym2.year of
      EQ => compare ym1.month ym2.month
      other => other

--------------------------------------------------------------------------------
-- Month-Day Type
--------------------------------------------------------------------------------

||| Month and day combination (for recurring dates)
public export
record MonthDay where
  constructor MkMonthDay
  month : Nat
  day : Nat
  {auto validMonth : (month >= 1, month <= 12)}
  {auto validDay : day >= 1}  -- Max depends on month

||| Create month-day safely
public export
makeMonthDay : Nat -> Nat -> Maybe MonthDay
makeMonthDay month day =
  if month >= 1 && month <= 12 && day >= 1 && day <= maxDay month
    then Just (MkMonthDay month day)
    else Nothing
  where
    maxDay : Nat -> Nat
    maxDay 2 = 29  -- Allow Feb 29 for leap years
    maxDay m = if m `elem` [4, 6, 9, 11] then 30 else 31

public export
Show MonthDay where
  show md = "--" ++ (if md.month < 10 then "0" else "") ++ show md.month ++
            "-" ++ (if md.day < 10 then "0" else "") ++ show md.day

--------------------------------------------------------------------------------
-- Date Range Type
--------------------------------------------------------------------------------

||| A range of dates (inclusive)
public export
record DateRange where
  constructor MkDateRange
  startYear : Nat
  startMonth : Nat
  startDay : Nat
  endYear : Nat
  endMonth : Nat
  endDay : Nat

||| Check if a date tuple is within range
public export
inRange : DateRange -> (Nat, Nat, Nat) -> Bool
inRange range (y, m, d) =
  let start = (range.startYear, range.startMonth, range.startDay)
      end = (range.endYear, range.endMonth, range.endDay)
      date = (y, m, d)
  in compareTuple start date /= GT && compareTuple date end /= GT
  where
    compareTuple : (Nat, Nat, Nat) -> (Nat, Nat, Nat) -> Ordering
    compareTuple (y1, m1, d1) (y2, m2, d2) =
      case compare y1 y2 of
        EQ => case compare m1 m2 of
                EQ => compare d1 d2
                other => other
        other => other

--------------------------------------------------------------------------------
-- Time Interval Type
--------------------------------------------------------------------------------

||| An interval between two instants
public export
record TimeInterval where
  constructor MkInterval
  startEpoch : Integer
  endEpoch : Integer
  {auto valid : startEpoch <= endEpoch}

||| Create interval safely
public export
makeInterval : Integer -> Integer -> Maybe TimeInterval
makeInterval start end =
  if start <= end
    then Just (MkInterval start end)
    else Nothing

||| Duration of interval in seconds
public export
intervalDuration : TimeInterval -> Integer
intervalDuration i = i.endEpoch - i.startEpoch

||| Check if instant is within interval
public export
containsInstant : TimeInterval -> Integer -> Bool
containsInstant i epoch = epoch >= i.startEpoch && epoch <= i.endEpoch

||| Check if intervals overlap
public export
overlaps : TimeInterval -> TimeInterval -> Bool
overlaps i1 i2 = i1.startEpoch <= i2.endEpoch && i2.startEpoch <= i1.endEpoch

||| Gap between intervals (Nothing if they overlap)
public export
gap : TimeInterval -> TimeInterval -> Maybe Integer
gap i1 i2 =
  if overlaps i1 i2
    then Nothing
    else Just $ if i1.endEpoch < i2.startEpoch
                  then i2.startEpoch - i1.endEpoch
                  else i1.startEpoch - i2.endEpoch

--------------------------------------------------------------------------------
-- Recurrence Types
--------------------------------------------------------------------------------

||| Frequency for recurring events
public export
data Frequency
  = Daily
  | Weekly
  | BiWeekly
  | Monthly
  | Quarterly
  | Yearly

public export
Show Frequency where
  show Daily = "DAILY"
  show Weekly = "WEEKLY"
  show BiWeekly = "BIWEEKLY"
  show Monthly = "MONTHLY"
  show Quarterly = "QUARTERLY"
  show Yearly = "YEARLY"

||| Recurrence rule
public export
record RecurrenceRule where
  constructor MkRecurrence
  frequency : Frequency
  interval : Nat
  count : Maybe Nat      -- Number of occurrences
  until : Maybe Integer  -- End epoch

--------------------------------------------------------------------------------
-- Clock and Calendar Types
--------------------------------------------------------------------------------

||| Clock type (12-hour or 24-hour)
public export
data ClockType = Clock12 | Clock24

||| AM/PM indicator for 12-hour clock
public export
data Meridiem = AM | PM

public export
Show Meridiem where
  show AM = "AM"
  show PM = "PM"

||| Convert 24-hour to 12-hour time
public export
to12Hour : Nat -> (Nat, Meridiem)
to12Hour hour =
  if hour == 0 then (12, AM)
  else if hour < 12 then (hour, AM)
  else if hour == 12 then (12, PM)
  else (hour `minus` 12, PM)

||| Convert 12-hour to 24-hour time
public export
to24Hour : Nat -> Meridiem -> Nat
to24Hour 12 AM = 0
to24Hour 12 PM = 12
to24Hour hour AM = hour
to24Hour hour PM = hour + 12

--------------------------------------------------------------------------------
-- Era and Calendar System
--------------------------------------------------------------------------------

||| Era for date representation
public export
data Era = BCE | CE

public export
Show Era where
  show BCE = "BCE"
  show CE = "CE"

||| Calendar system
public export
data CalendarSystem
  = Gregorian
  | Julian
  | ISO8601

public export
Show CalendarSystem where
  show Gregorian = "Gregorian"
  show Julian = "Julian"
  show ISO8601 = "ISO-8601"
