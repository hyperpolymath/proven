# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe date and time operations.

import std/[options, times, strutils]

type
  SafeDateTime* = object
    ## A validated date/time value.
    year*: int
    month*: int
    day*: int
    hour*: int
    minute*: int
    second*: int

proc isValidDate*(year, month, day: int): bool =
  ## Check if a date is valid.
  if year < 1 or year > 9999:
    return false
  if month < 1 or month > 12:
    return false
  if day < 1:
    return false

  # Days per month
  let daysInMonth = case month
    of 1, 3, 5, 7, 8, 10, 12: 31
    of 4, 6, 9, 11: 30
    of 2:
      if year mod 4 == 0 and (year mod 100 != 0 or year mod 400 == 0):
        29  # Leap year
      else:
        28
    else: 0

  day <= daysInMonth

proc isValidTime*(hour, minute, second: int): bool =
  ## Check if a time is valid.
  hour >= 0 and hour < 24 and
  minute >= 0 and minute < 60 and
  second >= 0 and second < 60

proc newSafeDateTime*(year, month, day, hour, minute, second: int): Option[SafeDateTime] =
  ## Create a validated SafeDateTime.
  if not isValidDate(year, month, day):
    return none(SafeDateTime)
  if not isValidTime(hour, minute, second):
    return none(SafeDateTime)

  result = some(SafeDateTime(
    year: year,
    month: month,
    day: day,
    hour: hour,
    minute: minute,
    second: second
  ))

proc newSafeDate*(year, month, day: int): Option[SafeDateTime] =
  ## Create a validated date (time is 00:00:00).
  newSafeDateTime(year, month, day, 0, 0, 0)

proc isLeapYear*(year: int): bool =
  ## Check if a year is a leap year.
  year mod 4 == 0 and (year mod 100 != 0 or year mod 400 == 0)

proc daysInMonth*(year, month: int): Option[int] =
  ## Get the number of days in a month.
  if month < 1 or month > 12:
    return none(int)

  let days = case month
    of 1, 3, 5, 7, 8, 10, 12: 31
    of 4, 6, 9, 11: 30
    of 2:
      if isLeapYear(year): 29 else: 28
    else: 0

  result = some(days)

proc dayOfWeek*(dt: SafeDateTime): int =
  ## Get the day of week (0 = Monday, 6 = Sunday).
  ## Uses Zeller's congruence.
  var y = dt.year
  var m = dt.month
  if m < 3:
    m += 12
    y -= 1

  let k = y mod 100
  let j = y div 100
  let h = (dt.day + (13 * (m + 1)) div 5 + k + k div 4 + j div 4 - 2 * j) mod 7
  ((h + 5) mod 7)  # Convert to Monday = 0

proc dayOfYear*(dt: SafeDateTime): int =
  ## Get the day of year (1-366).
  result = dt.day
  for m in 1 ..< dt.month:
    let days = daysInMonth(dt.year, m)
    if days.isSome:
      result += days.get()

proc toTimestamp*(dt: SafeDateTime): int64 =
  ## Convert to Unix timestamp.
  try:
    let t = dateTime(dt.year, Month(dt.month), MonthdayRange(dt.day),
                     HourRange(dt.hour), MinuteRange(dt.minute), SecondRange(dt.second),
                     zone = utc())
    result = t.toTime().toUnix()
  except:
    result = 0

proc fromTimestamp*(ts: int64): Option[SafeDateTime] =
  ## Create from Unix timestamp.
  try:
    let t = fromUnix(ts).utc()
    result = newSafeDateTime(t.year, t.month.ord, t.monthday, t.hour, t.minute, t.second)
  except:
    return none(SafeDateTime)

proc parseIso8601Date*(s: string): Option[SafeDateTime] =
  ## Parse an ISO 8601 date (YYYY-MM-DD).
  if s.len != 10:
    return none(SafeDateTime)
  if s[4] != '-' or s[7] != '-':
    return none(SafeDateTime)

  try:
    let year = parseInt(s[0..3])
    let month = parseInt(s[5..6])
    let day = parseInt(s[8..9])
    result = newSafeDate(year, month, day)
  except:
    return none(SafeDateTime)

proc toIso8601Date*(dt: SafeDateTime): string =
  ## Format as ISO 8601 date (YYYY-MM-DD).
  result = $dt.year
  while result.len < 4:
    result = "0" & result
  result.add("-")
  if dt.month < 10:
    result.add("0")
  result.add($dt.month)
  result.add("-")
  if dt.day < 10:
    result.add("0")
  result.add($dt.day)

proc toIso8601*(dt: SafeDateTime): string =
  ## Format as ISO 8601 datetime (YYYY-MM-DDTHH:MM:SSZ).
  result = toIso8601Date(dt)
  result.add("T")
  if dt.hour < 10:
    result.add("0")
  result.add($dt.hour)
  result.add(":")
  if dt.minute < 10:
    result.add("0")
  result.add($dt.minute)
  result.add(":")
  if dt.second < 10:
    result.add("0")
  result.add($dt.second)
  result.add("Z")

proc addDays*(dt: SafeDateTime, days: int): Option[SafeDateTime] =
  ## Add days to a datetime.
  let ts = toTimestamp(dt) + (days.int64 * 86400)
  fromTimestamp(ts)

proc diffDays*(a, b: SafeDateTime): int64 =
  ## Calculate the difference in days between two datetimes.
  let tsA = toTimestamp(a)
  let tsB = toTimestamp(b)
  (tsA - tsB) div 86400
