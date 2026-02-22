# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe date and time operations.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  SafeDateTime* = object
    ## A validated date/time value.
    year*: int32
    month*: uint8
    day*: uint8
    hour*: uint8
    minute*: uint8
    second*: uint8
    nanosecond*: uint32
    tzOffsetMinutes*: int16

proc parseIso8601*(s: string): Option[SafeDateTime] =
  ## Parse an ISO 8601 date/time string.
  ## Returns None if the string is not valid.
  if s.len == 0:
    return none(SafeDateTime)
  let res = provenDatetimeParse(unsafeAddr s[0], csize_t(s.len))
  if res.status == PROVEN_OK:
    return some(SafeDateTime(
      year: res.datetime.year,
      month: res.datetime.month,
      day: res.datetime.day,
      hour: res.datetime.hour,
      minute: res.datetime.minute,
      second: res.datetime.second,
      nanosecond: res.datetime.nanosecond,
      tzOffsetMinutes: res.datetime.tz_offset_minutes
    ))
  none(SafeDateTime)

proc formatIso8601*(dt: SafeDateTime): Option[string] =
  ## Format a SafeDateTime as an ISO 8601 string.
  ## Returns None if formatting fails.
  let pdt = ProvenDateTime(
    year: dt.year,
    month: dt.month,
    day: dt.day,
    hour: dt.hour,
    minute: dt.minute,
    second: dt.second,
    nanosecond: dt.nanosecond,
    tz_offset_minutes: dt.tzOffsetMinutes
  )
  let res = provenDatetimeFormatIso8601(pdt)
  if res.status == PROVEN_OK and res.value != nil:
    let formatted = $res.value
    provenFreeString(res.value)
    return some(formatted)
  none(string)

proc isLeapYear*(year: int32): bool =
  ## Check if a year is a leap year.
  provenDatetimeIsLeapYear(year)

proc daysInMonth*(year: int32, month: uint8): uint8 =
  ## Get the number of days in a month.
  ## Returns 0 for invalid month values.
  provenDatetimeDaysInMonth(year, month)
