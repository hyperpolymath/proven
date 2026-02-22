// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe-datetime.kk -- Safe ISO 8601 date/time operations via libproven FFI.
//
// Provides date/time parsing, formatting, and calendar utilities.
// All computation is performed by the Idris 2 verified implementation.
// No date/time logic is reimplemented in Koka.
module proven/safe-datetime

import std/num/int64
import std/num/int32
import proven/lib-proven

// ============================================================================
// DateTime type
// ============================================================================

// Parsed datetime components.
pub struct datetime-components
  year: int
  month: int       // 1-12
  day: int         // 1-31
  hour: int        // 0-23
  minute: int      // 0-59
  second: int      // 0-59
  nanosecond: int
  tz-offset-minutes: int  // 0 for UTC, negative for west of UTC

// ============================================================================
// Raw FFI declarations
// ============================================================================

// Parse ISO 8601 date string and extract year.
extern raw-parse-year(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenDateTimeResult r = proven_datetime_parse((const uint8_t*)#1, (size_t)#2); (r.status == 0) ? r.datetime.year : INT32_MIN; })"

extern raw-parse-month(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenDateTimeResult r = proven_datetime_parse((const uint8_t*)#1, (size_t)#2); (r.status == 0) ? (int32_t)r.datetime.month : -1; })"

extern raw-parse-day(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenDateTimeResult r = proven_datetime_parse((const uint8_t*)#1, (size_t)#2); (r.status == 0) ? (int32_t)r.datetime.day : -1; })"

extern raw-parse-hour(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenDateTimeResult r = proven_datetime_parse((const uint8_t*)#1, (size_t)#2); (r.status == 0) ? (int32_t)r.datetime.hour : -1; })"

extern raw-parse-minute(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenDateTimeResult r = proven_datetime_parse((const uint8_t*)#1, (size_t)#2); (r.status == 0) ? (int32_t)r.datetime.minute : -1; })"

extern raw-parse-second(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenDateTimeResult r = proven_datetime_parse((const uint8_t*)#1, (size_t)#2); (r.status == 0) ? (int32_t)r.datetime.second : -1; })"

extern raw-parse-status(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenDateTimeResult r = proven_datetime_parse((const uint8_t*)#1, (size_t)#2); r.status; })"

extern raw-parse-tz-offset(ptr: intptr_t, len: int64): int32
  c inline "({ ProvenDateTimeResult r = proven_datetime_parse((const uint8_t*)#1, (size_t)#2); (r.status == 0) ? (int32_t)r.datetime.tz_offset_minutes : 0; })"

// Check if a year is a leap year.
extern raw-is-leap-year(year: int32): bool
  c inline "proven_datetime_is_leap_year(#1)"

// Get number of days in a month.
extern raw-days-in-month(year: int32, month: int32): int32
  c inline "(int32_t)proven_datetime_days_in_month(#1, (uint8_t)#2)"

// ============================================================================
// Safe public API
// ============================================================================

// Parse an ISO 8601 datetime string.
// Supports: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, YYYY-MM-DDTHH:MM:SSZ,
// YYYY-MM-DDTHH:MM:SS+HH:MM
// Returns Nothing on parse failure.
pub fun parse-datetime(input: string): <ndet> maybe<datetime-components>
  val status = raw-parse-status(input.cptr, input.count.int64)
  if status == 0i32 then
    Just(Datetime-components(
      raw-parse-year(input.cptr, input.count.int64).int,
      raw-parse-month(input.cptr, input.count.int64).int,
      raw-parse-day(input.cptr, input.count.int64).int,
      raw-parse-hour(input.cptr, input.count.int64).int,
      raw-parse-minute(input.cptr, input.count.int64).int,
      raw-parse-second(input.cptr, input.count.int64).int,
      0,  // nanosecond (extracted separately if needed)
      raw-parse-tz-offset(input.cptr, input.count.int64).int
    ))
  else Nothing

// Check if a year is a leap year.
pub fun is-leap-year(year: int): <ndet> bool
  raw-is-leap-year(year.int32)

// Get the number of days in a month for a given year.
// Returns 0 for invalid months.
pub fun days-in-month(year: int, month: int): <ndet> int
  raw-days-in-month(year.int32, month.int32).int
