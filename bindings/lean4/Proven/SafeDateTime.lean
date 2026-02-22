/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeDateTime - ISO 8601 date/time handling

Provides safe ISO 8601 date/time parsing, formatting, and calendar utilities.
Every operation delegates to the formally verified Idris 2 core via the Zig
FFI bridge (`libproven`). No date/time logic is reimplemented in Lean.

## Supported formats

- `YYYY-MM-DD`
- `YYYY-MM-DDTHH:MM:SS`
- `YYYY-MM-DDTHH:MM:SSZ`
- `YYYY-MM-DDTHH:MM:SS+HH:MM`
-/

import Proven.FFI

namespace Proven.SafeDateTime

open Proven.FFI

/-- Error type for date/time operations. -/
structure DateTimeError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString DateTimeError := ⟨fun e => s!"DateTimeError: {e.status}"⟩

/-- Alias for date/time operation results. -/
abbrev DateTimeResult' (a : Type) := Except DateTimeError a

-- ============================================================================
-- DateTime wrapper
-- ============================================================================

/-- A parsed date/time value. Wraps the FFI `DateTime` structure with a
    friendlier Lean interface. -/
structure ProvenDateTime where
  /-- The underlying FFI datetime. -/
  raw : DateTime
  deriving Repr, BEq, Inhabited

/-- Display a `ProvenDateTime` in ISO 8601 format (approximate, for `ToString`). -/
instance : ToString ProvenDateTime where
  toString dt :=
    let r := dt.raw
    let pad2 (n : Nat) : String :=
      if n < 10 then s!"0{n}" else s!"{n}"
    let datePart := s!"{r.year}-{pad2 r.month.toNat}-{pad2 r.day.toNat}"
    let timePart := s!"{pad2 r.hour.toNat}:{pad2 r.minute.toNat}:{pad2 r.second.toNat}"
    let tzPart :=
      if r.tzOffsetMinutes == 0 then "Z"
      else
        let sign := if r.tzOffsetMinutes > 0 then "+" else "-"
        let absMin := (Int.natAbs r.tzOffsetMinutes.toInt)
        let h := absMin / 60
        let m := absMin % 60
        s!"{sign}{pad2 h}:{pad2 m}"
    s!"{datePart}T{timePart}{tzPart}"

/-- Get the year component. -/
def ProvenDateTime.year (dt : ProvenDateTime) : Int32 := dt.raw.year

/-- Get the month component (1-12). -/
def ProvenDateTime.month (dt : ProvenDateTime) : UInt8 := dt.raw.month

/-- Get the day component (1-31). -/
def ProvenDateTime.day (dt : ProvenDateTime) : UInt8 := dt.raw.day

/-- Get the hour component (0-23). -/
def ProvenDateTime.hour (dt : ProvenDateTime) : UInt8 := dt.raw.hour

/-- Get the minute component (0-59). -/
def ProvenDateTime.minute (dt : ProvenDateTime) : UInt8 := dt.raw.minute

/-- Get the second component (0-59). -/
def ProvenDateTime.second (dt : ProvenDateTime) : UInt8 := dt.raw.second

/-- Get the nanosecond component. -/
def ProvenDateTime.nanosecond (dt : ProvenDateTime) : UInt32 := dt.raw.nanosecond

/-- Get the timezone offset in minutes from UTC. -/
def ProvenDateTime.tzOffsetMinutes (dt : ProvenDateTime) : Int16 := dt.raw.tzOffsetMinutes

-- ============================================================================
-- Parsing
-- ============================================================================

/-- Parse an ISO 8601 date/time string.
    Delegates to `proven_datetime_parse`. -/
def parse (input : String) : IO (DateTimeResult' ProvenDateTime) := do
  let r <- provenDatetimeParse input.toUTF8
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => return .ok { raw := r.datetime }
  | _   => return .error { status := s }

/-- Parse an ISO 8601 date/time string, returning `Option`. -/
def parse? (input : String) : IO (Option ProvenDateTime) := do
  let r <- parse input
  return r.toOption

-- ============================================================================
-- Formatting
-- ============================================================================

/-- Format a `ProvenDateTime` as an ISO 8601 string using the C library.
    This produces the canonical output from the verified core.
    Delegates to `proven_datetime_format_iso8601`. -/
def formatIso8601 (dt : ProvenDateTime) : IO (DateTimeResult' String) := do
  let raw <- provenDatetimeFormatIso8601 dt.raw
  let s := ProvenStatus.ofInt32 raw.status
  match s with
  | .ok => do
    let maybeStr <- marshalStringResult raw
    match maybeStr with
    | some str => return .ok str
    | none     => return .error { status := .errAllocationFailed }
  | _ => return .error { status := s }

/-- Format a `ProvenDateTime` as ISO 8601, returning `Option`. -/
def formatIso8601? (dt : ProvenDateTime) : IO (Option String) := do
  let r <- formatIso8601 dt
  return r.toOption

-- ============================================================================
-- Calendar utilities
-- ============================================================================

/-- Check if a year is a leap year.
    Delegates to `proven_datetime_is_leap_year`. -/
def isLeapYear (year : Int32) : IO Bool :=
  provenDatetimeIsLeapYear year

/-- Get the number of days in a given month for a given year.
    Returns 0 if the month is invalid.
    Delegates to `proven_datetime_days_in_month`. -/
def daysInMonth (year : Int32) (month : UInt8) : IO UInt8 :=
  provenDatetimeDaysInMonth year month

end Proven.SafeDateTime
