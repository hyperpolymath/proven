-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Syslog Priority Value (RFC 5424 Section 6.2.1)
--
-- The priority value (PRI) is a single integer that encodes both the
-- facility and severity. It is calculated as: facility * 8 + severity.
-- The valid range is 0-191 (24 facilities x 8 severities).
-- This module provides encode/decode with validated range checks.

module Syslog.Priority

import Syslog.Facility
import Syslog.Severity

%default total

-- ============================================================================
-- Priority value
-- ============================================================================

||| A syslog priority value combining facility and severity.
||| The value is in the range 0-191, encoded as facility * 8 + severity.
public export
record Priority where
  constructor MkPriority
  ||| The facility that generated the message.
  facility : Facility
  ||| The severity of the message.
  severity : Severity

public export
Eq Priority where
  a == b = a.facility == b.facility && a.severity == b.severity

public export
Show Priority where
  show p = "<" ++ show (priorityValue p) ++ "> "
           ++ show p.facility ++ "." ++ show p.severity

-- ============================================================================
-- Priority encoding
-- ============================================================================

||| Calculate the numeric priority value from a Priority record.
||| Formula: facility_code * 8 + severity_code (RFC 5424 Section 6.2.1).
public export
priorityValue : Priority -> Nat
priorityValue p = facilityCode p.facility * 8 + severityCode p.severity

||| The maximum valid priority value (facility=23 * 8 + severity=7 = 191).
public export
maxPriority : Nat
maxPriority = 191

-- ============================================================================
-- Priority decoding
-- ============================================================================

||| Decode a numeric priority value into facility and severity.
||| Returns Nothing if the value exceeds 191 or the facility code is invalid.
public export
decodePriority : Nat -> Maybe Priority
decodePriority n =
  if n > maxPriority then Nothing
  else
    let facCode = div n 8
        sevCode = mod n 8
    in case (facilityFromCode facCode, severityFromCode sevCode) of
         (Just fac, Just sev) => Just (MkPriority fac sev)
         _                    => Nothing

-- ============================================================================
-- Priority construction helpers
-- ============================================================================

||| Create a priority from a facility and severity.
public export
mkPriority : Facility -> Severity -> Priority
mkPriority = MkPriority

||| Format a priority value as the RFC 5424 PRI field: "<N>".
public export
formatPRI : Priority -> String
formatPRI p = "<" ++ show (priorityValue p) ++ ">"

||| Parse a PRI field from a string like "<34>".
||| Returns Nothing if the format is invalid or the value is out of range.
public export
parsePRI : String -> Maybe Priority
parsePRI s =
  let chars = unpack s
  in case chars of
       ('<' :: rest) =>
         case span (/= '>') rest of
           (digits, '>' :: _) =>
             case parsePositive {a=Nat} (pack digits) of
               Just n  => decodePriority n
               Nothing => Nothing
           _ => Nothing
       _ => Nothing

-- ============================================================================
-- Priority classification
-- ============================================================================

||| Whether this priority represents an alarm condition.
||| Delegates to severity's isAlarm check.
public export
isPriorityAlarm : Priority -> Bool
isPriorityAlarm p = isAlarm p.severity

||| Whether this priority represents an error condition.
||| Delegates to severity's isError check.
public export
isPriorityError : Priority -> Bool
isPriorityError p = isError p.severity

||| All possible priority values for a given facility (one per severity).
public export
facilitySeverities : Facility -> List Priority
facilitySeverities fac =
  [ MkPriority fac Emergency
  , MkPriority fac Alert
  , MkPriority fac Critical
  , MkPriority fac Error
  , MkPriority fac Warning
  , MkPriority fac Notice
  , MkPriority fac Informational
  , MkPriority fac Debug
  ]
