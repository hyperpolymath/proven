-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Syslog Severity Levels (RFC 5424 Section 6.2.1)
--
-- Syslog defines 8 severity levels from Emergency (most severe) to
-- Debug (least severe). These form the lower 3 bits of the priority
-- value. The Ord instance reflects the operational urgency — Emergency
-- compares as greater than Debug.

module Syslog.Severity

%default total

-- ============================================================================
-- Syslog Severity Levels (RFC 5424 Section 6.2.1, Table 2)
-- ============================================================================

||| The 8 syslog severity levels as defined in RFC 5424.
||| Ordered from most severe (Emergency) to least severe (Debug).
public export
data Severity : Type where
  ||| Severity 0: System is unusable.
  Emergency     : Severity
  ||| Severity 1: Action must be taken immediately.
  Alert         : Severity
  ||| Severity 2: Critical conditions.
  Critical      : Severity
  ||| Severity 3: Error conditions.
  Error         : Severity
  ||| Severity 4: Warning conditions.
  Warning       : Severity
  ||| Severity 5: Normal but significant condition.
  Notice        : Severity
  ||| Severity 6: Informational messages.
  Informational : Severity
  ||| Severity 7: Debug-level messages.
  Debug         : Severity

public export
Eq Severity where
  Emergency     == Emergency     = True
  Alert         == Alert         = True
  Critical      == Critical      = True
  Error         == Error         = True
  Warning       == Warning       = True
  Notice        == Notice        = True
  Informational == Informational = True
  Debug         == Debug         = True
  _             == _             = False

public export
Show Severity where
  show Emergency     = "emerg"
  show Alert         = "alert"
  show Critical      = "crit"
  show Error         = "err"
  show Warning       = "warning"
  show Notice        = "notice"
  show Informational = "info"
  show Debug         = "debug"

||| Ordering by urgency: Emergency > Alert > ... > Debug.
||| Higher urgency compares as GT to support filtering by minimum severity.
public export
Ord Severity where
  compare Emergency     Emergency     = EQ
  compare Emergency     _             = GT
  compare Alert         Emergency     = LT
  compare Alert         Alert         = EQ
  compare Alert         _             = GT
  compare Critical      Emergency     = LT
  compare Critical      Alert         = LT
  compare Critical      Critical      = EQ
  compare Critical      _             = GT
  compare Error         Debug         = GT
  compare Error         Informational = GT
  compare Error         Notice        = GT
  compare Error         Warning       = GT
  compare Error         Error         = EQ
  compare Error         _             = LT
  compare Warning       Debug         = GT
  compare Warning       Informational = GT
  compare Warning       Notice        = GT
  compare Warning       Warning       = EQ
  compare Warning       _             = LT
  compare Notice        Debug         = GT
  compare Notice        Informational = GT
  compare Notice        Notice        = EQ
  compare Notice        _             = LT
  compare Informational Debug         = GT
  compare Informational Informational = EQ
  compare Informational _             = LT
  compare Debug         Debug         = EQ
  compare Debug         _             = LT

-- ============================================================================
-- Numeric code conversion
-- ============================================================================

||| Convert a severity to its numeric code (0-7).
public export
severityCode : Severity -> Nat
severityCode Emergency     = 0
severityCode Alert         = 1
severityCode Critical      = 2
severityCode Error         = 3
severityCode Warning       = 4
severityCode Notice        = 5
severityCode Informational = 6
severityCode Debug         = 7

||| Decode a numeric code to a severity.
||| Returns Nothing for codes outside the valid range (0-7).
public export
severityFromCode : Nat -> Maybe Severity
severityFromCode 0 = Just Emergency
severityFromCode 1 = Just Alert
severityFromCode 2 = Just Critical
severityFromCode 3 = Just Error
severityFromCode 4 = Just Warning
severityFromCode 5 = Just Notice
severityFromCode 6 = Just Informational
severityFromCode 7 = Just Debug
severityFromCode _ = Nothing

-- ============================================================================
-- Severity classification and filtering
-- ============================================================================

||| Human-readable description of the severity level.
public export
severityDescription : Severity -> String
severityDescription Emergency     = "System is unusable"
severityDescription Alert         = "Action must be taken immediately"
severityDescription Critical      = "Critical conditions"
severityDescription Error         = "Error conditions"
severityDescription Warning       = "Warning conditions"
severityDescription Notice        = "Normal but significant condition"
severityDescription Informational = "Informational messages"
severityDescription Debug         = "Debug-level messages"

||| Whether a message at this severity should trigger an alarm/page.
||| Emergency, Alert, and Critical severities are considered alarm-worthy.
public export
isAlarm : Severity -> Bool
isAlarm Emergency = True
isAlarm Alert     = True
isAlarm Critical  = True
isAlarm _         = False

||| Whether a message at this severity indicates an error condition.
||| Emergency through Error (0-3) are error conditions.
public export
isError : Severity -> Bool
isError Emergency = True
isError Alert     = True
isError Critical  = True
isError Error     = True
isError _         = False

||| Check whether a message severity passes a minimum severity filter.
||| A message passes if its severity is >= the minimum (more urgent or equal).
||| Example: meetsMinSeverity Warning Error => True (Warning is more severe)
public export
meetsMinSeverity : (minSeverity : Severity) -> (messageSeverity : Severity) -> Bool
meetsMinSeverity minSev msgSev = msgSev >= minSev
