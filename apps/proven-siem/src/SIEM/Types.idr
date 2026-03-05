-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SIEM.Types : Core types for Security Information and Event Management.
-- Defines event severities, categories, correlation rule types,
-- and alert lifecycle states.

module SIEM.Types

%default total

---------------------------------------------------------------------------
-- EventSeverity : Severity classification for security events.
---------------------------------------------------------------------------

||| Severity level assigned to an ingested security event.
public export
data EventSeverity : Type where
  Info     : EventSeverity
  Low      : EventSeverity
  Medium   : EventSeverity
  High     : EventSeverity
  Critical : EventSeverity

export
Show EventSeverity where
  show Info     = "Info"
  show Low      = "Low"
  show Medium   = "Medium"
  show High     = "High"
  show Critical = "Critical"

---------------------------------------------------------------------------
-- EventCategory : Broad classification of security event sources.
---------------------------------------------------------------------------

||| Category of the security event for correlation and reporting.
public export
data EventCategory : Type where
  Authentication    : EventCategory
  NetworkTraffic    : EventCategory
  FileActivity      : EventCategory
  ProcessExecution  : EventCategory
  PolicyViolation   : EventCategory
  Malware           : EventCategory
  DataExfiltration  : EventCategory

export
Show EventCategory where
  show Authentication   = "Authentication"
  show NetworkTraffic   = "NetworkTraffic"
  show FileActivity     = "FileActivity"
  show ProcessExecution = "ProcessExecution"
  show PolicyViolation  = "PolicyViolation"
  show Malware          = "Malware"
  show DataExfiltration = "DataExfiltration"

---------------------------------------------------------------------------
-- CorrelationRule : Types of event correlation strategies.
---------------------------------------------------------------------------

||| Strategy used by the correlation engine to detect complex threats.
public export
data CorrelationRule : Type where
  Threshold   : CorrelationRule
  Sequence    : CorrelationRule
  Aggregation : CorrelationRule
  Absence     : CorrelationRule
  Statistical : CorrelationRule

export
Show CorrelationRule where
  show Threshold   = "Threshold"
  show Sequence    = "Sequence"
  show Aggregation = "Aggregation"
  show Absence     = "Absence"
  show Statistical = "Statistical"

---------------------------------------------------------------------------
-- AlertState : Lifecycle state of a SIEM alert.
---------------------------------------------------------------------------

||| Current state of an alert in its lifecycle.
public export
data AlertState : Type where
  New           : AlertState
  Acknowledged  : AlertState
  InProgress    : AlertState
  Resolved      : AlertState
  FalsePositive : AlertState

export
Show AlertState where
  show New           = "New"
  show Acknowledged  = "Acknowledged"
  show InProgress    = "InProgress"
  show Resolved      = "Resolved"
  show FalsePositive = "FalsePositive"
