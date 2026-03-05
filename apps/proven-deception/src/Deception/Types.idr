-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-deception: Core protocol types for deception/decoy server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Deception.Types

%default total

-- ============================================================================
-- DecoyType
-- ============================================================================

||| The category of decoy asset deployed to attract attackers.
public export
data DecoyType : Type where
  ||| A fake network service (SSH, HTTP, SMB, etc.).
  Service    : DecoyType
  ||| A planted credential pair (username/password, API key).
  Credential : DecoyType
  ||| A honeypot file seeded with canary data.
  File       : DecoyType
  ||| A decoy network segment or host.
  Network    : DecoyType
  ||| A trackable token embedded in documents or systems.
  Token      : DecoyType
  ||| A trail of breadcrumbs leading attackers toward monitored assets.
  Breadcrumb : DecoyType

export
Show DecoyType where
  show Service    = "Service"
  show Credential = "Credential"
  show File       = "File"
  show Network    = "Network"
  show Token      = "Token"
  show Breadcrumb = "Breadcrumb"

-- ============================================================================
-- TriggerEvent
-- ============================================================================

||| The kind of interaction that triggers a decoy alert.
public export
data TriggerEvent : Type where
  ||| Any access attempt on the decoy.
  Access  : TriggerEvent
  ||| A login attempt using planted credentials.
  Login   : TriggerEvent
  ||| A read operation on a honeypot file.
  Read    : TriggerEvent
  ||| A write or modification attempt.
  Write   : TriggerEvent
  ||| An execution attempt on a decoy binary or script.
  Execute : TriggerEvent
  ||| A network scan that touches decoy addresses.
  Scan    : TriggerEvent

export
Show TriggerEvent where
  show Access  = "Access"
  show Login   = "Login"
  show Read    = "Read"
  show Write   = "Write"
  show Execute = "Execute"
  show Scan    = "Scan"

-- ============================================================================
-- AlertPriority
-- ============================================================================

||| Priority level assigned to a deception alert.
public export
data AlertPriority : Type where
  ||| Low confidence or low-impact interaction.
  Low      : AlertPriority
  ||| Moderate confidence, warrants investigation.
  Medium   : AlertPriority
  ||| High confidence, likely malicious activity.
  High     : AlertPriority
  ||| Critical -- active attacker confirmed, immediate response needed.
  Critical : AlertPriority

export
Show AlertPriority where
  show Low      = "Low"
  show Medium   = "Medium"
  show High     = "High"
  show Critical = "Critical"

-- ============================================================================
-- DecoyState
-- ============================================================================

||| Lifecycle state of a deployed decoy.
public export
data DecoyState : Type where
  ||| Decoy is deployed and monitoring for interactions.
  Active    : DecoyState
  ||| Decoy has been triggered by an attacker.
  Triggered : DecoyState
  ||| Decoy has been manually disabled.
  Disabled  : DecoyState
  ||| Decoy has passed its expiration time.
  Expired   : DecoyState

export
Show DecoyState where
  show Active    = "Active"
  show Triggered = "Triggered"
  show Disabled  = "Disabled"
  show Expired   = "Expired"

-- ============================================================================
-- ResponseAction
-- ============================================================================

||| Automated response action taken when a decoy is triggered.
public export
data ResponseAction : Type where
  ||| Send an alert notification to the security team.
  Alert       : ResponseAction
  ||| Redirect the attacker to a deeper honeypot.
  Redirect    : ResponseAction
  ||| Introduce latency to slow the attacker.
  Delay       : ResponseAction
  ||| Capture attacker fingerprint (tools, techniques, origin).
  Fingerprint : ResponseAction
  ||| Isolate the attacker's source from production networks.
  Isolate     : ResponseAction

export
Show ResponseAction where
  show Alert       = "Alert"
  show Redirect    = "Redirect"
  show Delay       = "Delay"
  show Fingerprint = "Fingerprint"
  show Isolate     = "Isolate"
