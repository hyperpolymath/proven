-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- IDS.Types : Core types for an Intrusion Detection/Prevention System.
-- Defines detection modes, response actions, protocol identifiers,
-- traffic directions, threat levels, and rule match criteria.

module IDS.Types

%default total

---------------------------------------------------------------------------
-- DetectionMode : Engine detection strategy.
---------------------------------------------------------------------------

||| Strategy the IDS engine uses to identify threats.
public export
data DetectionMode : Type where
  Signature : DetectionMode
  Anomaly   : DetectionMode
  Hybrid    : DetectionMode

export
Show DetectionMode where
  show Signature = "Signature"
  show Anomaly   = "Anomaly"
  show Hybrid    = "Hybrid"

---------------------------------------------------------------------------
-- Action : Response action when a rule fires.
---------------------------------------------------------------------------

||| Action taken by the IDS/IPS when a detection rule matches.
public export
data Action : Type where
  Alert  : Action
  Drop   : Action
  Reject : Action
  Log    : Action
  Pass   : Action

export
Show Action where
  show Alert  = "Alert"
  show Drop   = "Drop"
  show Reject = "Reject"
  show Log    = "Log"
  show Pass   = "Pass"

---------------------------------------------------------------------------
-- Protocol : Network protocol identifiers for rule matching.
---------------------------------------------------------------------------

||| Network protocols the IDS can inspect.
public export
data Protocol : Type where
  TCP  : Protocol
  UDP  : Protocol
  ICMP : Protocol
  DNS  : Protocol
  HTTP : Protocol
  TLS  : Protocol
  SSH  : Protocol

export
Show Protocol where
  show TCP  = "TCP"
  show UDP  = "UDP"
  show ICMP = "ICMP"
  show DNS  = "DNS"
  show HTTP = "HTTP"
  show TLS  = "TLS"
  show SSH  = "SSH"

---------------------------------------------------------------------------
-- Direction : Traffic direction for rule scope.
---------------------------------------------------------------------------

||| Direction of network traffic a rule applies to.
public export
data Direction : Type where
  Inbound  : Direction
  Outbound : Direction
  Both     : Direction

export
Show Direction where
  show Inbound  = "Inbound"
  show Outbound = "Outbound"
  show Both     = "Both"

---------------------------------------------------------------------------
-- ThreatLevel : Severity of a detected threat.
---------------------------------------------------------------------------

||| Assessed severity of a detected threat.
public export
data ThreatLevel : Type where
  TLInfo     : ThreatLevel
  TLLow      : ThreatLevel
  TLMedium   : ThreatLevel
  TLHigh     : ThreatLevel
  TLCritical : ThreatLevel

export
Show ThreatLevel where
  show TLInfo     = "Info"
  show TLLow      = "Low"
  show TLMedium   = "Medium"
  show TLHigh     = "High"
  show TLCritical = "Critical"

---------------------------------------------------------------------------
-- RuleMatch : Criteria for matching packets against rules.
---------------------------------------------------------------------------

||| Packet inspection criteria for rule matching.
public export
data RuleMatch : Type where
  SrcAddr   : RuleMatch
  DstAddr   : RuleMatch
  SrcPort   : RuleMatch
  DstPort   : RuleMatch
  Content   : RuleMatch
  Regex     : RuleMatch
  Threshold : RuleMatch

export
Show RuleMatch where
  show SrcAddr   = "SrcAddr"
  show DstAddr   = "DstAddr"
  show SrcPort   = "SrcPort"
  show DstPort   = "DstPort"
  show Content   = "Content"
  show Regex     = "Regex"
  show Threshold = "Threshold"
