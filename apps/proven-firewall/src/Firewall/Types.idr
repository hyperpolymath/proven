-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core packet filtering types as closed sum types.
-- | Models firewall rule actions, IP protocols, traffic direction,
-- | match criteria, and connection tracking states.
module Firewall.Types

%default total

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

||| Firewall rule actions applied to matching packets.
public export
data Action : Type where
  Accept   : Action
  Drop     : Action
  Reject   : Action
  Log      : Action
  Redirect : Action

||| Show instance for Action.
export
Show Action where
  show Accept   = "ACCEPT"
  show Drop     = "DROP"
  show Reject   = "REJECT"
  show Log      = "LOG"
  show Redirect = "REDIRECT"

-------------------------------------------------------------------------------
-- Protocols
-------------------------------------------------------------------------------

||| IP protocols for firewall rule matching.
public export
data Protocol : Type where
  TCP    : Protocol
  UDP    : Protocol
  ICMP   : Protocol
  ICMPv6 : Protocol
  Any    : Protocol

||| Show instance for Protocol.
export
Show Protocol where
  show TCP    = "TCP"
  show UDP    = "UDP"
  show ICMP   = "ICMP"
  show ICMPv6 = "ICMPv6"
  show Any    = "ANY"

-------------------------------------------------------------------------------
-- Direction
-------------------------------------------------------------------------------

||| Traffic direction for firewall chain selection.
public export
data Direction : Type where
  Inbound  : Direction
  Outbound : Direction
  Forward  : Direction

||| Show instance for Direction.
export
Show Direction where
  show Inbound  = "INBOUND"
  show Outbound = "OUTBOUND"
  show Forward  = "FORWARD"

-------------------------------------------------------------------------------
-- Rule Match Criteria
-------------------------------------------------------------------------------

||| Match criteria for firewall rules.
||| Each constructor represents a different field to match against.
public export
data RuleMatch : Type where
  SourceAddr : RuleMatch
  DestAddr   : RuleMatch
  SourcePort : RuleMatch
  DestPort   : RuleMatch
  MatchProto : RuleMatch
  Interface  : RuleMatch
  State      : RuleMatch

||| Show instance for RuleMatch.
export
Show RuleMatch where
  show SourceAddr = "SourceAddr"
  show DestAddr   = "DestAddr"
  show SourcePort = "SourcePort"
  show DestPort   = "DestPort"
  show MatchProto = "Protocol"
  show Interface  = "Interface"
  show State      = "State"

-------------------------------------------------------------------------------
-- Connection States
-------------------------------------------------------------------------------

||| Connection tracking states for stateful firewall inspection.
public export
data ConnState : Type where
  New         : ConnState
  Established : ConnState
  Related     : ConnState
  Invalid     : ConnState

||| Show instance for ConnState.
export
Show ConnState where
  show New         = "NEW"
  show Established = "ESTABLISHED"
  show Related     = "RELATED"
  show Invalid     = "INVALID"
