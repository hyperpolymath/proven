-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for IEEE 1588 Precision Time Protocol.
-- | Defines message types, clock classes, port states, and delay mechanisms
-- | as closed sum types with Show instances.

module PTP.Types

%default total

||| PTP message types as defined in IEEE 1588-2008 Section 13.3.
public export
data MessageType : Type where
  Sync                : MessageType
  DelayReq            : MessageType
  PdelayReq           : MessageType
  PdelayResp          : MessageType
  FollowUp            : MessageType
  DelayResp           : MessageType
  PdelayRespFollowUp  : MessageType
  Announce            : MessageType
  Signaling           : MessageType
  Management          : MessageType

public export
Show MessageType where
  show Sync               = "Sync"
  show DelayReq           = "DelayReq"
  show PdelayReq          = "PdelayReq"
  show PdelayResp         = "PdelayResp"
  show FollowUp           = "FollowUp"
  show DelayResp          = "DelayResp"
  show PdelayRespFollowUp = "PdelayRespFollowUp"
  show Announce           = "Announce"
  show Signaling          = "Signaling"
  show Management         = "Management"

||| Clock quality classification per IEEE 1588-2008 Section 7.6.2.4.
public export
data ClockClass : Type where
  PrimaryClock         : ClockClass
  ApplicationSpecific  : ClockClass
  SlaveOnly            : ClockClass
  DefaultClass         : ClockClass

public export
Show ClockClass where
  show PrimaryClock        = "PrimaryClock"
  show ApplicationSpecific = "ApplicationSpecific"
  show SlaveOnly           = "SlaveOnly"
  show DefaultClass        = "DefaultClass"

||| Port state machine states per IEEE 1588-2008 Section 9.2.5.
public export
data PortState : Type where
  Initializing  : PortState
  Faulty        : PortState
  Disabled      : PortState
  Listening     : PortState
  PreMaster     : PortState
  Master        : PortState
  Passive       : PortState
  Uncalibrated  : PortState
  Slave         : PortState

public export
Show PortState where
  show Initializing = "Initializing"
  show Faulty       = "Faulty"
  show Disabled     = "Disabled"
  show Listening    = "Listening"
  show PreMaster    = "PreMaster"
  show Master       = "Master"
  show Passive      = "Passive"
  show Uncalibrated = "Uncalibrated"
  show Slave        = "Slave"

||| Delay measurement mechanism per IEEE 1588-2008 Section 8.2.5.
public export
data DelayMechanism : Type where
  E2E       : DelayMechanism
  P2P       : DelayMechanism
  DMDisabled : DelayMechanism

public export
Show DelayMechanism where
  show E2E        = "E2E"
  show P2P        = "P2P"
  show DMDisabled = "Disabled"
