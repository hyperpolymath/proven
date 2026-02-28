-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- NTP Association Modes (RFC 5905 Section 3)
--
-- NTP defines 8 association modes encoded in a 3-bit field (bits 2-0)
-- of the first octet. Each mode determines the role of the participant
-- in the time synchronisation protocol. The type system ensures only
-- valid mode values can be constructed.

module NTP.Mode

%default total

-- ============================================================================
-- NTP Modes (RFC 5905 Section 3, Figure 1)
-- ============================================================================

||| The 8 NTP association modes as defined in RFC 5905.
||| These occupy bits 2-0 of the first packet octet.
public export
data NTPMode : Type where
  ||| Mode 0: Reserved.
  Reserved          : NTPMode
  ||| Mode 1: Symmetric active (peer-to-peer, initiator).
  SymmetricActive   : NTPMode
  ||| Mode 2: Symmetric passive (peer-to-peer, responder).
  SymmetricPassive  : NTPMode
  ||| Mode 3: Client (request time from server).
  Client            : NTPMode
  ||| Mode 4: Server (respond to client request).
  Server            : NTPMode
  ||| Mode 5: Broadcast (server pushes time to network).
  Broadcast         : NTPMode
  ||| Mode 6: NTP control message (ntpq/ntpdc queries).
  ControlMessage    : NTPMode
  ||| Mode 7: Reserved for private use.
  Private           : NTPMode

public export
Eq NTPMode where
  Reserved         == Reserved         = True
  SymmetricActive  == SymmetricActive  = True
  SymmetricPassive == SymmetricPassive = True
  Client           == Client           = True
  Server           == Server           = True
  Broadcast        == Broadcast        = True
  ControlMessage   == ControlMessage   = True
  Private          == Private          = True
  _                == _                = False

public export
Show NTPMode where
  show Reserved         = "Reserved"
  show SymmetricActive  = "Symmetric Active"
  show SymmetricPassive = "Symmetric Passive"
  show Client           = "Client"
  show Server           = "Server"
  show Broadcast        = "Broadcast"
  show ControlMessage   = "Control Message"
  show Private          = "Private"

-- ============================================================================
-- Numeric code conversion
-- ============================================================================

||| Convert an NTP mode to its 3-bit numeric code.
public export
modeCode : NTPMode -> Bits8
modeCode Reserved         = 0
modeCode SymmetricActive  = 1
modeCode SymmetricPassive = 2
modeCode Client           = 3
modeCode Server           = 4
modeCode Broadcast        = 5
modeCode ControlMessage   = 6
modeCode Private          = 7

||| Decode a 3-bit numeric code to an NTP mode.
||| All values 0-7 are valid, so this function is total without Maybe.
public export
modeFromCode : Bits8 -> NTPMode
modeFromCode 0 = Reserved
modeFromCode 1 = SymmetricActive
modeFromCode 2 = SymmetricPassive
modeFromCode 3 = Client
modeFromCode 4 = Server
modeFromCode 5 = Broadcast
modeFromCode 6 = ControlMessage
modeFromCode 7 = Private
modeFromCode _ = Reserved  -- Values > 7 are impossible in 3 bits; map to Reserved

-- ============================================================================
-- Mode classification and relationships
-- ============================================================================

||| Whether a mode is used for time synchronisation (as opposed to control).
public export
isTimeSyncMode : NTPMode -> Bool
isTimeSyncMode SymmetricActive  = True
isTimeSyncMode SymmetricPassive = True
isTimeSyncMode Client           = True
isTimeSyncMode Server           = True
isTimeSyncMode Broadcast        = True
isTimeSyncMode _                = False

||| The expected response mode for a given request mode.
||| Returns Nothing for modes that do not expect a specific response.
public export
responseMode : NTPMode -> Maybe NTPMode
responseMode SymmetricActive = Just SymmetricPassive
responseMode Client          = Just Server
responseMode _               = Nothing

||| Whether a mode represents an initiator (sends first packet).
public export
isInitiator : NTPMode -> Bool
isInitiator SymmetricActive = True
isInitiator Client          = True
isInitiator Broadcast       = True
isInitiator _               = False

||| Whether a mode represents a responder (replies to initiator).
public export
isResponder : NTPMode -> Bool
isResponder SymmetricPassive = True
isResponder Server           = True
isResponder _                = False

-- ============================================================================
-- Leap Indicator (RFC 5905 Section 7.3)
-- ============================================================================

||| Leap indicator values (bits 7-6 of the first octet).
||| Indicates whether an impending leap second insertion/deletion is scheduled.
public export
data LeapIndicator : Type where
  ||| No warning (no leap second this month).
  NoWarning     : LeapIndicator
  ||| Last minute of the day has 61 seconds (positive leap second).
  LastMinute61  : LeapIndicator
  ||| Last minute of the day has 59 seconds (negative leap second).
  LastMinute59  : LeapIndicator
  ||| Clock is not synchronised (alarm condition).
  Unsynchronised : LeapIndicator

public export
Eq LeapIndicator where
  NoWarning      == NoWarning      = True
  LastMinute61   == LastMinute61   = True
  LastMinute59   == LastMinute59   = True
  Unsynchronised == Unsynchronised = True
  _              == _              = False

public export
Show LeapIndicator where
  show NoWarning      = "No Warning"
  show LastMinute61   = "Last Minute 61s"
  show LastMinute59   = "Last Minute 59s"
  show Unsynchronised = "Unsynchronised"

||| Convert a leap indicator to its 2-bit numeric code.
public export
leapCode : LeapIndicator -> Bits8
leapCode NoWarning      = 0
leapCode LastMinute61   = 1
leapCode LastMinute59   = 2
leapCode Unsynchronised = 3

||| Decode a 2-bit value to a leap indicator.
public export
leapFromCode : Bits8 -> LeapIndicator
leapFromCode 0 = NoWarning
leapFromCode 1 = LastMinute61
leapFromCode 2 = LastMinute59
leapFromCode 3 = Unsynchronised
leapFromCode _ = Unsynchronised  -- Values > 3 impossible in 2 bits
