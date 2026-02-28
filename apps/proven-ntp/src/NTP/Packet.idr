-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- NTP Packet Structure (RFC 5905 Section 7.3)
--
-- An NTP packet is exactly 48 bytes (without extensions or MAC).
-- All fields have validated ranges: leap indicator (2 bits), version (3 bits),
-- mode (3 bits), stratum (0-16), poll interval (4-17), and so on.
-- Malformed packets produce parse errors — they cannot crash the server.

module NTP.Packet

import NTP.Mode
import NTP.Stratum
import NTP.Timestamp

%default total

-- ============================================================================
-- NTP Packet Record (RFC 5905 Section 7.3, Figure 8)
-- ============================================================================

||| A complete NTP packet as defined in RFC 5905.
||| The packet is 48 bytes on the wire (without optional extensions or MAC).
public export
record NTPPacket where
  constructor MkNTPPacket
  ||| Leap indicator (2 bits): warns of impending leap second.
  leap          : LeapIndicator
  ||| Version number (3 bits): currently 4 for NTPv4.
  version       : Bits8
  ||| Association mode (3 bits): client, server, broadcast, etc.
  mode          : NTPMode
  ||| Stratum level (8 bits): distance from primary reference.
  stratum       : Stratum
  ||| Poll interval (8 bits): log2 seconds between successive polls.
  ||| Valid range: 4 (16s) to 17 (131072s / ~36h).
  poll          : Bits8
  ||| Precision (8 bits): log2 seconds of the system clock precision.
  ||| Typically -18 to -20 for modern systems (~microsecond).
  precision     : Bits8
  ||| Root delay: total round-trip delay to the primary reference.
  ||| Fixed-point: 16 bits integer + 16 bits fraction.
  rootDelay     : Bits32
  ||| Root dispersion: total dispersion to the primary reference.
  ||| Fixed-point: 16 bits integer + 16 bits fraction.
  rootDispersion : Bits32
  ||| Reference identifier: 4 bytes identifying the particular reference.
  ||| For stratum 1: ASCII clock source (GPS, PPS, etc.).
  ||| For stratum 2+: IPv4 address of the reference server.
  referenceId   : Bits32
  ||| Reference timestamp: time the system clock was last set/corrected.
  referenceTime : NTPTimestamp
  ||| Origin timestamp: time the request departed the client (copied from transmit).
  originTime    : NTPTimestamp
  ||| Receive timestamp: time the request arrived at the server.
  receiveTime   : NTPTimestamp
  ||| Transmit timestamp: time the reply departed the server.
  transmitTime  : NTPTimestamp

public export
Show NTPPacket where
  show pkt = "NTPPacket{v=" ++ show (cast {to=Nat} pkt.version)
             ++ ", mode=" ++ show pkt.mode
             ++ ", stratum=" ++ show pkt.stratum
             ++ ", leap=" ++ show pkt.leap
             ++ ", poll=" ++ show (cast {to=Nat} pkt.poll)
             ++ ", xmit=" ++ show pkt.transmitTime
             ++ "}"

-- ============================================================================
-- Packet construction helpers
-- ============================================================================

||| Create a client request packet.
||| Sets version to 4, mode to Client, and all timestamps to null
||| except the transmit timestamp which the caller should set to the
||| current time before sending.
public export
mkClientRequest : (transmitTime : NTPTimestamp) -> NTPPacket
mkClientRequest xmit = MkNTPPacket
  { leap           = NoWarning
  , version        = 4
  , mode           = Client
  , stratum        = Unspecified
  , poll           = 6         -- 64 seconds default
  , precision      = 0         -- Client precision unknown
  , rootDelay      = 0
  , rootDispersion = 0
  , referenceId    = 0
  , referenceTime  = nullTimestamp
  , originTime     = nullTimestamp
  , receiveTime    = nullTimestamp
  , transmitTime   = xmit
  }

||| Create a server response packet from a client request.
||| Copies the client's transmit timestamp into the origin field,
||| fills in server timestamps, stratum, and reference information.
public export
mkServerResponse : (request : NTPPacket)
                -> (serverReceiveTime : NTPTimestamp)
                -> (serverTransmitTime : NTPTimestamp)
                -> (serverStratum : Stratum)
                -> (refId : Bits32)
                -> NTPPacket
mkServerResponse req recvT xmitT strat rid = MkNTPPacket
  { leap           = NoWarning
  , version        = 4
  , mode           = Server
  , stratum        = strat
  , poll           = req.poll
  , precision      = 236        -- ~-20 as unsigned = 236, ~microsecond precision
  , rootDelay      = 0
  , rootDispersion = 0
  , referenceId    = rid
  , referenceTime  = recvT
  , originTime     = req.transmitTime  -- Copy client's xmit time
  , receiveTime    = recvT
  , transmitTime   = xmitT
  }

-- ============================================================================
-- Packet validation
-- ============================================================================

||| Errors that can occur when validating an NTP packet.
public export
data NTPParseError : Type where
  ||| Packet is too short (must be at least 48 bytes).
  PacketTooShort    : (actual : Nat) -> NTPParseError
  ||| NTP version is not supported (only version 3 and 4 accepted).
  UnsupportedVersion : (version : Bits8) -> NTPParseError
  ||| Poll interval is outside the valid range (4-17).
  InvalidPoll       : (poll : Bits8) -> NTPParseError
  ||| Stratum value is in the reserved range (17-255).
  ReservedStratum   : (stratum : Bits8) -> NTPParseError
  ||| Transmit timestamp is zero in a non-KoD packet.
  ZeroTransmit      : NTPParseError

public export
Show NTPParseError where
  show (PacketTooShort n)      = "Packet too short: " ++ show n ++ " bytes (need 48)"
  show (UnsupportedVersion v)  = "Unsupported NTP version: " ++ show (cast {to=Nat} v)
  show (InvalidPoll p)         = "Invalid poll interval: " ++ show (cast {to=Nat} p)
  show (ReservedStratum s)     = "Reserved stratum value: " ++ show (cast {to=Nat} s)
  show ZeroTransmit            = "Transmit timestamp is zero"

||| Validate an NTP packet's field ranges.
||| Returns Right () if the packet is valid, or Left with the first error found.
public export
validatePacket : NTPPacket -> Either NTPParseError ()
validatePacket pkt =
  -- Check version (only 3 and 4 accepted)
  if pkt.version /= 3 && pkt.version /= 4
    then Left (UnsupportedVersion pkt.version)
  -- Check poll interval range for time sync modes
  else if isTimeSyncMode pkt.mode
          && cast {to=Nat} pkt.poll > 0
          && (cast {to=Nat} pkt.poll < 4 || cast {to=Nat} pkt.poll > 17)
    then Left (InvalidPoll pkt.poll)
  -- Check transmit timestamp is non-zero for responses
  else if pkt.mode == Server && isNull pkt.transmitTime
    then Left ZeroTransmit
  else Right ()

-- ============================================================================
-- Kiss-o'-Death (KoD) packets (RFC 5905 Section 7.4)
-- ============================================================================

||| Common Kiss-o'-Death codes returned in the reference ID field
||| when stratum is 0. These tell the client to change its behaviour.
public export
data KoDCode : Type where
  ||| Access denied by server policy.
  DENY : KoDCode
  ||| Access denied due to rate limiting.
  RSTR : KoDCode
  ||| Rate exceeded: reduce poll interval.
  RATE : KoDCode
  ||| Unknown KoD code.
  OtherKoD : (code : String) -> KoDCode

public export
Show KoDCode where
  show DENY = "DENY (Access Denied)"
  show RSTR = "RSTR (Access Restricted)"
  show RATE = "RATE (Rate Exceeded)"
  show (OtherKoD s) = "KoD: " ++ s

||| Check if a packet is a Kiss-o'-Death message.
public export
isKoD : NTPPacket -> Bool
isKoD pkt = pkt.stratum == Unspecified && not (isNull pkt.transmitTime)
