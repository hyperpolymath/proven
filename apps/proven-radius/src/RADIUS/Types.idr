-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- RADIUS.Types: Core protocol types for RADIUS (RFC 2865).
--
-- Defines closed sum types for RADIUS packet types (Access-Request through
-- Access-Challenge), attribute types for the most common RADIUS attributes,
-- and service type values used in Service-Type attribute (RFC 2865 Section 5.6).

module RADIUS.Types

%default total

-- ============================================================================
-- RADIUS packet types (RFC 2865 Section 3)
-- ============================================================================

||| RADIUS packet types from RFC 2865 Section 3.
||| The Code field in a RADIUS packet header identifies the type of packet.
public export
data PacketType : Type where
  ||| Client requests authentication (Code 1).
  AccessRequest      : PacketType
  ||| Server grants access (Code 2).
  AccessAccept       : PacketType
  ||| Server denies access (Code 3).
  AccessReject       : PacketType
  ||| Client sends accounting data (Code 4, RFC 2866).
  AccountingRequest  : PacketType
  ||| Server acknowledges accounting data (Code 5, RFC 2866).
  AccountingResponse : PacketType
  ||| Server requests additional authentication data (Code 11).
  AccessChallenge    : PacketType

public export
Eq PacketType where
  AccessRequest      == AccessRequest      = True
  AccessAccept       == AccessAccept       = True
  AccessReject       == AccessReject       = True
  AccountingRequest  == AccountingRequest  = True
  AccountingResponse == AccountingResponse = True
  AccessChallenge    == AccessChallenge    = True
  _                  == _                  = False

public export
Show PacketType where
  show AccessRequest      = "Access-Request(1)"
  show AccessAccept       = "Access-Accept(2)"
  show AccessReject       = "Access-Reject(3)"
  show AccountingRequest  = "Accounting-Request(4)"
  show AccountingResponse = "Accounting-Response(5)"
  show AccessChallenge    = "Access-Challenge(11)"

-- ============================================================================
-- RADIUS attribute types (RFC 2865 Section 5)
-- ============================================================================

||| Common RADIUS attribute types from RFC 2865 Section 5.
||| Each attribute is a Type-Length-Value (TLV) triple carried in a
||| RADIUS packet.
public export
data AttributeType : Type where
  ||| Name of the user to be authenticated (Type 1).
  UserName       : AttributeType
  ||| User's password, encrypted with shared secret (Type 2).
  UserPassword   : AttributeType
  ||| IP address of the NAS originating the request (Type 4).
  NASIPAddress   : AttributeType
  ||| Physical port number of the NAS authenticating the user (Type 5).
  NASPort        : AttributeType
  ||| Type of service requested or granted (Type 6).
  ServiceTy      : AttributeType
  ||| Framing protocol for framed access (Type 7).
  FramedProtocol : AttributeType
  ||| IP address to be configured for the user (Type 8).
  FramedIPAddr   : AttributeType
  ||| Maximum session duration in seconds (Type 27).
  SessionTimeout : AttributeType
  ||| Text to be displayed to the user (Type 18).
  ReplyMessage   : AttributeType

public export
Eq AttributeType where
  UserName       == UserName       = True
  UserPassword   == UserPassword   = True
  NASIPAddress   == NASIPAddress   = True
  NASPort        == NASPort        = True
  ServiceTy      == ServiceTy      = True
  FramedProtocol == FramedProtocol = True
  FramedIPAddr   == FramedIPAddr   = True
  SessionTimeout == SessionTimeout = True
  ReplyMessage   == ReplyMessage   = True
  _              == _              = False

public export
Show AttributeType where
  show UserName       = "User-Name(1)"
  show UserPassword   = "User-Password(2)"
  show NASIPAddress   = "NAS-IP-Address(4)"
  show NASPort        = "NAS-Port(5)"
  show ServiceTy      = "Service-Type(6)"
  show FramedProtocol = "Framed-Protocol(7)"
  show FramedIPAddr   = "Framed-IP-Address(8)"
  show SessionTimeout = "Session-Timeout(27)"
  show ReplyMessage   = "Reply-Message(18)"

-- ============================================================================
-- Service type values (RFC 2865 Section 5.6)
-- ============================================================================

||| Service-Type attribute values from RFC 2865 Section 5.6.
||| Indicates the type of service the user has requested or that is
||| being provided.
public export
data ServiceType : Type where
  ||| Login to a host (Value 1).
  Login          : ServiceType
  ||| Framed protocol (e.g., PPP, SLIP) (Value 2).
  Framed         : ServiceType
  ||| Callback login service (Value 3).
  CallbackLogin  : ServiceType
  ||| Callback framed service (Value 4).
  CallbackFramed : ServiceType
  ||| Outbound user (Value 5).
  Outbound       : ServiceType
  ||| Administrative access to the NAS (Value 6).
  Administrative : ServiceType

public export
Eq ServiceType where
  Login          == Login          = True
  Framed         == Framed         = True
  CallbackLogin  == CallbackLogin  = True
  CallbackFramed == CallbackFramed = True
  Outbound       == Outbound       = True
  Administrative == Administrative = True
  _              == _              = False

public export
Show ServiceType where
  show Login          = "Login(1)"
  show Framed         = "Framed(2)"
  show CallbackLogin  = "Callback-Login(3)"
  show CallbackFramed = "Callback-Framed(4)"
  show Outbound       = "Outbound(5)"
  show Administrative = "Administrative(6)"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All RADIUS packet types.
public export
allPacketTypes : List PacketType
allPacketTypes = [AccessRequest, AccessAccept, AccessReject,
                  AccountingRequest, AccountingResponse, AccessChallenge]

||| All attribute types.
public export
allAttributeTypes : List AttributeType
allAttributeTypes = [UserName, UserPassword, NASIPAddress, NASPort,
                     ServiceTy, FramedProtocol, FramedIPAddr,
                     SessionTimeout, ReplyMessage]

||| All service types.
public export
allServiceTypes : List ServiceType
allServiceTypes = [Login, Framed, CallbackLogin, CallbackFramed,
                   Outbound, Administrative]
