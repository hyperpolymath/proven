-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- DHCP.Types: Core protocol types for DHCP (RFC 2131).
--
-- Defines closed sum types for DHCP message types (DISCOVER through DECLINE),
-- option codes for the most common DHCP options, and hardware address types
-- used in the htype field of DHCP messages.

module DHCP.Types

%default total

-- ============================================================================
-- DHCP message types (RFC 2131 Section 3.1, option 53)
-- ============================================================================

||| DHCP message types as defined in RFC 2131.
||| These correspond to the value carried in DHCP option 53 and determine
||| the role of each message in the DORA (Discover-Offer-Request-Ack) exchange.
public export
data MessageType : Type where
  ||| Client broadcasts to locate available servers (step 1 of DORA).
  Discover : MessageType
  ||| Server responds to DISCOVER with configuration offer (step 2).
  Offer    : MessageType
  ||| Client requests offered parameters from a chosen server (step 3).
  Request  : MessageType
  ||| Server confirms address assignment to client (step 4).
  Ack      : MessageType
  ||| Server rejects the client request (address no longer available).
  Nak      : MessageType
  ||| Client relinquishes its IP address before lease expiry.
  Release  : MessageType
  ||| Client requests local configuration parameters only (no address).
  Inform   : MessageType
  ||| Client has detected the offered address is already in use (ARP probe).
  Decline  : MessageType

public export
Eq MessageType where
  Discover == Discover = True
  Offer    == Offer    = True
  Request  == Request  = True
  Ack      == Ack      = True
  Nak      == Nak      = True
  Release  == Release  = True
  Inform   == Inform   = True
  Decline  == Decline  = True
  _        == _        = False

public export
Show MessageType where
  show Discover = "DHCPDISCOVER"
  show Offer    = "DHCPOFFER"
  show Request  = "DHCPREQUEST"
  show Ack      = "DHCPACK"
  show Nak      = "DHCPNAK"
  show Release  = "DHCPRELEASE"
  show Inform   = "DHCPINFORM"
  show Decline  = "DHCPDECLINE"

-- ============================================================================
-- DHCP option codes (RFC 2132)
-- ============================================================================

||| Common DHCP option codes from RFC 2132.
||| Each option is identified by a single-byte code and carries configuration
||| data in the variable-length options field of a DHCP message.
public export
data OptionCode : Type where
  ||| Option 1: Subnet mask for the client's network.
  SubnetMask  : OptionCode
  ||| Option 3: Default gateway/router address.
  Router      : OptionCode
  ||| Option 6: DNS server addresses.
  DNS         : OptionCode
  ||| Option 15: Domain name for client hostname resolution.
  DomainName  : OptionCode
  ||| Option 51: IP address lease duration in seconds.
  LeaseTime   : OptionCode
  ||| Option 54: Identifier of the DHCP server making the offer.
  ServerID    : OptionCode
  ||| Option 50: IP address requested by the client.
  RequestedIP : OptionCode
  ||| Option 53: DHCP message type (see MessageType).
  MsgType     : OptionCode

public export
Eq OptionCode where
  SubnetMask  == SubnetMask  = True
  Router      == Router      = True
  DNS         == DNS         = True
  DomainName  == DomainName  = True
  LeaseTime   == LeaseTime   = True
  ServerID    == ServerID    = True
  RequestedIP == RequestedIP = True
  MsgType     == MsgType     = True
  _           == _           = False

public export
Show OptionCode where
  show SubnetMask  = "SubnetMask(1)"
  show Router      = "Router(3)"
  show DNS         = "DNS(6)"
  show DomainName  = "DomainName(15)"
  show LeaseTime   = "LeaseTime(51)"
  show ServerID    = "ServerID(54)"
  show RequestedIP = "RequestedIP(50)"
  show MsgType     = "MessageType(53)"

-- ============================================================================
-- Hardware types (RFC 1700 / IANA)
-- ============================================================================

||| Hardware address types for the htype field (RFC 1700).
||| Determines how the chaddr (client hardware address) field is interpreted.
public export
data HardwareType : Type where
  ||| Ethernet (10 Mb), htype = 1.
  Ethernet   : HardwareType
  ||| IEEE 802 networks, htype = 6.
  IEEE802    : HardwareType
  ||| Arcnet, htype = 7.
  Arcnet     : HardwareType
  ||| Frame Relay, htype = 15.
  FrameRelay : HardwareType

public export
Eq HardwareType where
  Ethernet   == Ethernet   = True
  IEEE802    == IEEE802    = True
  Arcnet     == Arcnet     = True
  FrameRelay == FrameRelay = True
  _          == _          = False

public export
Show HardwareType where
  show Ethernet   = "Ethernet(1)"
  show IEEE802    = "IEEE802(6)"
  show Arcnet     = "Arcnet(7)"
  show FrameRelay = "FrameRelay(15)"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All DHCP message types.
public export
allMessageTypes : List MessageType
allMessageTypes = [Discover, Offer, Request, Ack, Nak, Release, Inform, Decline]

||| All option codes.
public export
allOptionCodes : List OptionCode
allOptionCodes = [SubnetMask, Router, DNS, DomainName, LeaseTime,
                  ServerID, RequestedIP, MsgType]

||| All hardware types.
public export
allHardwareTypes : List HardwareType
allHardwareTypes = [Ethernet, IEEE802, Arcnet, FrameRelay]
