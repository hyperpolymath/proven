-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Syslog Transport Types (RFC 5425, RFC 5426, RFC 6587)
--
-- Syslog messages can be transmitted over three transports:
-- UDP/514 (RFC 5426), TCP/514 (RFC 6587), and TLS/6514 (RFC 5425).
-- Each transport has different reliability, ordering, and security
-- guarantees. This module models the transport options and their
-- properties.

module Syslog.Transport

%default total

-- ============================================================================
-- Transport types
-- ============================================================================

||| The three standard syslog transport mechanisms.
public export
data Transport : Type where
  ||| UDP on port 514 (RFC 5426).
  ||| Unreliable, unordered, no encryption. Maximum message size is
  ||| limited by MTU (typically 1472 bytes for Ethernet).
  UDP514  : Transport
  ||| TCP on port 514 (RFC 6587).
  ||| Reliable, ordered delivery. Uses octet-counting or non-transparent
  ||| framing to delimit messages. No encryption.
  TCP514  : Transport
  ||| TLS on port 6514 (RFC 5425).
  ||| Reliable, ordered, encrypted and authenticated delivery.
  ||| Requires X.509 certificates for mutual authentication.
  TLS6514 : Transport

public export
Eq Transport where
  UDP514  == UDP514  = True
  TCP514  == TCP514  = True
  TLS6514 == TLS6514 = True
  _       == _       = False

public export
Show Transport where
  show UDP514  = "UDP/514"
  show TCP514  = "TCP/514"
  show TLS6514 = "TLS/6514"

-- ============================================================================
-- Transport properties
-- ============================================================================

||| The default port number for a transport.
public export
transportPort : Transport -> Bits16
transportPort UDP514  = 514
transportPort TCP514  = 514
transportPort TLS6514 = 6514

||| Whether the transport provides reliable (in-order, guaranteed) delivery.
public export
isReliable : Transport -> Bool
isReliable UDP514  = False
isReliable TCP514  = True
isReliable TLS6514 = True

||| Whether the transport provides encryption for messages in transit.
public export
isEncrypted : Transport -> Bool
isEncrypted UDP514  = False
isEncrypted TCP514  = False
isEncrypted TLS6514 = True

||| Whether the transport provides mutual authentication.
public export
isAuthenticated : Transport -> Bool
isAuthenticated UDP514  = False
isAuthenticated TCP514  = False
isAuthenticated TLS6514 = True

||| The RFC that specifies this transport mechanism.
public export
transportRFC : Transport -> String
transportRFC UDP514  = "RFC 5426"
transportRFC TCP514  = "RFC 6587"
transportRFC TLS6514 = "RFC 5425"

-- ============================================================================
-- Maximum message sizes per transport
-- ============================================================================

||| Maximum message size recommended for a transport.
||| UDP: 2048 bytes (SHOULD support, RFC 5426 Section 3.1)
||| TCP: 480000 bytes (RFC 6587 does not specify a hard limit, but
|||       RFC 5424 recommends supporting at least this)
||| TLS: 480000 bytes (same as TCP, per RFC 5425)
public export
maxMessageSize : Transport -> Nat
maxMessageSize UDP514  = 2048
maxMessageSize TCP514  = 480000
maxMessageSize TLS6514 = 480000

-- ============================================================================
-- TCP framing methods (RFC 6587)
-- ============================================================================

||| Framing methods for syslog over TCP (RFC 6587 Section 3).
public export
data TCPFraming : Type where
  ||| Octet-counting: message length prefix followed by message bytes.
  ||| Example: "85 <165>1 2003-10-11T22:14:15.003Z ..."
  ||| This is the RECOMMENDED method.
  OctetCounting      : TCPFraming
  ||| Non-transparent framing: messages delimited by a trailer byte (LF).
  ||| This is for backwards compatibility with legacy BSD syslog.
  NonTransparent     : TCPFraming

public export
Eq TCPFraming where
  OctetCounting  == OctetCounting  = True
  NonTransparent == NonTransparent = True
  _              == _              = False

public export
Show TCPFraming where
  show OctetCounting  = "Octet-Counting"
  show NonTransparent = "Non-Transparent"

-- ============================================================================
-- Transport configuration
-- ============================================================================

||| Configuration for a syslog transport endpoint.
public export
record TransportConfig where
  constructor MkTransportConfig
  ||| The transport mechanism to use.
  transport     : Transport
  ||| Override port (Nothing = use default for transport).
  port          : Maybe Bits16
  ||| TCP framing method (only relevant for TCP514).
  tcpFraming    : TCPFraming
  ||| Maximum message size to accept (0 = use transport default).
  maxSize       : Nat

||| Create a default transport configuration.
public export
defaultTransportConfig : Transport -> TransportConfig
defaultTransportConfig t = MkTransportConfig
  { transport  = t
  , port       = Nothing
  , tcpFraming = OctetCounting
  , maxSize    = 0
  }

||| Get the effective port for a transport configuration.
||| Uses the override port if set, otherwise the transport default.
public export
effectivePort : TransportConfig -> Bits16
effectivePort cfg =
  case cfg.port of
    Just p  => p
    Nothing => transportPort cfg.transport

||| Get the effective maximum message size for a transport configuration.
||| Uses the override size if non-zero, otherwise the transport default.
public export
effectiveMaxSize : TransportConfig -> Nat
effectiveMaxSize cfg =
  if cfg.maxSize > 0
    then cfg.maxSize
    else maxMessageSize cfg.transport
