-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for RFC 1928 SOCKS5 proxy.
-- | Defines authentication methods, commands, address types, reply codes,
-- | and connection states as closed sum types with Show instances.

module SOCKS.Types

%default total

||| SOCKS5 authentication methods per RFC 1928 Section 3.
public export
data AuthMethod : Type where
  NoAuth           : AuthMethod
  GSSAPI           : AuthMethod
  UsernamePassword : AuthMethod
  NoAcceptable     : AuthMethod

public export
Show AuthMethod where
  show NoAuth           = "NoAuth"
  show GSSAPI           = "GSSAPI"
  show UsernamePassword = "UsernamePassword"
  show NoAcceptable     = "NoAcceptable"

||| SOCKS5 commands per RFC 1928 Section 4.
public export
data Command : Type where
  Connect      : Command
  Bind         : Command
  UDPAssociate : Command

public export
Show Command where
  show Connect      = "Connect"
  show Bind         = "Bind"
  show UDPAssociate = "UDPAssociate"

||| SOCKS5 address types per RFC 1928 Section 4.
public export
data AddressType : Type where
  IPv4       : AddressType
  DomainName : AddressType
  IPv6       : AddressType

public export
Show AddressType where
  show IPv4       = "IPv4"
  show DomainName = "DomainName"
  show IPv6       = "IPv6"

||| SOCKS5 reply codes per RFC 1928 Section 6.
public export
data Reply : Type where
  Succeeded              : Reply
  GeneralFailure         : Reply
  NotAllowed             : Reply
  NetworkUnreachable     : Reply
  HostUnreachable        : Reply
  ConnectionRefused      : Reply
  TTLExpired             : Reply
  CommandNotSupported    : Reply
  AddressTypeNotSupported : Reply

public export
Show Reply where
  show Succeeded               = "Succeeded"
  show GeneralFailure          = "GeneralFailure"
  show NotAllowed              = "NotAllowed"
  show NetworkUnreachable      = "NetworkUnreachable"
  show HostUnreachable         = "HostUnreachable"
  show ConnectionRefused       = "ConnectionRefused"
  show TTLExpired              = "TTLExpired"
  show CommandNotSupported     = "CommandNotSupported"
  show AddressTypeNotSupported = "AddressTypeNotSupported"

||| SOCKS5 connection state machine.
public export
data State : Type where
  Initial        : State
  Authenticating : State
  Authenticated  : State
  Connecting     : State
  Established    : State
  Closed         : State

public export
Show State where
  show Initial        = "Initial"
  show Authenticating = "Authenticating"
  show Authenticated  = "Authenticated"
  show Connecting     = "Connecting"
  show Established    = "Established"
  show Closed         = "Closed"
