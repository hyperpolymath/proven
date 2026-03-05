-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core STUN/TURN protocol types as closed sum types.
-- | Models message types (RFC 8489 Section 6), transport protocols,
-- | and error codes (Section 14.8).
module STUN.Types

%default total

-------------------------------------------------------------------------------
-- STUN Message Types
-------------------------------------------------------------------------------

||| STUN/TURN message types as defined in RFC 8489 Section 6.
||| Covers binding, allocation, refresh, and data relay operations.
public export
data MessageType : Type where
  BindingRequest      : MessageType
  BindingResponse     : MessageType
  BindingError        : MessageType
  AllocateRequest     : MessageType
  AllocateResponse    : MessageType
  AllocateError       : MessageType
  RefreshRequest      : MessageType
  RefreshResponse     : MessageType
  SendIndication      : MessageType
  DataIndication      : MessageType
  CreatePermission    : MessageType
  ChannelBind         : MessageType

||| Show instance for MessageType.
export
Show MessageType where
  show BindingRequest   = "BindingRequest"
  show BindingResponse  = "BindingResponse"
  show BindingError     = "BindingError"
  show AllocateRequest  = "AllocateRequest"
  show AllocateResponse = "AllocateResponse"
  show AllocateError    = "AllocateError"
  show RefreshRequest   = "RefreshRequest"
  show RefreshResponse  = "RefreshResponse"
  show SendIndication   = "SendIndication"
  show DataIndication   = "DataIndication"
  show CreatePermission = "CreatePermission"
  show ChannelBind      = "ChannelBind"

-------------------------------------------------------------------------------
-- Transport Protocols
-------------------------------------------------------------------------------

||| Transport protocols supported by STUN/TURN.
public export
data TransportProtocol : Type where
  UDP  : TransportProtocol
  TCP  : TransportProtocol
  TLS  : TransportProtocol
  DTLS : TransportProtocol

||| Show instance for TransportProtocol.
export
Show TransportProtocol where
  show UDP  = "UDP"
  show TCP  = "TCP"
  show TLS  = "TLS"
  show DTLS = "DTLS"

-------------------------------------------------------------------------------
-- Error Codes
-------------------------------------------------------------------------------

||| STUN/TURN error codes as defined in RFC 8489 Section 14.8.
public export
data ErrorCode : Type where
  TryAlternate         : ErrorCode
  BadRequest           : ErrorCode
  Unauthorized         : ErrorCode
  Forbidden            : ErrorCode
  MobilityForbidden    : ErrorCode
  StaleNonce           : ErrorCode
  ServerError          : ErrorCode
  InsufficientCapacity : ErrorCode

||| Show instance for ErrorCode, including the numeric error code.
export
Show ErrorCode where
  show TryAlternate         = "300 TryAlternate"
  show BadRequest           = "400 BadRequest"
  show Unauthorized         = "401 Unauthorized"
  show Forbidden            = "403 Forbidden"
  show MobilityForbidden    = "405 MobilityForbidden"
  show StaleNonce           = "438 StaleNonce"
  show ServerError          = "500 ServerError"
  show InsufficientCapacity = "508 InsufficientCapacity"
