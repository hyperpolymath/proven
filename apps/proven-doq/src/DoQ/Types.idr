-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core DNS over QUIC types as closed sum types.
-- | Models QUIC stream types, error codes, and session states
-- | per RFC 9250.
module DoQ.Types

%default total

-------------------------------------------------------------------------------
-- Stream Types
-------------------------------------------------------------------------------

||| QUIC stream types used in DNS over QUIC (RFC 9250 Section 4).
public export
data StreamType : Type where
  Unidirectional : StreamType
  Bidirectional  : StreamType

||| Show instance for StreamType.
export
Show StreamType where
  show Unidirectional = "Unidirectional"
  show Bidirectional  = "Bidirectional"

-------------------------------------------------------------------------------
-- Error Codes
-------------------------------------------------------------------------------

||| DoQ error codes (RFC 9250 Section 8.4).
public export
data ErrorCode : Type where
  NoError        : ErrorCode
  InternalError  : ErrorCode
  ExcessiveLoad  : ErrorCode
  ProtocolError  : ErrorCode

||| Show instance for ErrorCode, including the hex error code.
export
Show ErrorCode where
  show NoError       = "0x0 NoError"
  show InternalError = "0x1 InternalError"
  show ExcessiveLoad = "0x4 ExcessiveLoad"
  show ProtocolError = "0x5 ProtocolError"

-------------------------------------------------------------------------------
-- Session States
-------------------------------------------------------------------------------

||| QUIC session states for DNS over QUIC connections.
public export
data SessionState : Type where
  Initial     : SessionState
  Handshaking : SessionState
  Ready       : SessionState
  Draining    : SessionState
  Closed      : SessionState

||| Show instance for SessionState.
export
Show SessionState where
  show Initial     = "Initial"
  show Handshaking = "Handshaking"
  show Ready       = "Ready"
  show Draining    = "Draining"
  show Closed      = "Closed"
