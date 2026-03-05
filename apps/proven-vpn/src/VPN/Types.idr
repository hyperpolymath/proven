-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core WireGuard-style VPN protocol types as closed sum types.
-- | Models message types, handshake states, peer states,
-- | and error reasons inspired by the WireGuard protocol.
module VPN.Types

%default total

-------------------------------------------------------------------------------
-- Message Types
-------------------------------------------------------------------------------

||| VPN wire protocol message types.
||| Based on the WireGuard message type identifiers.
public export
data MessageType : Type where
  Handshake         : MessageType
  HandshakeResponse : MessageType
  CookieReply       : MessageType
  Transport         : MessageType

||| Show instance for MessageType, including the type identifier byte.
export
Show MessageType where
  show Handshake         = "1 Handshake"
  show HandshakeResponse = "2 HandshakeResponse"
  show CookieReply       = "3 CookieReply"
  show Transport         = "4 Transport"

-------------------------------------------------------------------------------
-- Handshake States
-------------------------------------------------------------------------------

||| Noise protocol handshake state machine states.
public export
data HandshakeState : Type where
  Empty        : HandshakeState
  InitSent     : HandshakeState
  InitReceived : HandshakeState
  Established  : HandshakeState

||| Show instance for HandshakeState.
export
Show HandshakeState where
  show Empty        = "Empty"
  show InitSent     = "InitSent"
  show InitReceived = "InitReceived"
  show Established  = "Established"

-------------------------------------------------------------------------------
-- Peer States
-------------------------------------------------------------------------------

||| Peer connection states.
public export
data PeerState : Type where
  Connected    : PeerState
  Disconnected : PeerState
  Expired      : PeerState

||| Show instance for PeerState.
export
Show PeerState where
  show Connected    = "Connected"
  show Disconnected = "Disconnected"
  show Expired      = "Expired"

-------------------------------------------------------------------------------
-- Error Reasons
-------------------------------------------------------------------------------

||| Error reasons specific to VPN protocol processing.
public export
data ErrorReason : Type where
  InvalidMAC       : ErrorReason
  DecryptionFailed : ErrorReason
  ReplayDetected   : ErrorReason
  HandshakeTimeout : ErrorReason

||| Show instance for ErrorReason.
export
Show ErrorReason where
  show InvalidMAC       = "InvalidMAC"
  show DecryptionFailed = "DecryptionFailed"
  show ReplayDetected   = "ReplayDetected"
  show HandshakeTimeout = "HandshakeTimeout"
