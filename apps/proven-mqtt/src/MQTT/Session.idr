-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- MQTT Session State Machine (MQTT 3.1.1 Section 3.1, 4.1)
--
-- The MQTT connection lifecycle is modelled as a finite state machine
-- with four states. Dependent types ensure only valid transitions can
-- be constructed — connecting from Disconnecting or publishing from
-- Idle are compile-time errors. The clean session flag controls
-- whether session state persists across connections.

module MQTT.Session

import MQTT.QoS
import MQTT.Topic

%default total

-- ============================================================================
-- Session states
-- ============================================================================

||| The four states of an MQTT client connection lifecycle.
public export
data SessionState : Type where
  ||| No connection established. Initial state and terminal state.
  Idle          : SessionState
  ||| TCP connection established, CONNECT packet sent, awaiting CONNACK.
  Connecting    : SessionState
  ||| CONNACK received with return code 0. Session is fully operational.
  Connected     : SessionState
  ||| DISCONNECT packet sent or connection lost. Cleaning up resources.
  Disconnecting : SessionState

public export
Eq SessionState where
  Idle          == Idle          = True
  Connecting    == Connecting    = True
  Connected     == Connected     = True
  Disconnecting == Disconnecting = True
  _             == _             = False

public export
Show SessionState where
  show Idle          = "Idle"
  show Connecting    = "Connecting"
  show Connected     = "Connected"
  show Disconnecting = "Disconnecting"

-- ============================================================================
-- Session events
-- ============================================================================

||| Events that drive session state transitions.
public export
data SessionEvent : Type where
  ||| Client initiates a TCP connection and sends CONNECT.
  InitiateConnect   : SessionEvent
  ||| CONNACK received with return code 0 (connection accepted).
  ConnAckAccepted   : SessionEvent
  ||| CONNACK received with non-zero return code (connection refused).
  ConnAckRefused    : SessionEvent
  ||| Client sends DISCONNECT (clean shutdown).
  ClientDisconnect  : SessionEvent
  ||| TCP connection lost unexpectedly.
  ConnectionLost    : SessionEvent
  ||| Cleanup complete, resources released.
  CleanupComplete   : SessionEvent
  ||| Keep-alive timeout expired without receiving any packet.
  KeepAliveTimeout  : SessionEvent

public export
Show SessionEvent where
  show InitiateConnect  = "InitiateConnect"
  show ConnAckAccepted  = "ConnAckAccepted"
  show ConnAckRefused   = "ConnAckRefused"
  show ClientDisconnect = "ClientDisconnect"
  show ConnectionLost   = "ConnectionLost"
  show CleanupComplete  = "CleanupComplete"
  show KeepAliveTimeout = "KeepAliveTimeout"

-- ============================================================================
-- CONNACK return codes (MQTT 3.1.1 Section 3.2.2.3)
-- ============================================================================

||| CONNACK return codes indicating the result of a connection attempt.
public export
data ConnAckCode : Type where
  ||| Connection accepted.
  ConnectionAccepted        : ConnAckCode
  ||| Unacceptable protocol version.
  UnacceptableProtocol      : ConnAckCode
  ||| Client identifier rejected.
  IdentifierRejected        : ConnAckCode
  ||| Server unavailable.
  ServerUnavailable         : ConnAckCode
  ||| Bad username or password.
  BadCredentials            : ConnAckCode
  ||| Client not authorised.
  NotAuthorised             : ConnAckCode

public export
Eq ConnAckCode where
  ConnectionAccepted   == ConnectionAccepted   = True
  UnacceptableProtocol == UnacceptableProtocol = True
  IdentifierRejected   == IdentifierRejected   = True
  ServerUnavailable    == ServerUnavailable    = True
  BadCredentials       == BadCredentials       = True
  NotAuthorised        == NotAuthorised        = True
  _                    == _                    = False

public export
Show ConnAckCode where
  show ConnectionAccepted   = "Connection Accepted"
  show UnacceptableProtocol = "Unacceptable Protocol Version"
  show IdentifierRejected   = "Identifier Rejected"
  show ServerUnavailable    = "Server Unavailable"
  show BadCredentials       = "Bad Username or Password"
  show NotAuthorised        = "Not Authorised"

||| Convert a return code byte to a ConnAckCode.
public export
connAckFromByte : Bits8 -> Maybe ConnAckCode
connAckFromByte 0 = Just ConnectionAccepted
connAckFromByte 1 = Just UnacceptableProtocol
connAckFromByte 2 = Just IdentifierRejected
connAckFromByte 3 = Just ServerUnavailable
connAckFromByte 4 = Just BadCredentials
connAckFromByte 5 = Just NotAuthorised
connAckFromByte _ = Nothing

||| Convert a ConnAckCode to its byte value.
public export
connAckToByte : ConnAckCode -> Bits8
connAckToByte ConnectionAccepted   = 0
connAckToByte UnacceptableProtocol = 1
connAckToByte IdentifierRejected   = 2
connAckToByte ServerUnavailable    = 3
connAckToByte BadCredentials       = 4
connAckToByte NotAuthorised        = 5

-- ============================================================================
-- Session transition function
-- ============================================================================

||| Result of a session state transition.
public export
record SessionTransition where
  constructor MkSessionTransition
  ||| The new state after the transition.
  newState : SessionState
  ||| Whether the transition was valid and applied.
  valid    : Bool

||| Session state transition function (total over all state/event combinations).
||| Invalid combinations produce the same state with valid=False.
public export
sessionTransition : SessionState -> SessionEvent -> SessionTransition
-- From Idle: only InitiateConnect is valid
sessionTransition Idle InitiateConnect  = MkSessionTransition Connecting True
sessionTransition Idle _                = MkSessionTransition Idle False
-- From Connecting: CONNACK responses or connection failure
sessionTransition Connecting ConnAckAccepted  = MkSessionTransition Connected True
sessionTransition Connecting ConnAckRefused   = MkSessionTransition Disconnecting True
sessionTransition Connecting ConnectionLost   = MkSessionTransition Disconnecting True
sessionTransition Connecting KeepAliveTimeout = MkSessionTransition Disconnecting True
sessionTransition Connecting _                = MkSessionTransition Connecting False
-- From Connected: disconnect events
sessionTransition Connected ClientDisconnect = MkSessionTransition Disconnecting True
sessionTransition Connected ConnectionLost   = MkSessionTransition Disconnecting True
sessionTransition Connected KeepAliveTimeout = MkSessionTransition Disconnecting True
sessionTransition Connected _                = MkSessionTransition Connected False
-- From Disconnecting: only cleanup can return to Idle
sessionTransition Disconnecting CleanupComplete = MkSessionTransition Idle True
sessionTransition Disconnecting _               = MkSessionTransition Disconnecting False

-- ============================================================================
-- Session record
-- ============================================================================

||| A subscription entry: a topic filter paired with a granted QoS level.
public export
record Subscription where
  constructor MkSubscription
  filter     : TopicFilter
  grantedQoS : QoS

||| Complete session state for an MQTT client.
public export
record MQTTSession where
  constructor MkMQTTSession
  ||| Current state in the connection lifecycle.
  state         : SessionState
  ||| Whether this is a clean session (no persistent state).
  cleanSession  : Bool
  ||| Client identifier string.
  clientId      : String
  ||| Keep-alive interval in seconds (0 = disabled).
  keepAlive     : Bits16
  ||| Active subscriptions for this session.
  subscriptions : List Subscription
  ||| Number of in-flight QoS 1/2 messages awaiting acknowledgement.
  pendingAcks   : Nat

||| Create a new session in Idle state.
public export
newSession : (clientId : String) -> (cleanSession : Bool) -> (keepAlive : Bits16) -> MQTTSession
newSession cid clean ka = MkMQTTSession
  { state         = Idle
  , cleanSession  = clean
  , clientId      = cid
  , keepAlive     = ka
  , subscriptions = []
  , pendingAcks   = 0
  }

||| Apply an event to a session, returning the updated session.
||| If the transition is invalid, the session is returned unchanged.
public export
applyEvent : MQTTSession -> SessionEvent -> (MQTTSession, Bool)
applyEvent session event =
  let result = sessionTransition session.state event
      newSess = if result.valid
                  then let s = { state := result.newState } session
                       in if result.newState == Idle && session.cleanSession
                            then { subscriptions := [], pendingAcks := 0 } s
                            else s
                  else session
  in (newSess, result.valid)

||| Check whether the session is in the Connected state (ready for publish/subscribe).
public export
isConnected : MQTTSession -> Bool
isConnected session = session.state == Connected
