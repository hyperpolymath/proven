-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebSocket Session State Machine (RFC 6455 Section 7)
--
-- Tracks the lifecycle of a WebSocket connection from the opening
-- handshake through data exchange to graceful close.  The state
-- machine enforces that data frames cannot be sent after a Close
-- frame, and that Ping/Pong tracking is correct.

module WS.Session

import WS.Opcode
import WS.Frame
import WS.CloseCode

%default total

-- ============================================================================
-- Session States (RFC 6455 Sections 4, 7)
-- ============================================================================

||| The lifecycle states of a WebSocket session.
public export
data WSState : Type where
  ||| HTTP upgrade handshake in progress
  Connecting : WSState
  ||| Connection established — data frames can be exchanged
  WSOpen     : WSState
  ||| Close frame sent or received, waiting for the peer's Close response
  WSClosing  : WSState
  ||| Connection fully closed
  WSClosed   : WSState

public export
Eq WSState where
  Connecting == Connecting = True
  WSOpen     == WSOpen     = True
  WSClosing  == WSClosing  = True
  WSClosed   == WSClosed   = True
  _          == _          = False

public export
Show WSState where
  show Connecting = "Connecting"
  show WSOpen     = "Open"
  show WSClosing  = "Closing"
  show WSClosed   = "Closed"

-- ============================================================================
-- Valid Transitions
-- ============================================================================

||| Proof that a WebSocket session state transition is valid.
public export
data ValidWSTransition : WSState -> WSState -> Type where
  ||| Handshake completed successfully
  HandshakeComplete : ValidWSTransition Connecting WSOpen
  ||| Handshake failed
  HandshakeFailed   : ValidWSTransition Connecting WSClosed
  ||| Close frame sent or received
  CloseInitiated    : ValidWSTransition WSOpen WSClosing
  ||| Close handshake completed
  CloseCompleted    : ValidWSTransition WSClosing WSClosed
  ||| Abnormal closure from Open state
  AbnormalClose     : ValidWSTransition WSOpen WSClosed

-- ============================================================================
-- Ping/Pong Tracking
-- ============================================================================

||| Tracks outstanding Ping frames awaiting Pong responses.
||| Used to detect unresponsive peers.
public export
record PingTracker where
  constructor MkPingTracker
  ||| Number of Pings sent without a matching Pong
  outstanding  : Nat
  ||| Maximum allowed outstanding Pings before declaring peer dead
  maxOutstanding : Nat
  ||| Total Pings sent during this session
  totalSent    : Nat
  ||| Total Pongs received during this session
  totalReceived : Nat

||| Create a fresh ping tracker.
public export
newPingTracker : (maxOutstanding : Nat) -> PingTracker
newPingTracker maxO = MkPingTracker
  { outstanding    = 0
  , maxOutstanding = maxO
  , totalSent      = 0
  , totalReceived  = 0
  }

||| Record that a Ping was sent.
public export
recordPingSent : PingTracker -> PingTracker
recordPingSent pt =
  { outstanding $= (+ 1)
  , totalSent   $= (+ 1)
  } pt

||| Record that a Pong was received.
public export
recordPongReceived : PingTracker -> PingTracker
recordPongReceived pt =
  { outstanding   $= (\n => if n > 0 then minus n 1 else 0)
  , totalReceived $= (+ 1)
  } pt

||| Check if the peer is considered dead (too many unanswered Pings).
public export
isPeerDead : PingTracker -> Bool
isPeerDead pt = pt.outstanding >= pt.maxOutstanding

-- ============================================================================
-- Session Record
-- ============================================================================

||| A WebSocket session tracks the connection state, message fragmentation,
||| ping/pong health, and close code.
public export
record WSSession where
  constructor MkWSSession
  ||| Current state in the session state machine
  state            : WSState
  ||| Whether we initiated the close (True = we sent Close first)
  closeInitiator   : Bool
  ||| Close code received from peer (if any)
  peerCloseCode    : Maybe CloseCode
  ||| Close code we sent (if any)
  ourCloseCode     : Maybe CloseCode
  ||| Ping/Pong health tracker
  pingTracker      : PingTracker
  ||| Whether a fragmented message is in progress
  fragmentInProgress : Bool
  ||| Opcode of the fragmented message in progress (Text or Binary)
  fragmentOpcode   : Maybe Opcode
  ||| Total frames received in this session
  framesReceived   : Nat
  ||| Total frames sent in this session
  framesSent       : Nat

||| Create a new WebSocket session in the Connecting state.
public export
newWSSession : WSSession
newWSSession = MkWSSession
  { state              = Connecting
  , closeInitiator     = False
  , peerCloseCode      = Nothing
  , ourCloseCode       = Nothing
  , pingTracker        = newPingTracker 3
  , fragmentInProgress = False
  , fragmentOpcode     = Nothing
  , framesReceived     = 0
  , framesSent         = 0
  }

-- ============================================================================
-- Session Operations
-- ============================================================================

||| Complete the handshake, moving from Connecting to Open.
public export
completeHandshake : WSSession -> Maybe WSSession
completeHandshake s =
  case s.state of
    Connecting => Just ({ state := WSOpen } s)
    _          => Nothing

||| Fail the handshake, moving from Connecting to Closed.
public export
failHandshake : WSSession -> Maybe WSSession
failHandshake s =
  case s.state of
    Connecting => Just ({ state := WSClosed } s)
    _          => Nothing

||| Initiate a close from our side.
public export
initiateClose : CloseCode -> WSSession -> Maybe WSSession
initiateClose code s =
  case s.state of
    WSOpen => Just ({ state          := WSClosing
                    , closeInitiator := True
                    , ourCloseCode   := Just code
                    } s)
    _      => Nothing

||| Process a Close frame received from the peer.
public export
receiveClose : CloseCode -> WSSession -> WSSession
receiveClose code s =
  case s.state of
    WSOpen =>
      -- Peer initiated close — we need to respond
      { state        := WSClosing
      , peerCloseCode := Just code
      } s
    WSClosing =>
      -- We initiated close, peer responded — close complete
      { state        := WSClosed
      , peerCloseCode := Just code
      } s
    other =>
      -- In any other state, just record the code
      { peerCloseCode := Just code } s

||| Complete the close (both sides have sent Close frames).
public export
completeClose : WSSession -> WSSession
completeClose s = { state := WSClosed } s

||| Record that a frame was received.
public export
recordFrameReceived : WSSession -> WSSession
recordFrameReceived = { framesReceived $= (+ 1) }

||| Record that a frame was sent.
public export
recordFrameSent : WSSession -> WSSession
recordFrameSent = { framesSent $= (+ 1) }

||| Check if the session is open and can exchange data frames.
public export
canSendData : WSSession -> Bool
canSendData s = s.state == WSOpen

||| Check if the session is fully closed.
public export
isClosed : WSSession -> Bool
isClosed s = s.state == WSClosed
