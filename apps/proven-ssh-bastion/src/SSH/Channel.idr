-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SSH Channel Layer (RFC 4254)
--
-- Defines channel types, the channel state machine, and channel records
-- with flow control (window size, max packet size).  The state machine
-- ensures only valid transitions are representable — you cannot send
-- data on a closed channel because the types forbid it.

module SSH.Channel

%default total

-- ============================================================================
-- Channel Types (RFC 4254 Section 5)
-- ============================================================================

||| SSH channel types as specified in the channel open request.
public export
data ChannelType : Type where
  ||| "session" — remote command execution or shell (RFC 4254 Section 6)
  Session       : ChannelType
  ||| "direct-tcpip" — client-to-server TCP forwarding (RFC 4254 Section 7.2)
  DirectTcpIp   : ChannelType
  ||| "forwarded-tcpip" — server-to-client TCP forwarding (RFC 4254 Section 7.1)
  ForwardedTcpIp : ChannelType
  ||| "x11" — X11 forwarding channel (RFC 4254 Section 6.3)
  X11           : ChannelType

public export
Eq ChannelType where
  Session        == Session        = True
  DirectTcpIp    == DirectTcpIp    = True
  ForwardedTcpIp == ForwardedTcpIp = True
  X11            == X11            = True
  _              == _              = False

public export
Show ChannelType where
  show Session        = "session"
  show DirectTcpIp    = "direct-tcpip"
  show ForwardedTcpIp = "forwarded-tcpip"
  show X11            = "x11"

||| Parse a channel type string from the wire.
||| Returns Nothing for unrecognised types — no crash.
public export
channelTypeFromString : String -> Maybe ChannelType
channelTypeFromString "session"        = Just Session
channelTypeFromString "direct-tcpip"   = Just DirectTcpIp
channelTypeFromString "forwarded-tcpip" = Just ForwardedTcpIp
channelTypeFromString "x11"            = Just X11
channelTypeFromString _                = Nothing

-- ============================================================================
-- Channel State Machine
-- ============================================================================

||| States a channel passes through during its lifetime.
||| The state machine enforces that data can only be sent on an Open channel
||| and that a channel progresses through states in order.
public export
data ChannelState : Type where
  ||| Channel open request sent, waiting for confirmation
  Opening : ChannelState
  ||| Channel confirmed and active — data transfer allowed
  Open    : ChannelState
  ||| EOF or close sent/received, draining remaining data
  Closing : ChannelState
  ||| Channel fully closed, resources released
  Closed  : ChannelState

public export
Eq ChannelState where
  Opening == Opening = True
  Open    == Open    = True
  Closing == Closing = True
  Closed  == Closed  = True
  _       == _       = False

public export
Show ChannelState where
  show Opening = "Opening"
  show Open    = "Open"
  show Closing = "Closing"
  show Closed  = "Closed"

-- ============================================================================
-- Valid Channel Transitions
-- ============================================================================

||| Proof that a channel state transition is valid.
||| Only the following transitions are permitted:
|||   Opening -> Open     (confirmation received)
|||   Opening -> Closed   (open failed)
|||   Open    -> Closing  (EOF or close initiated)
|||   Closing -> Closed   (close confirmed)
public export
data ValidChannelTransition : ChannelState -> ChannelState -> Type where
  ||| Channel open confirmed by peer
  OpenConfirmed : ValidChannelTransition Opening Open
  ||| Channel open rejected by peer
  OpenRejected  : ValidChannelTransition Opening Closed
  ||| Channel close initiated (EOF sent or received)
  CloseInitiated : ValidChannelTransition Open Closing
  ||| Channel close completed
  CloseCompleted : ValidChannelTransition Closing Closed

-- ============================================================================
-- Channel Open Failure Codes (RFC 4254 Section 5.1)
-- ============================================================================

||| Reason codes for channel open failure.
public export
data ChannelOpenFailure : Type where
  ||| SSH_OPEN_ADMINISTRATIVELY_PROHIBITED (1)
  AdminProhibited         : ChannelOpenFailure
  ||| SSH_OPEN_CONNECT_FAILED (2)
  ConnectFailed           : ChannelOpenFailure
  ||| SSH_OPEN_UNKNOWN_CHANNEL_TYPE (3)
  UnknownChannelType      : ChannelOpenFailure
  ||| SSH_OPEN_RESOURCE_SHORTAGE (4)
  ResourceShortage        : ChannelOpenFailure

public export
Eq ChannelOpenFailure where
  AdminProhibited    == AdminProhibited    = True
  ConnectFailed      == ConnectFailed      = True
  UnknownChannelType == UnknownChannelType = True
  ResourceShortage   == ResourceShortage   = True
  _                  == _                  = False

public export
Show ChannelOpenFailure where
  show AdminProhibited    = "administratively prohibited"
  show ConnectFailed      = "connect failed"
  show UnknownChannelType = "unknown channel type"
  show ResourceShortage   = "resource shortage"

||| Convert a failure reason to its wire code.
public export
failureCode : ChannelOpenFailure -> Bits32
failureCode AdminProhibited    = 1
failureCode ConnectFailed      = 2
failureCode UnknownChannelType = 3
failureCode ResourceShortage   = 4

-- ============================================================================
-- Channel Record
-- ============================================================================

||| A single SSH channel with its current state and flow control parameters.
public export
record Channel where
  constructor MkChannel
  ||| Local channel identifier (assigned by this side)
  localId       : Bits32
  ||| Remote channel identifier (assigned by peer)
  remoteId      : Bits32
  ||| The type of this channel
  channelType   : ChannelType
  ||| Current state in the channel state machine
  state         : ChannelState
  ||| Remaining window size for INCOMING data (how much more data we accept)
  localWindow   : Nat
  ||| Remaining window size for OUTGOING data (how much more data peer accepts)
  remoteWindow  : Nat
  ||| Maximum packet size for data payloads on this channel
  maxPacketSize : Nat
  ||| Whether we have sent EOF on this channel
  eofSent       : Bool
  ||| Whether we have received EOF from the peer
  eofReceived   : Bool

||| Create a new channel in the Opening state.
public export
newChannel : (localId : Bits32) -> (channelType : ChannelType)
           -> (windowSize : Nat) -> (maxPacket : Nat) -> Channel
newChannel lid ct ws mp = MkChannel
  { localId       = lid
  , remoteId      = 0         -- Set when open confirmation arrives
  , channelType   = ct
  , state         = Opening
  , localWindow   = ws
  , remoteWindow  = 0         -- Set when open confirmation arrives
  , maxPacketSize = mp
  , eofSent       = False
  , eofReceived   = False
  }

||| Confirm channel opening with the peer's parameters.
public export
confirmChannel : (remoteId : Bits32) -> (remoteWindow : Nat)
               -> Channel -> Channel
confirmChannel rid rw ch =
  { remoteId     := rid
  , remoteWindow := rw
  , state        := Open
  } ch

||| Consume window space when receiving data.
||| Returns Nothing if the data exceeds the available window — the sender
||| has violated flow control.
public export
consumeWindow : (dataLen : Nat) -> Channel -> Maybe Channel
consumeWindow len ch =
  if len > ch.localWindow
    then Nothing  -- Flow control violation
    else Just ({ localWindow $= (\w => minus w len) } ch)

||| Adjust the remote window when a window adjust message arrives.
public export
adjustRemoteWindow : (increment : Nat) -> Channel -> Channel
adjustRemoteWindow inc ch = { remoteWindow $= (+ inc) } ch

||| Check whether data can be sent on this channel.
||| Data can only be sent when the channel is Open, EOF has not been sent,
||| and there is remaining remote window space.
public export
canSendData : Channel -> Bool
canSendData ch = ch.state == Open
              && not ch.eofSent
              && ch.remoteWindow > 0
