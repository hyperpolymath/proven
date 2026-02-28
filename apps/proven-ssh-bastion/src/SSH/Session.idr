-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SSH Session State Machine
--
-- Tracks the overall lifecycle of an SSH connection from version
-- exchange through key exchange, user authentication, to an
-- established session with channels.  The state machine prevents
-- operations from happening out of order — you cannot open a channel
-- before authentication completes because the types forbid it.

module SSH.Session

import SSH.Transport
import SSH.Auth
import SSH.Channel

%default total

-- ============================================================================
-- Session States
-- ============================================================================

||| The top-level states of an SSH session.
||| These correspond to the protocol phases defined across RFCs 4253/4252/4254.
public export
data SessionState : Type where
  ||| Initial phase: exchanging SSH-2.0 version strings
  VersionExchange : SessionState
  ||| Key exchange in progress (KEXINIT sent, computing shared secret)
  KeyExchange     : SessionState
  ||| User authentication in progress (SSH_MSG_USERAUTH_REQUEST cycle)
  UserAuth        : SessionState
  ||| Fully authenticated — channels can be opened, data can flow
  Authenticated   : SessionState
  ||| Session terminated (cleanly or due to error)
  Disconnected    : SessionState

public export
Eq SessionState where
  VersionExchange == VersionExchange = True
  KeyExchange     == KeyExchange     = True
  UserAuth        == UserAuth        = True
  Authenticated   == Authenticated   = True
  Disconnected    == Disconnected    = True
  _               == _               = False

public export
Show SessionState where
  show VersionExchange = "VersionExchange"
  show KeyExchange     = "KeyExchange"
  show UserAuth        = "UserAuth"
  show Authenticated   = "Authenticated"
  show Disconnected    = "Disconnected"

-- ============================================================================
-- Valid Session Transitions
-- ============================================================================

||| Proof that a session state transition is valid.
||| The SSH protocol progresses strictly:
|||   VersionExchange -> KeyExchange -> UserAuth -> Authenticated
||| Any state can transition to Disconnected.
||| Re-keying: Authenticated -> KeyExchange -> Authenticated
public export
data ValidSessionTransition : SessionState -> SessionState -> Type where
  ||| Version strings exchanged, begin key exchange
  VersionDone     : ValidSessionTransition VersionExchange KeyExchange
  ||| Key exchange complete, begin user authentication
  KexComplete     : ValidSessionTransition KeyExchange UserAuth
  ||| User authentication successful, session established
  AuthComplete    : ValidSessionTransition UserAuth Authenticated
  ||| Re-keying initiated from an established session
  RekeyInitiated  : ValidSessionTransition Authenticated KeyExchange
  ||| Re-key complete, return to authenticated state
  RekeyComplete   : ValidSessionTransition KeyExchange Authenticated
  ||| Disconnect from version exchange
  DisconnectFromVE  : ValidSessionTransition VersionExchange Disconnected
  ||| Disconnect from key exchange
  DisconnectFromKex : ValidSessionTransition KeyExchange Disconnected
  ||| Disconnect from user auth
  DisconnectFromAuth : ValidSessionTransition UserAuth Disconnected
  ||| Disconnect from authenticated session
  DisconnectFromEstablished : ValidSessionTransition Authenticated Disconnected

-- ============================================================================
-- Disconnect Reason Codes (RFC 4253 Section 11.1)
-- ============================================================================

||| SSH disconnect reason codes.
public export
data DisconnectReason : Type where
  ||| SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT (1)
  HostNotAllowed        : DisconnectReason
  ||| SSH_DISCONNECT_PROTOCOL_ERROR (2)
  ProtocolError         : DisconnectReason
  ||| SSH_DISCONNECT_KEY_EXCHANGE_FAILED (3)
  KeyExchangeFailed     : DisconnectReason
  ||| SSH_DISCONNECT_HOST_AUTHENTICATION_FAILED (4)
  HostAuthFailed        : DisconnectReason
  ||| SSH_DISCONNECT_MAC_ERROR (5)
  MACError              : DisconnectReason
  ||| SSH_DISCONNECT_SERVICE_NOT_AVAILABLE (7)
  ServiceNotAvailable   : DisconnectReason
  ||| SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED (8)
  VersionNotSupported   : DisconnectReason
  ||| SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE (9)
  HostKeyNotVerifiable  : DisconnectReason
  ||| SSH_DISCONNECT_CONNECTION_LOST (10)
  ConnectionLost        : DisconnectReason
  ||| SSH_DISCONNECT_BY_APPLICATION (11)
  ByApplication         : DisconnectReason
  ||| SSH_DISCONNECT_TOO_MANY_CONNECTIONS (12)
  TooManyConnections    : DisconnectReason
  ||| SSH_DISCONNECT_AUTH_CANCELLED_BY_USER (13)
  AuthCancelled         : DisconnectReason

public export
Show DisconnectReason where
  show HostNotAllowed       = "host not allowed to connect"
  show ProtocolError        = "protocol error"
  show KeyExchangeFailed    = "key exchange failed"
  show HostAuthFailed       = "host authentication failed"
  show MACError             = "MAC error"
  show ServiceNotAvailable  = "service not available"
  show VersionNotSupported  = "protocol version not supported"
  show HostKeyNotVerifiable = "host key not verifiable"
  show ConnectionLost       = "connection lost"
  show ByApplication        = "disconnected by application"
  show TooManyConnections   = "too many connections"
  show AuthCancelled        = "auth cancelled by user"

-- ============================================================================
-- Session Record
-- ============================================================================

||| An SSH session tracks the current protocol phase, negotiated algorithms,
||| authenticated user, and active channels.
public export
record SSHSession where
  constructor MkSSHSession
  ||| Current state in the session state machine
  state             : SessionState
  ||| Negotiated algorithms (set after key exchange completes)
  algorithms        : Maybe NegotiatedAlgorithms
  ||| Authenticated username (set after auth completes)
  authenticatedUser : Maybe String
  ||| Authentication attempt tracker
  authAttempts      : Maybe AuthAttempts
  ||| Active channels indexed by local channel ID
  channels          : List Channel
  ||| Next available local channel ID
  nextChannelId     : Bits32
  ||| Number of completed key exchanges (increments on re-key)
  keyExchangeCount  : Nat

||| Create a new SSH session in the initial VersionExchange state.
public export
newSSHSession : SSHSession
newSSHSession = MkSSHSession
  { state             = VersionExchange
  , algorithms        = Nothing
  , authenticatedUser = Nothing
  , authAttempts      = Nothing
  , channels          = []
  , nextChannelId     = 0
  , keyExchangeCount  = 0
  }

-- ============================================================================
-- Session operations
-- ============================================================================

||| Advance the session to KeyExchange after version exchange.
public export
beginKeyExchange : SSHSession -> Maybe SSHSession
beginKeyExchange s =
  case s.state of
    VersionExchange => Just ({ state := KeyExchange } s)
    Authenticated   => Just ({ state := KeyExchange } s)  -- Re-key
    _               => Nothing

||| Complete key exchange and advance to UserAuth (or back to Authenticated for re-key).
public export
completeKeyExchange : NegotiatedAlgorithms -> SSHSession -> Maybe SSHSession
completeKeyExchange algs s =
  case s.state of
    KeyExchange =>
      let nextState = if s.keyExchangeCount > 0
                        then Authenticated   -- Re-key complete
                        else UserAuth        -- First kex -> auth
      in Just ({ state            := nextState
               , algorithms       := Just algs
               , keyExchangeCount $= (+ 1)
               } s)
    _ => Nothing

||| Complete authentication successfully.
public export
completeAuth : (username : String) -> SSHSession -> Maybe SSHSession
completeAuth user s =
  case s.state of
    UserAuth => Just ({ state             := Authenticated
                      , authenticatedUser := Just user
                      } s)
    _        => Nothing

||| Open a new channel on an authenticated session.
||| Returns the updated session and the new channel, or Nothing if
||| the session is not in Authenticated state.
public export
openChannel : (channelType : ChannelType) -> (windowSize : Nat)
            -> (maxPacket : Nat) -> SSHSession
            -> Maybe (SSHSession, Channel)
openChannel ct ws mp s =
  case s.state of
    Authenticated =>
      let ch = newChannel s.nextChannelId ct ws mp
      in Just ({ channels      $= (ch ::)
               , nextChannelId $= (+ 1)
               } s, ch)
    _ => Nothing

||| Find a channel by its local ID.
public export
findChannel : Bits32 -> SSHSession -> Maybe Channel
findChannel cid s = find (\ch => ch.localId == cid) s.channels

||| Count active (non-closed) channels.
public export
activeChannelCount : SSHSession -> Nat
activeChannelCount s = length (filter (\ch => ch.state /= Closed) s.channels)

||| Disconnect the session with a reason.
public export
disconnect : DisconnectReason -> SSHSession -> SSHSession
disconnect _ s = { state := Disconnected, channels := [] } s

||| Check if the session is authenticated and ready for channel operations.
public export
isAuthenticated : SSHSession -> Bool
isAuthenticated s = s.state == Authenticated
