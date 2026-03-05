-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- XMPP.Types: Core protocol types for XMPP (RFC 6120).
--
-- Defines closed sum types for the three XML stanza types (message, presence,
-- IQ), message subtypes, presence show values, IQ types, and stream-level
-- error conditions from RFC 6120 Section 4.9.3.

module XMPP.Types

%default total

-- ============================================================================
-- XMPP stanza types (RFC 6120 Section 8)
-- ============================================================================

||| The three fundamental XMPP stanza types from RFC 6120 Section 8.
||| All XMPP communication consists of these three XML element types
||| exchanged over a bidirectional XML stream.
public export
data StanzaType : Type where
  ||| Push-style content delivery between entities (Section 8.2.1).
  Message  : StanzaType
  ||| Availability and status broadcasting (Section 8.2.2).
  Presence : StanzaType
  ||| Info/Query: request-response semantics (Section 8.2.3).
  IQ       : StanzaType

public export
Eq StanzaType where
  Message  == Message  = True
  Presence == Presence = True
  IQ       == IQ       = True
  _        == _        = False

public export
Show StanzaType where
  show Message  = "message"
  show Presence = "presence"
  show IQ       = "iq"

-- ============================================================================
-- Message types (RFC 6121 Section 5.2.2)
-- ============================================================================

||| Values for the 'type' attribute on <message> stanzas.
||| Determines how the message should be handled and displayed by clients.
public export
data MessageType : Type where
  ||| One-to-one chat session.
  Chat      : MessageType
  ||| Error response to a previous message.
  Error     : MessageType
  ||| Multi-user chat (MUC) message.
  Groupchat : MessageType
  ||| Automated alert or notification (no reply expected).
  Headline  : MessageType
  ||| Standalone message outside a chat session (default type).
  Normal    : MessageType

public export
Eq MessageType where
  Chat      == Chat      = True
  Error     == Error     = True
  Groupchat == Groupchat = True
  Headline  == Headline  = True
  Normal    == Normal    = True
  _         == _         = False

public export
Show MessageType where
  show Chat      = "chat"
  show Error     = "error"
  show Groupchat = "groupchat"
  show Headline  = "headline"
  show Normal    = "normal"

-- ============================================================================
-- Presence types (RFC 6121 Section 4.7.2)
-- ============================================================================

||| Presence show values and unavailable state.
||| The <show> child element indicates the specific availability sub-state,
||| while 'unavailable' is signalled via the type attribute.
public export
data PresenceType : Type where
  ||| Entity is online and available (no <show> element needed).
  Available   : PresenceType
  ||| Entity is temporarily away.
  Away        : PresenceType
  ||| Entity does not wish to be disturbed.
  DND         : PresenceType
  ||| Entity is away for an extended period.
  XA          : PresenceType
  ||| Entity is going offline.
  Unavailable : PresenceType

public export
Eq PresenceType where
  Available   == Available   = True
  Away        == Away        = True
  DND         == DND         = True
  XA          == XA          = True
  Unavailable == Unavailable = True
  _           == _           = False

public export
Show PresenceType where
  show Available   = "available"
  show Away        = "away"
  show DND         = "dnd"
  show XA          = "xa"
  show Unavailable = "unavailable"

-- ============================================================================
-- IQ types (RFC 6120 Section 8.2.3)
-- ============================================================================

||| Values for the 'type' attribute on <iq> stanzas.
||| IQ stanzas follow a strict request-response pattern.
public export
data IQType : Type where
  ||| Request information (request).
  Get    : IQType
  ||| Provide data or set configuration (request).
  Set    : IQType
  ||| Successful response to a Get or Set.
  Result : IQType
  ||| Error response to a Get or Set.
  IQError : IQType

public export
Eq IQType where
  Get     == Get     = True
  Set     == Set     = True
  Result  == Result  = True
  IQError == IQError = True
  _       == _       = False

public export
Show IQType where
  show Get     = "get"
  show Set     = "set"
  show Result  = "result"
  show IQError = "error"

-- ============================================================================
-- Stream errors (RFC 6120 Section 4.9.3)
-- ============================================================================

||| Stream-level error conditions from RFC 6120 Section 4.9.3.
||| These are fatal errors that cause the XML stream to be closed.
public export
data StreamError : Type where
  ||| XML is not well-formed.
  BadFormat          : StreamError
  ||| Resource conflict (another session bound to same resource).
  Conflict           : StreamError
  ||| Connection has been idle too long.
  ConnectionTimeout  : StreamError
  ||| Peer's hostname is no longer serviced.
  HostGone           : StreamError
  ||| Peer's hostname is not recognised.
  HostUnknown        : StreamError
  ||| Entity is not authorised to open a stream.
  NotAuthorized      : StreamError
  ||| Entity has violated a local service policy.
  PolicyViolation    : StreamError
  ||| Server lacks resources to service the stream.
  ResourceConstraint : StreamError
  ||| Server is shutting down.
  SystemShutdown     : StreamError

public export
Eq StreamError where
  BadFormat          == BadFormat          = True
  Conflict           == Conflict           = True
  ConnectionTimeout  == ConnectionTimeout  = True
  HostGone           == HostGone           = True
  HostUnknown        == HostUnknown        = True
  NotAuthorized      == NotAuthorized      = True
  PolicyViolation    == PolicyViolation    = True
  ResourceConstraint == ResourceConstraint = True
  SystemShutdown     == SystemShutdown     = True
  _                  == _                  = False

public export
Show StreamError where
  show BadFormat          = "bad-format"
  show Conflict           = "conflict"
  show ConnectionTimeout  = "connection-timeout"
  show HostGone           = "host-gone"
  show HostUnknown        = "host-unknown"
  show NotAuthorized      = "not-authorized"
  show PolicyViolation    = "policy-violation"
  show ResourceConstraint = "resource-constraint"
  show SystemShutdown     = "system-shutdown"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All stanza types.
public export
allStanzaTypes : List StanzaType
allStanzaTypes = [Message, Presence, IQ]

||| All message types.
public export
allMessageTypes : List MessageType
allMessageTypes = [Chat, Error, Groupchat, Headline, Normal]

||| All presence types.
public export
allPresenceTypes : List PresenceType
allPresenceTypes = [Available, Away, DND, XA, Unavailable]

||| All IQ types.
public export
allIQTypes : List IQType
allIQTypes = [Get, Set, Result, IQError]

||| All stream errors.
public export
allStreamErrors : List StreamError
allStreamErrors = [BadFormat, Conflict, ConnectionTimeout, HostGone,
                   HostUnknown, NotAuthorized, PolicyViolation,
                   ResourceConstraint, SystemShutdown]
