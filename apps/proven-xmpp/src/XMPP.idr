-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-xmpp: An XMPP client/server that cannot crash on malformed stanzas.
--
-- Architecture:
--   - XMPP.Types: StanzaType, MessageType, PresenceType, IQType, StreamError
--     as closed sum types with Show/Eq instances.
--
-- This module defines core XMPP constants (RFC 6120) and re-exports XMPP.Types.

module XMPP

import public XMPP.Types

%default total

-- ============================================================================
-- XMPP constants (RFC 6120)
-- ============================================================================

||| Standard XMPP client-to-server port (RFC 6120 Section 3).
public export
xmppPort : Bits16
xmppPort = 5222

||| Legacy XMPP over TLS/SSL port (pre-STARTTLS, commonly used).
public export
xmppsPort : Bits16
xmppsPort = 5223

||| Standard XMPP server-to-server port (RFC 6120 Section 3).
public export
xmppServerPort : Bits16
xmppServerPort = 5269
