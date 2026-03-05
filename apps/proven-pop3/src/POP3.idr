-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-pop3: A POP3 server that cannot crash on malformed commands.
--
-- Architecture:
--   - POP3.Types: Command, State, Response as closed sum types
--     with Show/Eq instances.
--
-- This module defines core POP3 constants (RFC 1939) and re-exports POP3.Types.

module POP3

import public POP3.Types

%default total

-- ============================================================================
-- POP3 constants (RFC 1939)
-- ============================================================================

||| Standard POP3 port (RFC 1939).
public export
pop3Port : Bits16
pop3Port = 110

||| POP3 over TLS/SSL port (POP3S, RFC 8314).
public export
pop3sPort : Bits16
pop3sPort = 995
