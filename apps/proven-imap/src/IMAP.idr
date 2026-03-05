-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-imap: An IMAP4rev1 server that cannot crash on malformed commands.
--
-- Architecture:
--   - IMAP.Types: Command, State, Flag as closed sum types
--     with Show/Eq instances.
--
-- This module defines core IMAP constants (RFC 3501) and re-exports IMAP.Types.

module IMAP

import public IMAP.Types

%default total

-- ============================================================================
-- IMAP constants (RFC 3501)
-- ============================================================================

||| Standard IMAP4rev1 port (RFC 3501).
public export
imapPort : Bits16
imapPort = 143

||| IMAP over TLS/SSL port (IMAPS, RFC 8314).
public export
imapsPort : Bits16
imapsPort = 993
