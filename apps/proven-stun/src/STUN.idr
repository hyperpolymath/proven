-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level STUN module. Re-exports STUN.Types and defines protocol constants.
module STUN

import public STUN.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (RFC 8489)
-------------------------------------------------------------------------------

||| Default STUN/TURN plaintext port (RFC 8489 Section 9).
public export
stunPort : Nat
stunPort = 3478

||| Default STUN/TURN over TLS port (RFC 8489 Section 9).
public export
stunsPort : Nat
stunsPort = 5349
