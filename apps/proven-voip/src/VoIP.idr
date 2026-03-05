-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level VoIP module. Re-exports VoIP.Types and defines protocol constants.
module VoIP

import public VoIP.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (RFC 3261)
-------------------------------------------------------------------------------

||| Default SIP plaintext port (RFC 3261 Section 19.1.1).
public export
sipPort : Nat
sipPort = 5060

||| Default SIP over TLS port (RFC 3261 Section 19.1.2).
public export
sipsPort : Nat
sipsPort = 5061
