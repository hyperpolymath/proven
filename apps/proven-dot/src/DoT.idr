-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level DoT module. Re-exports DoT.Types and defines protocol constants.
module DoT

import public DoT.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (RFC 7858)
-------------------------------------------------------------------------------

||| Default DNS over TLS port (RFC 7858 Section 3.1).
public export
dotPort : Nat
dotPort = 853

||| Recommended idle timeout in seconds before closing the TLS session.
public export
idleTimeout : Nat
idleTimeout = 30
