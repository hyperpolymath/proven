-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level DoH module. Re-exports DoH.Types and defines protocol constants.
module DoH

import public DoH.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (RFC 8484)
-------------------------------------------------------------------------------

||| Default DoH port (HTTPS).
public export
dohPort : Nat
dohPort = 443

||| Maximum DNS message payload size in bytes.
public export
maxPayload : Nat
maxPayload = 65535

||| Standard DoH query path (RFC 8484 Section 6).
public export
dohPath : String
dohPath = "/dns-query"
