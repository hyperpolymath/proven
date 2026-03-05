-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level DoQ module. Re-exports DoQ.Types and defines protocol constants.
module DoQ

import public DoQ.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (RFC 9250)
-------------------------------------------------------------------------------

||| Default DNS over QUIC port (RFC 9250 Section 5).
||| Shares port 853 with DNS over TLS; distinguished by ALPN.
public export
doqPort : Nat
doqPort = 853

||| Recommended maximum concurrent streams per connection.
public export
maxStreams : Nat
maxStreams = 100
