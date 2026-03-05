-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level MDNS module. Re-exports MDNS.Types and defines protocol constants.
module MDNS

import public MDNS.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (RFC 6762)
-------------------------------------------------------------------------------

||| mDNS port (RFC 6762 Section 2).
public export
mdnsPort : Nat
mdnsPort = 5353

||| IPv4 multicast address for mDNS (RFC 6762 Section 2).
public export
mdnsAddr : String
mdnsAddr = "224.0.0.251"

||| IPv6 multicast address for mDNS (RFC 6762 Section 2).
public export
mdnsAddr6 : String
mdnsAddr6 = "ff02::fb"

||| Maximum recommended TTL for mDNS records in seconds (75 minutes).
public export
maxRecordTTL : Nat
maxRecordTTL = 4500
