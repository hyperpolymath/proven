-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-socks skeleton.
-- | Re-exports SOCKS.Types and defines protocol constants for
-- | RFC 1928 SOCKS5 proxy.

module SOCKS

import public SOCKS.Types

%default total

||| Default SOCKS5 TCP port per RFC 1928.
public export
socksPort : Nat
socksPort = 1080
