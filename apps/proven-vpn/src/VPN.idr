-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level VPN module. Re-exports VPN.Types and defines protocol constants.
module VPN

import public VPN.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (WireGuard-style)
-------------------------------------------------------------------------------

||| Default VPN listening port (WireGuard default).
public export
vpnPort : Nat
vpnPort = 51820

||| Persistent keepalive interval in seconds.
public export
keepaliveInterval : Nat
keepaliveInterval = 25

||| Handshake timeout in seconds before retry.
public export
handshakeTimeout : Nat
handshakeTimeout = 5
