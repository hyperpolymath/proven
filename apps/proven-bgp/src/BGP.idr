-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-bgp: A BGP-4 (RFC 4271) implementation that cannot crash.
--
-- Architecture:
--   - FSM: 6 states, 28 events (proven at compile time via dependent types)
--   - Message parsing: Bounds-checked via SafeBuffer (no buffer overflows)
--   - Network I/O: SafeNetwork (validated IP addresses)
--   - Timers: SafeMonotonic (no timer overflow)
--   - Route selection: SafeMath (no integer overflow in path comparison)
--
-- This module defines the core BGP types and re-exports submodules.

module BGP

import public BGP.FSM
import public BGP.Message
import public BGP.Route
import public BGP.Peer
import public BGP.Config

||| BGP version (RFC 4271)
public export
bgpVersion : Bits8
bgpVersion = 4

||| BGP default port (RFC 4271 Section 4.1)
public export
bgpPort : Bits16
bgpPort = 179

||| Maximum BGP message size (RFC 4271 Section 10)
||| 4096 octets including header
public export
maxMessageSize : Nat
maxMessageSize = 4096

||| Minimum BGP message size (RFC 4271 Section 10)
||| 19 octets (header only, for KEEPALIVE)
public export
minMessageSize : Nat
minMessageSize = 19

||| BGP marker field: 16 bytes of 0xFF (RFC 4271 Section 4.1)
public export
bgpMarker : Vect 16 Bits8
bgpMarker = replicate 16 0xFF
