-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-radius: A RADIUS server with formally verified packet handling.
--
-- Architecture:
--   - RADIUS.Types: PacketType, AttributeType, ServiceType as closed sum
--     types with Show/Eq instances.
--
-- This module defines core RADIUS constants (RFC 2865) and re-exports
-- RADIUS.Types.

module RADIUS

import public RADIUS.Types

%default total

-- ============================================================================
-- RADIUS constants (RFC 2865)
-- ============================================================================

||| RADIUS authentication port (RFC 2865 Section 3).
public export
authPort : Bits16
authPort = 1812

||| RADIUS accounting port (RFC 2866 Section 3).
public export
acctPort : Bits16
acctPort = 1813

||| Maximum RADIUS packet size in bytes (RFC 2865 Section 3).
||| The minimum is 20 bytes (header only); the maximum is 4096 bytes.
public export
maxPacketSize : Nat
maxPacketSize = 4096
