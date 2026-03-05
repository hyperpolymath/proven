-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-snmp: An SNMP agent that cannot crash on malformed PDUs.
--
-- Architecture:
--   - SNMP.Types: Version, PDUType, ErrorStatus as closed sum types
--     with Show/Eq instances.
--
-- This module defines core SNMP constants (RFC 3411) and re-exports SNMP.Types.

module SNMP

import public SNMP.Types

%default total

-- ============================================================================
-- SNMP constants (RFC 3411)
-- ============================================================================

||| Standard SNMP agent port for queries (RFC 3411).
public export
snmpPort : Bits16
snmpPort = 161

||| Standard SNMP trap receiver port (RFC 3411).
public export
trapPort : Bits16
trapPort = 162
