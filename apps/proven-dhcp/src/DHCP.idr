-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-dhcp: A DHCP server/client that cannot crash on malformed messages.
--
-- Architecture:
--   - DHCP.Types: MessageType, OptionCode, HardwareType as closed sum types
--     with Show/Eq instances.
--
-- This module defines core DHCP constants (RFC 2131) and re-exports DHCP.Types.

module DHCP

import public DHCP.Types

%default total

-- ============================================================================
-- DHCP constants (RFC 2131)
-- ============================================================================

||| DHCP server port (RFC 2131 Section 4.1): servers listen on UDP port 67.
public export
serverPort : Bits16
serverPort = 67

||| DHCP client port (RFC 2131 Section 4.1): clients listen on UDP port 68.
public export
clientPort : Bits16
clientPort = 68

||| Minimum DHCP message size in bytes (RFC 2131 Section 2).
||| Messages smaller than 576 bytes must be padded.
public export
maxMessageSize : Nat
maxMessageSize = 576
