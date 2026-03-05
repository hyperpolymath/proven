-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-smb: An SMB2/3 client that cannot crash on malformed responses.
--
-- Architecture:
--   - SMB.Types: Command, Dialect, ShareType as closed sum types
--     with Show/Eq instances.
--
-- This module defines core SMB constants and re-exports SMB.Types.

module SMB

import public SMB.Types

%default total

-- ============================================================================
-- SMB constants
-- ============================================================================

||| Standard SMB port (Microsoft-DS, TCP 445).
||| SMB2/3 exclusively uses this port; the legacy NetBIOS port 139 is not
||| supported by this implementation.
public export
smbPort : Bits16
smbPort = 445
