-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-nfs: An NFSv4 client/server that cannot crash on malformed RPCs.
--
-- Architecture:
--   - NFS.Types: Operation, FileType, Status as closed sum types
--     with Show/Eq instances.
--
-- This module defines core NFS constants (RFC 7530) and re-exports NFS.Types.

module NFS

import public NFS.Types

%default total

-- ============================================================================
-- NFS constants (RFC 7530)
-- ============================================================================

||| Standard NFSv4 port (RFC 7530 Section 3.1).
||| NFSv4 uses a single well-known port instead of the portmapper used by
||| earlier versions.
public export
nfsPort : Bits16
nfsPort = 2049
