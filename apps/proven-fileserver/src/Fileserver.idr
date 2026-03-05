-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-fileserver: Network file server.
--
-- Architecture:
--   - Types: Operation, FileType, Permission, LockType, ErrorCode
--
-- This module defines core file server constants and re-exports Fileserver.Types.

module Fileserver

import public Fileserver.Types

%default total

||| Default port for the file server protocol.
public export
fileserverPort : Nat
fileserverPort = 9090

||| Maximum path length in bytes.
public export
maxPathLength : Nat
maxPathLength = 4096

||| Maximum file size in bytes (1 TiB).
public export
maxFileSize : Nat
maxFileSize = 1099511627776
