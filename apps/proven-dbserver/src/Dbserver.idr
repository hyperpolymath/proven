-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-dbserver: Database server.
--
-- Architecture:
--   - Types: QueryType, DataType, IsolationLevel, ErrorCode, JoinType
--
-- This module defines core database server constants and re-exports Dbserver.Types.

module Dbserver

import public Dbserver.Types

%default total

||| Default database server port (PostgreSQL convention).
public export
dbPort : Nat
dbPort = 5432

||| Maximum query text length in bytes (1 MB).
public export
maxQueryLength : Nat
maxQueryLength = 1048576

||| Maximum number of concurrent client connections.
public export
maxConnections : Nat
maxConnections = 100
