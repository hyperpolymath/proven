-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-graphdb graph database server.
||| Re-exports core types and provides server constants.
module Graphdb

import public Graphdb.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default Bolt protocol port.
public export
graphdbPort : Nat
graphdbPort = 7687

||| Bolt protocol port (alias for graphdbPort).
public export
boltPort : Nat
boltPort = 7687

||| HTTP API port.
public export
httpPort : Nat
httpPort = 7474

||| Maximum number of properties per node.
public export
maxNodeProperties : Nat
maxNodeProperties = 65536

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-graphdb"
