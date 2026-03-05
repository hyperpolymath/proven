-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-triplestore RDF triple store server.
||| Re-exports core types and provides server constants.
module Triplestore

import public Triplestore.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default triple store HTTP port.
public export
triplestorePort : Nat
triplestorePort = 3030

||| Maximum number of named graphs.
public export
maxGraphs : Nat
maxGraphs = 10000

||| Batch import size (triples per transaction).
public export
batchImportSize : Nat
batchImportSize = 100000

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-triplestore"
