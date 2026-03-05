-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-semweb semantic web server.
||| Re-exports core types and provides server constants.
module Semweb

import public Semweb.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default HTTP port for the semantic web server.
public export
semwebPort : Nat
semwebPort = 8080

||| Maximum number of triples in the store (10 million).
public export
maxTriples : Nat
maxTriples = 10000000

||| Default page size for paginated results.
public export
defaultPageSize : Nat
defaultPageSize = 100

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-semweb"
