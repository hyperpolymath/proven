-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-sparql endpoint.
||| Re-exports core types and provides server constants.
module Sparql

import public Sparql.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default SPARQL endpoint port.
public export
sparqlPort : Nat
sparqlPort = 3030

||| Default query timeout in seconds.
public export
defaultTimeout : Nat
defaultTimeout = 30

||| Maximum result set size (number of bindings).
public export
maxResultSize : Nat
maxResultSize = 1000000

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-sparql"
