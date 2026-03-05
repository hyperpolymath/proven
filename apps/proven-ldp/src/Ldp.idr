-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-ldp Linked Data Platform server.
||| Re-exports core types and provides server constants.
module Ldp

import public Ldp.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default LDP HTTP port.
public export
ldpPort : Nat
ldpPort = 8080

||| Maximum resource size in bytes (10 MiB).
public export
maxResourceSize : Nat
maxResourceSize = 10485760

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-ldp"
