-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-configmgmt configuration management server.
||| Re-exports core types from Configmgmt.Types and defines server constants.
module Configmgmt

import public Configmgmt.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default listening port for the configuration management API.
public export
configPort : Nat
configPort = 8140

||| Default interval between configuration sync runs, in seconds.
public export
syncInterval : Nat
syncInterval = 1800

||| Maximum number of managed resources the server will track.
public export
maxResources : Nat
maxResources = 100000
