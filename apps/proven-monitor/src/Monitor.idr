-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-monitor monitoring server.
||| Re-exports core types from Monitor.Types and defines server constants.
module Monitor

import public Monitor.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default listening port for the monitoring HTTP API.
public export
monitorPort : Nat
monitorPort = 8080

||| Default interval between health checks, in seconds.
public export
defaultInterval : Nat
defaultInterval = 60

||| Default timeout for a single health check execution, in seconds.
public export
defaultTimeout : Nat
defaultTimeout = 10

||| Maximum number of concurrent checks the server will track.
public export
maxChecks : Nat
maxChecks = 10000
