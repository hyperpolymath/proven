-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-metrics telemetry server.
||| Re-exports core types from Metrics.Types and defines server constants.
module Metrics

import public Metrics.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default listening port for the metrics query and scrape API.
public export
metricsPort : Nat
metricsPort = 9090

||| Default interval between target scrapes, in seconds.
public export
scrapeInterval : Nat
scrapeInterval = 15

||| Default interval between alerting rule evaluations, in seconds.
public export
evaluationInterval : Nat
evaluationInterval = 15

||| Maximum number of active time series the server will retain.
public export
maxSeries : Nat
maxSeries = 1000000
