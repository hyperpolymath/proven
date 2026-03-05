-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-logcollector structured log ingestion server.
||| Re-exports core types from Logcollector.Types and defines server constants.
module Logcollector

import public Logcollector.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default listening port for syslog/TCP/UDP log ingestion.
public export
logPort : Nat
logPort = 5140

||| Default listening port for OpenTelemetry gRPC log ingestion.
public export
grpcPort : Nat
grpcPort = 4317

||| Default listening port for OpenTelemetry HTTP log ingestion.
public export
httpPort : Nat
httpPort = 4318

||| Maximum number of log entries per batch before flushing to outputs.
public export
maxBatchSize : Nat
maxBatchSize = 1000

||| Default flush interval in seconds (flush even if batch is not full).
public export
flushInterval : Nat
flushInterval = 5
