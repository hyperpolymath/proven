-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-appserver: Application server.
--
-- Architecture:
--   - Types: RequestType, LifecycleState, HealthCheck, DeployStrategy, ErrorCategory
--
-- This module defines core appserver constants and re-exports Appserver.Types.

module Appserver

import public Appserver.Types

%default total

||| Default HTTP port for the application server.
public export
appPort : Nat
appPort = 8080

||| Grace period (in seconds) for draining in-flight requests during shutdown.
public export
shutdownGrace : Nat
shutdownGrace = 30

||| Maximum request body size in bytes (10 MB).
public export
maxRequestSize : Nat
maxRequestSize = 10485760
