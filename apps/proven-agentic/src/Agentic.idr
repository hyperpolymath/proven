-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Agentic: Top-level module for the multi-agent coordination server.
-- Re-exports all protocol types from Agentic.Types and defines
-- server configuration constants.

module Agentic

import public Agentic.Types

%default total

------------------------------------------------------------------------
-- Server configuration constants
------------------------------------------------------------------------

||| The TCP port the agentic coordination server listens on.
public export
agenticPort : Nat
agenticPort = 9600

||| Maximum number of concurrent agents the server can manage.
public export
maxAgents : Nat
maxAgents = 256

||| Maximum depth of nested plan steps before the planner refuses
||| to recurse further.
public export
maxPlanDepth : Nat
maxPlanDepth = 50

||| Timeout in seconds for safety checks. If a safety evaluation
||| does not complete within this window, it returns Timeout.
public export
safetyTimeout : Nat
safetyTimeout = 10

||| Maximum number of tool calls a single agent may issue within
||| one task execution before being forcibly stopped.
public export
maxToolCalls : Nat
maxToolCalls = 1000
