-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-sandbox: Sandbox execution server.
--
-- Architecture:
--   - Types: ExecutionPolicy, ResourceLimit, SandboxState, ExitReason, SyscallPolicy
--
-- This module defines core sandbox constants and re-exports Sandbox.Types.

module Sandbox

import public Sandbox.Types

%default total

||| Default execution timeout in seconds.
public export
defaultTimeout : Nat
defaultTimeout = 30

||| Default maximum memory allocation in megabytes.
public export
maxMemoryMB : Nat
maxMemoryMB = 512

||| Default maximum CPU time in seconds.
public export
maxCPUSeconds : Nat
maxCPUSeconds = 60
