-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-sandbox: Core protocol types for sandbox execution server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Sandbox.Types

%default total

-- ============================================================================
-- ExecutionPolicy
-- ============================================================================

||| Security policy governing what a sandboxed process may do.
public export
data ExecutionPolicy : Type where
  ||| No restrictions (for trusted code only -- use with extreme caution).
  Unrestricted : ExecutionPolicy
  ||| File system is read-only; writes are discarded.
  ReadOnly     : ExecutionPolicy
  ||| All network access is denied.
  NetworkDenied : ExecutionPolicy
  ||| Fully isolated: no network, no shared filesystem, no IPC.
  Isolated     : ExecutionPolicy
  ||| Ephemeral: entire environment is destroyed after execution.
  Ephemeral    : ExecutionPolicy

export
Show ExecutionPolicy where
  show Unrestricted  = "Unrestricted"
  show ReadOnly      = "ReadOnly"
  show NetworkDenied = "NetworkDenied"
  show Isolated      = "Isolated"
  show Ephemeral     = "Ephemeral"

-- ============================================================================
-- ResourceLimit
-- ============================================================================

||| Categories of resource limits enforceable on a sandbox.
public export
data ResourceLimit : Type where
  ||| Maximum CPU time in seconds.
  CPUTime         : ResourceLimit
  ||| Maximum memory in bytes.
  Memory          : ResourceLimit
  ||| Maximum disk I/O bandwidth.
  DiskIO          : ResourceLimit
  ||| Maximum network I/O bandwidth.
  NetworkIO       : ResourceLimit
  ||| Maximum number of open file descriptors.
  FileDescriptors : ResourceLimit
  ||| Maximum number of spawned processes.
  Processes       : ResourceLimit

export
Show ResourceLimit where
  show CPUTime         = "CPUTime"
  show Memory          = "Memory"
  show DiskIO          = "DiskIO"
  show NetworkIO       = "NetworkIO"
  show FileDescriptors = "FileDescriptors"
  show Processes       = "Processes"

-- ============================================================================
-- SandboxState
-- ============================================================================

||| Lifecycle state of a sandbox instance.
public export
data SandboxState : Type where
  ||| Sandbox environment is being provisioned.
  Creating   : SandboxState
  ||| Sandbox is provisioned and ready to accept workloads.
  Ready      : SandboxState
  ||| A workload is actively executing inside the sandbox.
  Running    : SandboxState
  ||| Execution is paused (e.g. for checkpointing).
  Suspended  : SandboxState
  ||| Workload has finished; sandbox awaits cleanup.
  Terminated : SandboxState
  ||| Sandbox has been fully destroyed and resources reclaimed.
  Destroyed  : SandboxState

export
Show SandboxState where
  show Creating   = "Creating"
  show Ready      = "Ready"
  show Running    = "Running"
  show Suspended  = "Suspended"
  show Terminated = "Terminated"
  show Destroyed  = "Destroyed"

-- ============================================================================
-- ExitReason
-- ============================================================================

||| Reason a sandboxed workload terminated.
public export
data ExitReason : Type where
  ||| Workload exited normally with exit code 0.
  Normal         : ExitReason
  ||| Workload exceeded its time limit.
  Timeout        : ExitReason
  ||| Workload exceeded its memory limit.
  MemoryExceeded : ExitReason
  ||| Workload violated its execution policy.
  PolicyViolation : ExitReason
  ||| Workload was explicitly killed by an administrator.
  Killed         : ExitReason
  ||| Workload terminated due to an internal error.
  Error          : ExitReason

export
Show ExitReason where
  show Normal          = "Normal"
  show Timeout         = "Timeout"
  show MemoryExceeded  = "MemoryExceeded"
  show PolicyViolation = "PolicyViolation"
  show Killed          = "Killed"
  show Error           = "Error"

-- ============================================================================
-- SyscallPolicy
-- ============================================================================

||| Action to take when a sandboxed process invokes a system call.
public export
data SyscallPolicy : Type where
  ||| Allow the system call to proceed.
  Allow : SyscallPolicy
  ||| Deny the system call and return an error to the process.
  Deny  : SyscallPolicy
  ||| Allow the system call but log it for audit.
  Log   : SyscallPolicy
  ||| Trap the system call for inspection before deciding.
  Trap  : SyscallPolicy

export
Show SyscallPolicy where
  show Allow = "Allow"
  show Deny  = "Deny"
  show Log   = "Log"
  show Trap  = "Trap"
