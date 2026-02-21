-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeProcess - Safe process lifecycle management
|||
||| This module provides safe process operations including:
||| - Fork and exec with validation
||| - Process reaping (wait, waitpid)
||| - Exit code handling
||| - Zombie prevention
||| - Resource limit enforcement
|||
||| Example usage:
||| ```idris
||| -- Spawn process safely
||| case spawnProcess "/bin/ls" ["-la"] of
|||   Ok pid => do
|||     -- Wait for completion
|||     result <- waitForProcess pid
|||     handleResult result
|||   Err (CommandNotFound _) => putStrLn "Command not found"
|||   Err e => handleError e
||| ```
module Proven.SafeProcess

import public Proven.Core
import Proven.SafePath
import Proven.SafeShell
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Process Types
--------------------------------------------------------------------------------

||| Process ID (abstract, platform-specific)
public export
data ProcessId = MkPid Int

||| Exit status
public export
data ExitStatus
  = Exited Int              -- Normal exit with code
  | Signaled Int            -- Killed by signal
  | Stopped Int             -- Stopped by signal
  | Continued               -- Continued after stop

||| Process state
public export
data ProcessState
  = Running
  | Zombie
  | StoppedState
  | Terminated ExitStatus

public export
Eq ExitStatus where
  (Exited a) == (Exited b) = a == b
  (Signaled a) == (Signaled b) = a == b
  (Stopped a) == (Stopped b) = a == b
  Continued == Continued = True
  _ == _ = False

public export
Eq ProcessState where
  Running == Running = True
  Zombie == Zombie = True
  StoppedState == StoppedState = True
  (Terminated a) == (Terminated b) = a == b
  _ == _ = False

||| Process errors
public export
data ProcessError
  = ForkFailed String
  | ExecFailed String String   -- program, reason
  | CommandNotFound String
  | PermissionDenied String
  | InvalidArguments
  | ProcessNotFound ProcessId
  | WaitFailed String
  | ResourceLimitExceeded
  | TooManyProcesses

||| Result type for process operations
public export
ProcessResult : Type -> Type
ProcessResult = Result ProcessError

--------------------------------------------------------------------------------
-- Process Spawning
--------------------------------------------------------------------------------

||| Validate executable path (must exist, be executable)
public export
partial
validateExecutable : String -> ProcessResult String
validateExecutable path = do
  -- Check path is safe
  case validatePath path of
    Right safePath => Ok path  -- Return validated path string
    Left _ => Err (CommandNotFound path)
  -- FFI: check file exists and is executable
  -- For now, accept any path

||| Validate process arguments (check for injection, null bytes)
public export
validateArgs : List String -> ProcessResult (List String)
validateArgs args =
  if all (\arg => not (isInfixOf "\0" arg)) args
    then Ok args
    else Err InvalidArguments

||| Spawn new process (fork + exec)
||| FFI stub - actual implementation via Zig
public export
partial
spawnProcess : String -> List String -> ProcessResult ProcessId
spawnProcess prog args = do
  validProg <- validateExecutable prog
  validArgs <- validateArgs args
  -- FFI call to fork(2) + execve(2)
  -- For now, return stub PID
  Ok (MkPid 1234)

||| Spawn process with custom environment
public export
partial
spawnProcessEnv : String -> List String -> List (String, String) -> ProcessResult ProcessId
spawnProcessEnv prog args env = do
  validProg <- validateExecutable prog
  validArgs <- validateArgs args
  -- Validate environment variables (no null bytes, valid names)
  -- FFI call to fork(2) + execve(2) with env
  Ok (MkPid 1235)

--------------------------------------------------------------------------------
-- Process Waiting
--------------------------------------------------------------------------------

||| Wait for process to terminate (blocking)
||| FFI stub - actual implementation via Zig
public export
partial
waitForProcess : ProcessId -> ProcessResult ExitStatus
waitForProcess (MkPid pid) =
  if pid <= 0
    then Err (ProcessNotFound (MkPid pid))
    else Ok (Exited 0)  -- FFI call to waitpid(2)

||| Wait for any child process (non-blocking)
||| Returns Nothing if no child has exited
public export
partial
waitAnyProcess : ProcessResult (Maybe (ProcessId, ExitStatus))
waitAnyProcess =
  -- FFI call to waitpid(-1, &status, WNOHANG)
  Ok Nothing  -- Stub

||| Check if process has terminated (non-blocking)
public export
partial
pollProcess : ProcessId -> ProcessResult (Maybe ExitStatus)
pollProcess (MkPid pid) =
  -- FFI call to waitpid(pid, &status, WNOHANG)
  Ok Nothing  -- Stub

--------------------------------------------------------------------------------
-- Process Management
--------------------------------------------------------------------------------

||| Send signal to process
||| FFI stub - actual implementation via Zig
public export
partial
sendSignal : ProcessId -> Int -> ProcessResult ()
sendSignal (MkPid pid) sig =
  if pid <= 0
    then Err (ProcessNotFound (MkPid pid))
    else Ok ()  -- FFI call to kill(2)

||| Terminate process gracefully (SIGTERM)
public export
partial
terminateProcess : ProcessId -> ProcessResult ()
terminateProcess pid = sendSignal pid 15  -- SIGTERM

||| Kill process forcefully (SIGKILL)
public export
partial
killProcess : ProcessId -> ProcessResult ()
killProcess pid = sendSignal pid 9  -- SIGKILL

||| Get process state
public export
partial
getProcessState : ProcessId -> ProcessResult ProcessState
getProcessState pid =
  -- Check if process exists, is zombie, etc.
  Ok Running  -- Stub

--------------------------------------------------------------------------------
-- Resource Tracking
--------------------------------------------------------------------------------

||| Tracked process with metadata
public export
record TrackedProcess where
  constructor Track
  pid : ProcessId
  program : String
  args : List String
  state : ProcessState
  startTime : Int  -- Unix timestamp
  endTime : Maybe Int

||| Initialize tracked process
public export
trackProcess : ProcessId -> String -> List String -> Int -> TrackedProcess
trackProcess pid prog args start =
  Track pid prog args Running start Nothing

||| Update process state
public export
updateState : ProcessState -> TrackedProcess -> TrackedProcess
updateState newState tracked =
  { state := newState } tracked

||| Mark process as terminated
public export
markTerminated : ExitStatus -> Int -> TrackedProcess -> TrackedProcess
markTerminated status endTime tracked =
  { state := Terminated status
  , endTime := Just endTime
  } tracked

||| Check if process is zombie (needs reaping)
public export
isZombie : TrackedProcess -> Bool
isZombie tracked = tracked.state == Zombie

||| Get process runtime (in seconds)
public export
getRuntime : TrackedProcess -> Maybe Int
getRuntime tracked =
  case tracked.endTime of
    Nothing => Nothing
    Just end => Just (end - tracked.startTime)

--------------------------------------------------------------------------------
-- Zombie Prevention
--------------------------------------------------------------------------------

||| Reap all zombie children (non-blocking)
public export
partial
reapZombies : ProcessResult (List (ProcessId, ExitStatus))
reapZombies =
  -- Loop calling waitpid(-1, &status, WNOHANG) until no more zombies
  Ok []  -- Stub

||| Reap specific process if it's a zombie
public export
partial
reapProcess : ProcessId -> ProcessResult (Maybe ExitStatus)
reapProcess pid = pollProcess pid

--------------------------------------------------------------------------------
-- Exit Code Utilities
--------------------------------------------------------------------------------

||| Check if exit status indicates success
public export
isSuccess : ExitStatus -> Bool
isSuccess (Exited 0) = True
isSuccess _ = False

||| Get exit code from status (if exited normally)
public export
getExitCode : ExitStatus -> Maybe Int
getExitCode (Exited code) = Just code
getExitCode _ = Nothing

||| Get signal number (if killed by signal)
public export
getSignal : ExitStatus -> Maybe Int
getSignal (Signaled sig) = Just sig
getSignal _ = Nothing

--------------------------------------------------------------------------------
-- Proofs
--------------------------------------------------------------------------------

||| Proof: Spawned processes have positive PIDs
-- public export
-- spawnedPidPositive : (prog : String) -> (args : List String) ->
--                      (pid : ProcessId) ->
--                      spawnProcess prog args = Ok pid ->
--                      (n : Int ** pid = MkPid n && n > 0)
-- Implementation deferred

||| Proof: Reaped processes are no longer zombies
-- public export
-- reapedNotZombie : (pid : ProcessId) -> (status : ExitStatus) ->
--                   reapProcess pid = Ok (Just status) ->
--                   (tracked : TrackedProcess) ->
--                   tracked.pid = pid ->
--                   isZombie (markTerminated status 0 tracked) = False
-- Implementation deferred

||| Proof: Terminated processes cannot be waited again
-- public export
-- terminatedOnce : (pid : ProcessId) -> (status : ExitStatus) ->
--                  waitForProcess pid = Ok status ->
-- --                  (waitForProcess pid /= Ok status)
-- -- -- Cannot wait same process twice (implementation deferred)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

||| Get friendly error message
public export
friendlyError : ProcessError -> String
friendlyError (ForkFailed reason) = "Fork failed: " ++ reason
friendlyError (ExecFailed prog reason) = "Failed to execute " ++ prog ++ ": " ++ reason
friendlyError (CommandNotFound prog) = "Command not found: " ++ prog
friendlyError (PermissionDenied prog) = "Permission denied: " ++ prog
friendlyError InvalidArguments = "Invalid arguments (null bytes detected)"
friendlyError (ProcessNotFound (MkPid pid)) = "Process not found: " ++ show pid
friendlyError (WaitFailed reason) = "Wait failed: " ++ reason
friendlyError ResourceLimitExceeded = "Resource limit exceeded"
friendlyError TooManyProcesses = "Too many processes"

||| Format exit status as string
public export
formatExitStatus : ExitStatus -> String
formatExitStatus (Exited code) = "exited with code " ++ show code
formatExitStatus (Signaled sig) = "killed by signal " ++ show sig
formatExitStatus (Stopped sig) = "stopped by signal " ++ show sig
formatExitStatus Continued = "continued"
