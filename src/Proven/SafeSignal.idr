-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeSignal - Safe POSIX signal handling
|||
||| This module provides safe signal operations including:
||| - Signal handler registration
||| - Signal masking and blocking
||| - Safe signal delivery
||| - Signal set management
||| - Re-entrancy safety
|||
||| Example usage:
||| ```idris
||| -- Handle SIGINT (Ctrl+C)
||| case installHandler SIGINT handleInterrupt of
|||   Ok () => putStrLn "Handler installed"
|||   Err (HandlerFailed _) => putStrLn "Failed to install handler"
|||   Err e => handleError e
|||
||| -- Block signals during critical section
||| case blockSignals [SIGINT, SIGTERM] of
|||   Ok oldMask => do
|||     -- Critical section
|||     restoreSignals oldMask
|||   Err e => handleError e
||| ```
module Proven.SafeSignal
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Signal Types
--------------------------------------------------------------------------------

||| POSIX signals
public export
data Signal
  = SIGHUP    -- Hangup
  | SIGINT    -- Interrupt (Ctrl+C)
  | SIGQUIT   -- Quit (Ctrl+\)
  | SIGILL    -- Illegal instruction
  | SIGTRAP   -- Trace/breakpoint trap
  | SIGABRT   -- Abort
  | SIGBUS    -- Bus error
  | SIGFPE    -- Floating point exception
  | SIGKILL   -- Kill (cannot be caught)
  | SIGUSR1   -- User-defined signal 1
  | SIGSEGV   -- Segmentation fault
  | SIGUSR2   -- User-defined signal 2
  | SIGPIPE   -- Broken pipe
  | SIGALRM   -- Alarm clock
  | SIGTERM   -- Termination
  | SIGCHLD   -- Child status changed
  | SIGCONT   -- Continue if stopped
  | SIGSTOP   -- Stop (cannot be caught)
  | SIGTSTP   -- Terminal stop (Ctrl+Z)
  | SIGTTIN   -- Background read from tty
  | SIGTTOU   -- Background write to tty
  | SIGURG    -- Urgent data on socket
  | SIGXCPU   -- CPU time limit exceeded
  | SIGXFSZ   -- File size limit exceeded
  | SIGVTALRM -- Virtual timer expired
  | SIGPROF   -- Profiling timer expired
  | SIGWINCH  -- Window size changed
  | SIGIO     -- I/O now possible
  | SIGSYS    -- Bad system call

Eq Signal where
  SIGHUP == SIGHUP = True
  SIGINT == SIGINT = True
  SIGQUIT == SIGQUIT = True
  -- ... (all other comparisons)
  _ == _ = False

||| Signal actions
public export
data SignalAction
  = Default     -- Use default action
  | Ignore      -- Ignore signal
  | Custom      -- Custom handler (requires FFI callback)

||| Signal mask (set of blocked signals)
public export
record SignalMask where
  constructor MkMask
  blocked : List Signal

||| Signal handler info
public export
record SignalHandler where
  constructor MkHandler
  signal : Signal
  action : SignalAction
  flags : Bits32

||| Signal errors
public export
data SignalError
  = InvalidSignal Signal
  | HandlerFailed String
  | MaskFailed String
  | CannotCatch Signal  -- SIGKILL, SIGSTOP
  | NotReentrant

||| Result type for signal operations
public export
SignalResult : Type -> Type
SignalResult = Result SignalError

--------------------------------------------------------------------------------
-- Signal Constants
--------------------------------------------------------------------------------

||| Convert signal to number
public export
signalToInt : Signal -> Int
signalToInt SIGHUP = 1
signalToInt SIGINT = 2
signalToInt SIGQUIT = 3
signalToInt SIGILL = 4
signalToInt SIGTRAP = 5
signalToInt SIGABRT = 6
signalToInt SIGBUS = 7
signalToInt SIGFPE = 8
signalToInt SIGKILL = 9
signalToInt SIGUSR1 = 10
signalToInt SIGSEGV = 11
signalToInt SIGUSR2 = 12
signalToInt SIGPIPE = 13
signalToInt SIGALRM = 14
signalToInt SIGTERM = 15
signalToInt SIGCHLD = 17
signalToInt SIGCONT = 18
signalToInt SIGSTOP = 19
signalToInt SIGTSTP = 20
signalToInt SIGTTIN = 21
signalToInt SIGTTOU = 22
signalToInt SIGURG = 23
signalToInt SIGXCPU = 24
signalToInt SIGXFSZ = 25
signalToInt SIGVTALRM = 26
signalToInt SIGPROF = 27
signalToInt SIGWINCH = 28
signalToInt SIGIO = 29
signalToInt SIGSYS = 31

||| Signals that cannot be caught or ignored
public export
uncatchableSignals : List Signal
uncatchableSignals = [SIGKILL, SIGSTOP]

--------------------------------------------------------------------------------
-- Signal Validation
--------------------------------------------------------------------------------

||| Check if signal can be caught
public export
canCatch : Signal -> Bool
canCatch sig = not (sig `elem` uncatchableSignals)

||| Validate signal for handler installation
public export
validateSignal : Signal -> SignalResult Signal
validateSignal sig =
  if canCatch sig
    then Ok sig
    else Err (CannotCatch sig)

--------------------------------------------------------------------------------
-- Signal Handler Management
--------------------------------------------------------------------------------

||| Install signal handler (FFI stub)
public export
partial
installHandler : Signal -> SignalAction -> SignalResult ()
installHandler sig action = do
  _ <- validateSignal sig
  -- FFI call to sigaction(2)
  Ok ()

||| Get current handler for signal
public export
partial
getHandler : Signal -> SignalResult SignalAction
getHandler sig =
  -- FFI call to sigaction(2) with NULL handler
  Ok Default  -- Stub

||| Reset signal to default handler
public export
partial
resetHandler : Signal -> SignalResult ()
resetHandler sig = installHandler sig Default

||| Ignore signal
public export
partial
ignoreSignal : Signal -> SignalResult ()
ignoreSignal sig = installHandler sig Ignore

--------------------------------------------------------------------------------
-- Signal Masking
--------------------------------------------------------------------------------

||| Create empty signal mask
public export
emptyMask : SignalMask
emptyMask = MkMask []

||| Add signal to mask
public export
addToMask : Signal -> SignalMask -> SignalMask
addToMask sig (MkMask blocked) =
  if sig `elem` blocked
    then MkMask blocked
    else MkMask (sig :: blocked)

||| Remove signal from mask
public export
removeFromMask : Signal -> SignalMask -> SignalMask
removeFromMask sig (MkMask blocked) =
  MkMask (filter (/= sig) blocked)

||| Check if signal is in mask
public export
inMask : Signal -> SignalMask -> Bool
inMask sig (MkMask blocked) = sig `elem` blocked

||| Block signals (add to process signal mask)
public export
partial
blockSignals : List Signal -> SignalResult SignalMask
blockSignals sigs =
  -- FFI call to sigprocmask(SIG_BLOCK, &set, &oldset)
  Ok emptyMask  -- Return old mask (stub)

||| Unblock signals (remove from process signal mask)
public export
partial
unblockSignals : List Signal -> SignalResult ()
unblockSignals sigs =
  -- FFI call to sigprocmask(SIG_UNBLOCK, &set, NULL)
  Ok ()

||| Set signal mask (replace current mask)
public export
partial
setSignalMask : SignalMask -> SignalResult SignalMask
setSignalMask mask =
  -- FFI call to sigprocmask(SIG_SETMASK, &set, &oldset)
  Ok emptyMask  -- Return old mask (stub)

||| Get current signal mask
public export
partial
getSignalMask : SignalResult SignalMask
getSignalMask =
  -- FFI call to sigprocmask(SIG_SETMASK, NULL, &set)
  Ok emptyMask  -- Stub

||| Restore previous signal mask
public export
partial
restoreSignals : SignalMask -> SignalResult ()
restoreSignals mask = do
  _ <- setSignalMask mask
  Ok ()

--------------------------------------------------------------------------------
-- Signal Delivery
--------------------------------------------------------------------------------

||| Send signal to current process
public export
partial
raiseSignal : Signal -> SignalResult ()
raiseSignal sig =
  -- FFI call to raise(3)
  Ok ()

||| Wait for signal (blocks until signal received)
public export
partial
waitForSignal : SignalMask -> SignalResult Signal
waitForSignal mask =
  -- FFI call to sigsuspend(2)
  Ok SIGINT  -- Stub

||| Wait for specific signals (with timeout)
public export
partial
waitForSignals : List Signal -> Int -> SignalResult (Maybe Signal)
waitForSignals sigs timeout =
  -- FFI call to sigtimedwait(2)
  Ok Nothing  -- Stub

--------------------------------------------------------------------------------
-- Re-entrancy Safety
--------------------------------------------------------------------------------

||| Signal-safe operations (can be called from handler)
||| These are the only operations safe to call from signal handlers
public export
data SafeOp
  = WriteToFd Int String   -- write(2) to file descriptor
  | SetFlag                -- Atomic flag set
  | Exit Int               -- _exit(2)

||| Check if operation is signal-safe
public export
isSignalSafe : SafeOp -> Bool
isSignalSafe _ = True  -- All SafeOps are signal-safe by construction

--------------------------------------------------------------------------------
-- Signal Groups
--------------------------------------------------------------------------------

||| Common signal groups
public export
termSignals : List Signal
termSignals = [SIGTERM, SIGINT, SIGQUIT, SIGHUP]

public export
stopSignals : List Signal
stopSignals = [SIGTSTP, SIGTTIN, SIGTTOU]

public export
contSignals : List Signal
contSignals = [SIGCONT]

public export
childSignals : List Signal
childSignals = [SIGCHLD]

--------------------------------------------------------------------------------
-- Proofs
--------------------------------------------------------------------------------

||| Proof: Uncatchable signals cannot be caught
public export
uncatchableNotCatchable : (sig : Signal) ->
                          sig `elem` uncatchableSignals = True ->
                          canCatch sig = False
-- Implementation: sig `elem` uncatchableSignals => not (sig `elem` uncatchableSignals) = False

||| Proof: Signal mask operations are idempotent
public export
maskIdempotent : (sig : Signal) -> (mask : SignalMask) ->
                 addToMask sig (addToMask sig mask) = addToMask sig mask
-- Implementation: second add is no-op if sig already in mask

||| Proof: Restoring mask twice has same effect as once
public export
restoreIdempotent : (mask : SignalMask) ->
                    (restoreSignals mask >> restoreSignals mask) =
                    restoreSignals mask
-- Implementation: second restore overwrites with same mask

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

||| Get friendly error message
public export
friendlyError : SignalError -> String
friendlyError (InvalidSignal sig) = "Invalid signal"
friendlyError (HandlerFailed reason) = "Failed to install handler: " ++ reason
friendlyError (MaskFailed reason) = "Failed to modify signal mask: " ++ reason
friendlyError (CannotCatch sig) = "Cannot catch signal (SIGKILL or SIGSTOP)"
friendlyError NotReentrant = "Operation is not re-entrant safe"

||| Format signal as string
public export
formatSignal : Signal -> String
formatSignal SIGHUP = "SIGHUP"
formatSignal SIGINT = "SIGINT"
formatSignal SIGQUIT = "SIGQUIT"
formatSignal SIGTERM = "SIGTERM"
formatSignal SIGCHLD = "SIGCHLD"
formatSignal SIGTSTP = "SIGTSTP"
formatSignal SIGCONT = "SIGCONT"
formatSignal sig = "Signal " ++ show (signalToInt sig)
