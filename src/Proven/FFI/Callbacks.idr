-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI Callback Infrastructure for Bidirectional Communication
|||
||| This module provides the Idris2 ABI definitions for the callback system.
||| It enables host languages to register functions that Idris2 code can
||| invoke when events occur (validation failures, resource lifecycle,
||| circuit breaker state changes, etc.).
|||
||| Architecture:
|||   Host language → Zig callback_register → stores fn pointer
|||   Idris2 event  → Zig callback_fire     → invokes fn pointer → Host
|||
||| The callback system is thread-safe (mutex-protected on the Zig side).
module Proven.FFI.Callbacks

import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Event Type Identifiers
--------------------------------------------------------------------------------

||| Event types that can trigger callbacks.
||| Must match ProvenEventType enum in ffi/zig/src/main.zig exactly.
public export
data EventType
  = ValidationFailed    -- ^ Validation failure (SafeJson, SafeXML, SafeEmail, etc.)
  | ResourceAcquired    -- ^ Resource acquired (SafeResource, SafeFile)
  | ResourceReleased    -- ^ Resource released (SafeResource, SafeFile)
  | RateLimitHit        -- ^ Rate limit hit (SafeRateLimiter)
  | CircuitStateChange  -- ^ Circuit breaker state change (SafeCircuitBreaker)
  | SignalReceived      -- ^ Signal received (SafeSignal)
  | RetryAttempt        -- ^ Retry attempt (SafeRetry)
  | CryptoOperation     -- ^ Cryptographic operation completed (SafeCrypto)
  | CustomEvent         -- ^ Custom event (user-defined)

||| Convert EventType to its C ABI integer representation.
public export
eventTypeToInt : EventType -> Int
eventTypeToInt ValidationFailed   = 1
eventTypeToInt ResourceAcquired   = 2
eventTypeToInt ResourceReleased   = 3
eventTypeToInt RateLimitHit       = 4
eventTypeToInt CircuitStateChange = 5
eventTypeToInt SignalReceived     = 6
eventTypeToInt RetryAttempt       = 7
eventTypeToInt CryptoOperation    = 8
eventTypeToInt CustomEvent        = 100

||| Parse an integer back to EventType.
public export
intToEventType : Int -> Maybe EventType
intToEventType 1   = Just ValidationFailed
intToEventType 2   = Just ResourceAcquired
intToEventType 3   = Just ResourceReleased
intToEventType 4   = Just RateLimitHit
intToEventType 5   = Just CircuitStateChange
intToEventType 6   = Just SignalReceived
intToEventType 7   = Just RetryAttempt
intToEventType 8   = Just CryptoOperation
intToEventType 100 = Just CustomEvent
intToEventType _   = Nothing

--------------------------------------------------------------------------------
-- Callback Handle
--------------------------------------------------------------------------------

||| Opaque handle to a registered callback.
||| 0 is invalid (registration failed).
public export
CallbackHandle : Type
CallbackHandle = Int

||| Check if a callback handle is valid (non-zero).
public export
isValidHandle : CallbackHandle -> Bool
isValidHandle h = h /= 0

--------------------------------------------------------------------------------
-- Callback Errors
--------------------------------------------------------------------------------

||| Errors that can occur during callback operations.
public export
data CallbackError
  = RegistrationFailed String    -- ^ Could not register (registry full, null fn)
  | DeregistrationFailed String  -- ^ Handle not found
  | InvocationFailed String      -- ^ Event fire failed
  | InvalidHandle                -- ^ Handle is 0

public export
Show CallbackError where
  show (RegistrationFailed msg) = "Callback registration failed: " ++ msg
  show (DeregistrationFailed msg) = "Callback deregistration failed: " ++ msg
  show (InvocationFailed msg) = "Callback invocation failed: " ++ msg
  show InvalidHandle = "Invalid callback handle (0)"

--------------------------------------------------------------------------------
-- FFI Declarations (bind to Zig callback infrastructure)
--------------------------------------------------------------------------------

||| Register a callback for an event type.
||| Returns a handle (0 on failure).
||| The callback function pointer and context are opaque from Idris2's perspective.
export
%foreign "C:proven_callback_register,proven"
prim__callbackRegister : Int -> AnyPtr -> AnyPtr -> PrimIO Int

||| Unregister a callback by handle.
||| Returns 0 on success.
export
%foreign "C:proven_callback_unregister,proven"
prim__callbackUnregister : Int -> PrimIO Int

||| Fire an event, invoking all registered callbacks for that event type.
||| Returns the number of callbacks invoked.
export
%foreign "C:proven_callback_fire,proven"
prim__callbackFire : Int -> String -> Int -> Int -> PrimIO Int

||| Query how many callbacks are registered for an event type.
export
%foreign "C:proven_callback_count,proven"
prim__callbackCount : Int -> PrimIO Int

||| Unregister all callbacks. Returns count removed.
export
%foreign "C:proven_callback_clear_all,proven"
prim__callbackClearAll : PrimIO Int

--------------------------------------------------------------------------------
-- Safe Wrappers (IO operations with error handling)
--------------------------------------------------------------------------------

||| Fire an event from Idris2 code, invoking all registered host callbacks.
||| This is the primary mechanism for Idris2 → Host communication.
|||
||| @ eventType  The type of event being fired
||| @ message    Optional human-readable description
||| @ code       Event-specific integer code (e.g., error code, signal number)
||| @ Returns    Number of callbacks invoked, or an error
export
fireEvent : EventType -> String -> Int -> IO (Either CallbackError Nat)
fireEvent eventType message code = do
  result <- primIO $ prim__callbackFire (eventTypeToInt eventType) message (cast (length message)) code
  if result < 0
    then pure (Left (InvocationFailed ("fire returned " ++ show result)))
    else pure (Right (integerToNat (cast result)))

||| Query the number of registered callbacks for an event type.
export
callbackCount : EventType -> IO Nat
callbackCount eventType = do
  result <- primIO $ prim__callbackCount (eventTypeToInt eventType)
  pure (integerToNat (cast result))

||| Unregister all callbacks. Returns count of callbacks removed.
export
clearAllCallbacks : IO Nat
clearAllCallbacks = do
  result <- primIO $ prim__callbackClearAll
  pure (integerToNat (cast result))

--------------------------------------------------------------------------------
-- Convenience: Fire Events from Specific Modules
--------------------------------------------------------------------------------

||| Report a validation failure event.
||| @ moduleName  Which module detected the failure (e.g., "SafeJson")
||| @ details     Human-readable failure description
export
fireValidationFailed : (moduleName : String) -> (details : String) -> IO ()
fireValidationFailed moduleName details = do
  _ <- fireEvent ValidationFailed (moduleName ++ ": " ++ details) 0
  pure ()

||| Report a resource lifecycle event.
||| @ acquired  True if acquired, False if released
||| @ name      Resource identifier
export
fireResourceEvent : (acquired : Bool) -> (name : String) -> IO ()
fireResourceEvent acquired name = do
  let eventType = if acquired then ResourceAcquired else ResourceReleased
  _ <- fireEvent eventType name 0
  pure ()

||| Report a rate limit hit.
||| @ key       Rate limiter key/identifier
||| @ limit     The limit that was exceeded
export
fireRateLimitHit : (key : String) -> (limit : Nat) -> IO ()
fireRateLimitHit key limit = do
  _ <- fireEvent RateLimitHit key (cast limit)
  pure ()

||| Report a circuit breaker state change.
||| @ name       Circuit breaker name
||| @ newState   New state as integer (0=Closed, 1=Open, 2=HalfOpen)
export
fireCircuitStateChange : (name : String) -> (newState : Int) -> IO ()
fireCircuitStateChange name newState = do
  _ <- fireEvent CircuitStateChange name newState
  pure ()

||| Report a retry attempt.
||| @ operation  Operation being retried
||| @ attempt    Attempt number (1-based)
export
fireRetryAttempt : (operation : String) -> (attempt : Nat) -> IO ()
fireRetryAttempt operation attempt = do
  _ <- fireEvent RetryAttempt operation (cast attempt)
  pure ()
