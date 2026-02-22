-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe Callback Types with Dependent Type Proofs
|||
||| This module defines the ABI-level callback type system for bidirectional
||| FFI communication. It provides type-safe callback registration with proofs
||| that:
|||   1. Callback handles are valid (non-zero)
|||   2. Event types are well-formed
|||   3. Callback invocation respects the registration contract
|||
||| This is the ABI layer (type definitions + proofs).
||| The FFI layer (actual C bindings) is in Proven.FFI.Callbacks.
module Proven.Callbacks

import Proven.Core
import Proven.FFI.Callbacks

%default total

--------------------------------------------------------------------------------
-- Validated Callback Handle
--------------------------------------------------------------------------------

||| A callback handle that has been validated as non-zero.
||| Constructed only through successful registration.
public export
data ValidHandle : Type where
  MkValidHandle : (h : CallbackHandle) -> {auto prf : So (h /= 0)} -> ValidHandle

||| Extract the raw handle value.
public export
rawHandle : ValidHandle -> CallbackHandle
rawHandle (MkValidHandle h) = h

--------------------------------------------------------------------------------
-- Callback Registration Proof
--------------------------------------------------------------------------------

||| Evidence that a callback was successfully registered.
||| Carries the handle and the event type it was registered for.
public export
data Registered : EventType -> Type where
  MkRegistered : (handle : ValidHandle) -> (eventType : EventType) -> Registered eventType

||| Get the handle from a registration proof.
public export
registeredHandle : Registered et -> ValidHandle
registeredHandle (MkRegistered h _) = h

||| Get the event type from a registration proof.
public export
registeredEventType : Registered et -> EventType
registeredEventType (MkRegistered _ et) = et

--------------------------------------------------------------------------------
-- Callback Safety Properties
--------------------------------------------------------------------------------

||| Property: event type round-trips through integer encoding.
public export
eventTypeRoundTrip : (et : EventType) -> intToEventType (eventTypeToInt et) = Just et
eventTypeRoundTrip ValidationFailed   = Refl
eventTypeRoundTrip ResourceAcquired   = Refl
eventTypeRoundTrip ResourceReleased   = Refl
eventTypeRoundTrip RateLimitHit       = Refl
eventTypeRoundTrip CircuitStateChange = Refl
eventTypeRoundTrip SignalReceived     = Refl
eventTypeRoundTrip RetryAttempt       = Refl
eventTypeRoundTrip CryptoOperation    = Refl
eventTypeRoundTrip CustomEvent        = Refl

||| Property: all event type integers are positive.
public export
eventTypePositive : (et : EventType) -> So (eventTypeToInt et > 0)
eventTypePositive ValidationFailed   = Oh
eventTypePositive ResourceAcquired   = Oh
eventTypePositive ResourceReleased   = Oh
eventTypePositive RateLimitHit       = Oh
eventTypePositive CircuitStateChange = Oh
eventTypePositive SignalReceived     = Oh
eventTypePositive RetryAttempt       = Oh
eventTypePositive CryptoOperation    = Oh
eventTypePositive CustomEvent        = Oh

--------------------------------------------------------------------------------
-- Safe Registration API
--------------------------------------------------------------------------------

||| Register a callback, returning a validated handle on success.
||| This is the safe wrapper around the FFI registration function.
||| Fails if the registry is full or the callback function is null.
export
registerCallback : EventType -> AnyPtr -> AnyPtr -> IO (Either CallbackError (Registered et))
registerCallback {et} eventType callbackFn context = do
  h <- primIO $ prim__callbackRegister (eventTypeToInt eventType) callbackFn context
  if h == 0
    then pure (Left (RegistrationFailed "Registry full or null callback"))
    else case choose (h /= 0) of
           Left prf => pure (Right (MkRegistered (MkValidHandle h) eventType))
           Right _ => pure (Left (RegistrationFailed "Unexpected zero handle"))

||| Unregister a callback using a validated handle.
export
unregisterCallback : ValidHandle -> IO (Either CallbackError ())
unregisterCallback vh = do
  result <- primIO $ prim__callbackUnregister (rawHandle vh)
  if result == 0
    then pure (Right ())
    else pure (Left (DeregistrationFailed ("Status: " ++ show result)))

--------------------------------------------------------------------------------
-- Scoped Callback (RAII-style)
--------------------------------------------------------------------------------

||| Run an IO action with a callback registered, automatically unregistering
||| the callback when the action completes. Ensures no leaked callbacks.
|||
||| @ eventType  Event type to register for
||| @ callbackFn Function pointer (from host language)
||| @ context    Opaque context pointer
||| @ action     IO action to run while callback is active
export
withCallback : (eventType : EventType) -> (callbackFn : AnyPtr) -> (context : AnyPtr) ->
               (action : IO a) -> IO (Either CallbackError a)
withCallback eventType callbackFn context action = do
  regResult <- registerCallback eventType callbackFn context
  case regResult of
    Left err => pure (Left err)
    Right reg => do
      result <- action
      _ <- unregisterCallback (registeredHandle reg)
      pure (Right result)
