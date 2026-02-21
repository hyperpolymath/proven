-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeCircuitBreaker - Safe circuit breaker pattern implementation
|||
||| This module provides a circuit breaker for fault tolerance,
||| preventing cascading failures in distributed systems.
module Proven.SafeCircuitBreaker
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Circuit Breaker State
--------------------------------------------------------------------------------

||| Circuit breaker states
public export
data CircuitState : Type where
  Closed : CircuitState      -- Normal operation, requests flow through
  Open : CircuitState        -- Failing, requests rejected immediately
  HalfOpen : CircuitState    -- Testing if service recovered

public export
Eq CircuitState where
  Closed == Closed = True
  Open == Open = True
  HalfOpen == HalfOpen = True
  _ == _ = False

public export
Show CircuitState where
  show Closed = "Closed"
  show Open = "Open"
  show HalfOpen = "HalfOpen"

--------------------------------------------------------------------------------
-- Circuit Breaker Configuration
--------------------------------------------------------------------------------

||| Circuit breaker configuration
public export
record CircuitConfig where
  constructor MkConfig
  failureThreshold : Nat     -- Failures before opening
  successThreshold : Nat     -- Successes to close from half-open
  timeout : Nat              -- Time before attempting half-open (time units)
  halfOpenMaxCalls : Nat     -- Max calls allowed in half-open state

||| Default circuit breaker configuration
public export
defaultConfig : CircuitConfig
defaultConfig = MkConfig 5 2 30 3

--------------------------------------------------------------------------------
-- Circuit Breaker Type
--------------------------------------------------------------------------------

||| Circuit breaker state
public export
record CircuitBreaker where
  constructor MkBreaker
  config : CircuitConfig
  state : CircuitState
  failures : Nat             -- Consecutive failures
  successes : Nat            -- Consecutive successes (in half-open)
  lastFailureTime : Nat      -- Timestamp of last failure
  halfOpenCalls : Nat        -- Calls made in half-open state

||| Create a new circuit breaker
public export
newBreaker : CircuitConfig -> CircuitBreaker
newBreaker cfg = MkBreaker cfg Closed 0 0 0 0

||| Create a circuit breaker with default config
public export
defaultBreaker : CircuitBreaker
defaultBreaker = newBreaker defaultConfig

--------------------------------------------------------------------------------
-- State Transitions
--------------------------------------------------------------------------------

||| Check if circuit should transition to half-open
public export
shouldTransitionToHalfOpen : (currentTime : Nat) -> CircuitBreaker -> Bool
shouldTransitionToHalfOpen now breaker =
  case breaker.state of
    Open => now >= breaker.lastFailureTime + breaker.config.timeout
    _ => False

||| Update circuit breaker state based on current time
public export
updateState : (currentTime : Nat) -> CircuitBreaker -> CircuitBreaker
updateState now breaker =
  if shouldTransitionToHalfOpen now breaker
    then MkBreaker breaker.config HalfOpen breaker.failures 0 breaker.lastFailureTime 0
    else breaker

||| Check if a request is allowed
public export
canExecute : (currentTime : Nat) -> CircuitBreaker -> Bool
canExecute now breaker =
  let updated = updateState now breaker
  in case updated.state of
       Closed => True
       Open => False
       HalfOpen => updated.halfOpenCalls < updated.config.halfOpenMaxCalls

--------------------------------------------------------------------------------
-- Recording Results
--------------------------------------------------------------------------------

||| Record a successful call
public export
recordSuccess : CircuitBreaker -> CircuitBreaker
recordSuccess breaker =
  case breaker.state of
    Closed => MkBreaker breaker.config Closed 0 0 breaker.lastFailureTime 0
    HalfOpen =>
      let newSuccesses = S breaker.successes
      in if newSuccesses >= breaker.config.successThreshold
           then MkBreaker breaker.config Closed 0 0 breaker.lastFailureTime 0
           else MkBreaker breaker.config HalfOpen breaker.failures newSuccesses breaker.lastFailureTime breaker.halfOpenCalls
    Open => breaker  -- Shouldn't happen, but preserve state

||| Record a failed call
public export
recordFailure : (currentTime : Nat) -> CircuitBreaker -> CircuitBreaker
recordFailure now breaker =
  case breaker.state of
    Closed =>
      let newFailures = S breaker.failures
      in if newFailures >= breaker.config.failureThreshold
           then MkBreaker breaker.config Open newFailures 0 now 0
           else MkBreaker breaker.config Closed newFailures 0 now 0
    HalfOpen => MkBreaker breaker.config Open (S breaker.failures) 0 now 0
    Open => MkBreaker breaker.config Open (S breaker.failures) 0 now 0

||| Record an attempt (for half-open tracking)
public export
recordAttempt : CircuitBreaker -> CircuitBreaker
recordAttempt breaker =
  case breaker.state of
    HalfOpen => MkBreaker breaker.config HalfOpen breaker.failures breaker.successes breaker.lastFailureTime (S breaker.halfOpenCalls)
    _ => breaker

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

||| Result of attempting to execute through circuit breaker
public export
data ExecuteResult a : Type where
  Executed : a -> ExecuteResult a
  CircuitOpen : (retryAfter : Nat) -> ExecuteResult a

||| Try to execute an action through the circuit breaker
public export
tryExecute : (currentTime : Nat) -> CircuitBreaker -> (result : Bool) -> (CircuitBreaker, Bool)
tryExecute now breaker result =
  let updated = updateState now breaker
  in if canExecute now updated
       then let attempted = recordAttempt updated
            in if result
                 then (recordSuccess attempted, True)
                 else (recordFailure now attempted, False)
       else (updated, False)

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

||| Get failure rate (0.0 to 1.0)
public export
failureRate : CircuitBreaker -> Double
failureRate breaker =
  let total = breaker.failures + breaker.successes
  in if total == 0 then 0.0
     else cast breaker.failures / cast total

||| Check if circuit is healthy
public export
isHealthy : CircuitBreaker -> Bool
isHealthy breaker = breaker.state == Closed

||| Get time until circuit might close
public export
timeUntilRetry : (currentTime : Nat) -> CircuitBreaker -> Nat
timeUntilRetry now breaker =
  case breaker.state of
    Open =>
      let retryTime = breaker.lastFailureTime + breaker.config.timeout
      in if now >= retryTime then 0 else minus retryTime now
    _ => 0

||| Force reset the circuit breaker
public export
reset : CircuitBreaker -> CircuitBreaker
reset breaker = MkBreaker breaker.config Closed 0 0 0 0

