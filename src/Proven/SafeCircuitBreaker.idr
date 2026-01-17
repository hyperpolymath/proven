-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeCircuitBreaker - Verified circuit breaker pattern
|||
||| Type-safe circuit breaker for fault tolerance in distributed systems.
||| Prevents cascading failures by failing fast when a service is unhealthy.
|||
||| States: Closed (normal) -> Open (failing) -> HalfOpen (testing)
module Proven.SafeCircuitBreaker

import Proven.Core
import Data.So

%default total

-- ============================================================================
-- CIRCUIT BREAKER STATE
-- ============================================================================

||| Circuit breaker states
public export
data CircuitState : Type where
  ||| Normal operation - requests pass through
  Closed : CircuitState
  ||| Service failing - requests fail fast
  Open : CircuitState
  ||| Testing recovery - limited requests allowed
  HalfOpen : CircuitState

export
Eq CircuitState where
  Closed == Closed = True
  Open == Open = True
  HalfOpen == HalfOpen = True
  _ == _ = False

export
Show CircuitState where
  show Closed = "Closed"
  show Open = "Open"
  show HalfOpen = "HalfOpen"

-- ============================================================================
-- CIRCUIT BREAKER CONFIGURATION
-- ============================================================================

||| Configuration for circuit breaker behavior
public export
record CircuitBreakerConfig where
  constructor MkConfig
  failureThreshold : Nat     -- Failures before opening
  successThreshold : Nat     -- Successes in half-open to close
  timeout : Integer          -- Milliseconds before trying half-open
  halfOpenMaxCalls : Nat     -- Max concurrent calls in half-open
  0 failureThresholdPos : So (failureThreshold > 0)
  0 successThresholdPos : So (successThreshold > 0)
  0 timeoutPos : So (timeout > 0)

||| Create a circuit breaker configuration
export
mkConfig : (failureThreshold : Nat)
        -> (successThreshold : Nat)
        -> (timeout : Integer)
        -> (halfOpenMaxCalls : Nat)
        -> Maybe CircuitBreakerConfig
mkConfig ft st to maxCalls =
  if ft == 0 || st == 0 || to <= 0
  then Nothing
  else Just (believe_me (MkConfig ft st to (max 1 maxCalls)))

||| Default configuration
export
defaultConfig : CircuitBreakerConfig
defaultConfig = believe_me (MkConfig 5 2 30000 1)

-- ============================================================================
-- CIRCUIT BREAKER
-- ============================================================================

||| Circuit breaker state machine
public export
record CircuitBreaker where
  constructor MkCircuitBreaker
  state : CircuitState
  failureCount : Nat
  successCount : Nat
  lastStateChange : Integer
  halfOpenCalls : Nat
  config : CircuitBreakerConfig

||| Create a new circuit breaker
export
newCircuitBreaker : CircuitBreakerConfig -> Integer -> CircuitBreaker
newCircuitBreaker cfg now = MkCircuitBreaker Closed 0 0 now 0 cfg

||| Get current state
export
getState : CircuitBreaker -> CircuitState
getState cb = cb.state

||| Check if circuit is allowing requests
export
isAllowing : CircuitBreaker -> Integer -> Bool
isAllowing cb now =
  case cb.state of
    Closed => True
    Open => now >= cb.lastStateChange + cb.config.timeout  -- Time to try half-open
    HalfOpen => cb.halfOpenCalls < cb.config.halfOpenMaxCalls

-- ============================================================================
-- STATE TRANSITIONS
-- ============================================================================

||| Attempt to acquire permission to make a call
||| Returns (allowed, updated breaker)
export
tryAcquire : CircuitBreaker -> Integer -> (Bool, CircuitBreaker)
tryAcquire cb now =
  case cb.state of
    Closed => (True, cb)
    Open =>
      if now >= cb.lastStateChange + cb.config.timeout
      then -- Transition to half-open
           (True, MkCircuitBreaker HalfOpen 0 0 now 1 cb.config)
      else (False, cb)
    HalfOpen =>
      if cb.halfOpenCalls < cb.config.halfOpenMaxCalls
      then (True, { halfOpenCalls := S cb.halfOpenCalls } cb)
      else (False, cb)

||| Record a successful call
export
recordSuccess : CircuitBreaker -> Integer -> CircuitBreaker
recordSuccess cb now =
  case cb.state of
    Closed =>
      -- Reset failure count on success
      { failureCount := 0 } cb
    HalfOpen =>
      let newSuccessCount = S cb.successCount
      in if newSuccessCount >= cb.config.successThreshold
         then -- Transition to closed
              MkCircuitBreaker Closed 0 0 now 0 cb.config
         else { successCount := newSuccessCount, halfOpenCalls $= pred } cb
    Open =>
      -- Should not happen, but handle gracefully
      cb

||| Record a failed call
export
recordFailure : CircuitBreaker -> Integer -> CircuitBreaker
recordFailure cb now =
  case cb.state of
    Closed =>
      let newFailureCount = S cb.failureCount
      in if newFailureCount >= cb.config.failureThreshold
         then -- Transition to open
              MkCircuitBreaker Open 0 0 now 0 cb.config
         else { failureCount := newFailureCount } cb
    HalfOpen =>
      -- Any failure in half-open returns to open
      MkCircuitBreaker Open 0 0 now 0 cb.config
    Open =>
      -- Already open, just update timestamp
      { lastStateChange := now } cb

-- ============================================================================
-- CALL RESULT
-- ============================================================================

||| Result of attempting a call through the circuit breaker
public export
data CallResult a : Type where
  ||| Call was allowed and succeeded
  Success : a -> CallResult a
  ||| Call was allowed but failed
  Failure : String -> CallResult a
  ||| Call was rejected (circuit open)
  Rejected : CallResult a

||| Check if call was successful
export
isSuccess : CallResult a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

||| Check if call was rejected
export
isRejected : CallResult a -> Bool
isRejected Rejected = True
isRejected _ = False

||| Extract value or use default
export
getOrDefault : a -> CallResult a -> a
getOrDefault _ (Success x) = x
getOrDefault def _ = def

-- ============================================================================
-- METRICS
-- ============================================================================

||| Circuit breaker metrics
public export
record CircuitBreakerMetrics where
  constructor MkMetrics
  totalCalls : Nat
  successfulCalls : Nat
  failedCalls : Nat
  rejectedCalls : Nat
  stateChanges : Nat

||| Create empty metrics
export
emptyMetrics : CircuitBreakerMetrics
emptyMetrics = MkMetrics 0 0 0 0 0

||| Update metrics for success
export
metricSuccess : CircuitBreakerMetrics -> CircuitBreakerMetrics
metricSuccess m = { totalCalls $= S, successfulCalls $= S } m

||| Update metrics for failure
export
metricFailure : CircuitBreakerMetrics -> CircuitBreakerMetrics
metricFailure m = { totalCalls $= S, failedCalls $= S } m

||| Update metrics for rejection
export
metricRejected : CircuitBreakerMetrics -> CircuitBreakerMetrics
metricRejected m = { totalCalls $= S, rejectedCalls $= S } m

||| Update metrics for state change
export
metricStateChange : CircuitBreakerMetrics -> CircuitBreakerMetrics
metricStateChange m = { stateChanges $= S } m

||| Calculate success rate (0.0 to 1.0)
export
successRate : CircuitBreakerMetrics -> Double
successRate m =
  let attempted = m.totalCalls `minus` m.rejectedCalls
  in if attempted == 0 then 1.0
     else cast m.successfulCalls / cast attempted

||| Calculate rejection rate (0.0 to 1.0)
export
rejectionRate : CircuitBreakerMetrics -> Double
rejectionRate m =
  if m.totalCalls == 0 then 0.0
  else cast m.rejectedCalls / cast m.totalCalls

-- ============================================================================
-- CIRCUIT BREAKER WITH METRICS
-- ============================================================================

||| Circuit breaker with built-in metrics tracking
public export
record MeteredCircuitBreaker where
  constructor MkMeteredCB
  breaker : CircuitBreaker
  metrics : CircuitBreakerMetrics

||| Create a metered circuit breaker
export
newMeteredCircuitBreaker : CircuitBreakerConfig -> Integer -> MeteredCircuitBreaker
newMeteredCircuitBreaker cfg now =
  MkMeteredCB (newCircuitBreaker cfg now) emptyMetrics

||| Try to acquire with metrics
export
meteredTryAcquire : MeteredCircuitBreaker -> Integer -> (Bool, MeteredCircuitBreaker)
meteredTryAcquire mcb now =
  let oldState = mcb.breaker.state
      (allowed, newBreaker) = tryAcquire mcb.breaker now
      newMetrics = if allowed then mcb.metrics
                   else metricRejected mcb.metrics
      stateChanged = oldState /= newBreaker.state
      finalMetrics = if stateChanged then metricStateChange newMetrics else newMetrics
  in (allowed, MkMeteredCB newBreaker finalMetrics)

||| Record success with metrics
export
meteredRecordSuccess : MeteredCircuitBreaker -> Integer -> MeteredCircuitBreaker
meteredRecordSuccess mcb now =
  let oldState = mcb.breaker.state
      newBreaker = recordSuccess mcb.breaker now
      newMetrics = metricSuccess mcb.metrics
      stateChanged = oldState /= newBreaker.state
      finalMetrics = if stateChanged then metricStateChange newMetrics else newMetrics
  in MkMeteredCB newBreaker finalMetrics

||| Record failure with metrics
export
meteredRecordFailure : MeteredCircuitBreaker -> Integer -> MeteredCircuitBreaker
meteredRecordFailure mcb now =
  let oldState = mcb.breaker.state
      newBreaker = recordFailure mcb.breaker now
      newMetrics = metricFailure mcb.metrics
      stateChanged = oldState /= newBreaker.state
      finalMetrics = if stateChanged then metricStateChange newMetrics else newMetrics
  in MkMeteredCB newBreaker finalMetrics

-- ============================================================================
-- HEALTH CHECK
-- ============================================================================

||| Health status of the circuit breaker
public export
data HealthStatus : Type where
  Healthy : HealthStatus
  Degraded : HealthStatus
  Unhealthy : HealthStatus

||| Get health status based on current state
export
healthStatus : CircuitBreaker -> HealthStatus
healthStatus cb =
  case cb.state of
    Closed => Healthy
    HalfOpen => Degraded
    Open => Unhealthy

||| Time until circuit might allow requests (in Open state)
export
timeUntilRetry : CircuitBreaker -> Integer -> Integer
timeUntilRetry cb now =
  case cb.state of
    Open => max 0 (cb.lastStateChange + cb.config.timeout - now)
    _ => 0
