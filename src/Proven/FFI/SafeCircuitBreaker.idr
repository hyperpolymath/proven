-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCircuitBreaker operations
|||
||| This module exports circuit breaker pattern helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total and manage fault tolerance.
|||
||| Return conventions:
||| - State → Int (Closed=0, Open=1, HalfOpen=2)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Statistics → Int (counts, percentages)
||| - Time → Int (milliseconds, seconds)
|||
||| CRITICAL: Circuit breakers prevent cascading failures by stopping operations
|||           to failing services and allowing recovery time.
|||
||| States:
||| - Closed: Normal operation, requests allowed
||| - Open: Service failing, requests blocked immediately
||| - HalfOpen: Testing recovery, limited requests allowed
|||
||| Transitions:
||| - Closed → Open: After failure threshold exceeded
||| - Open → HalfOpen: After timeout period
||| - HalfOpen → Closed: After success threshold met
||| - HalfOpen → Open: On any failure during testing
|||
||| Configuration:
||| - failureThreshold: Failures before opening circuit
||| - successThreshold: Successes to close from half-open
||| - timeout: Wait time before trying half-open (ms)
||| - halfOpenMaxCalls: Max requests during half-open
module Proven.FFI.SafeCircuitBreaker

import Proven.SafeCircuitBreaker
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode CircuitState
encodeCircuitState : CircuitState -> Int
encodeCircuitState Closed = 0
encodeCircuitState Open = 1
encodeCircuitState HalfOpen = 2

||| Decode CircuitState
decodeCircuitState : Int -> Maybe CircuitState
decodeCircuitState 0 = Just Closed
decodeCircuitState 1 = Just Open
decodeCircuitState 2 = Just HalfOpen
decodeCircuitState _ = Nothing

--------------------------------------------------------------------------------
-- State Encoding
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_state_closed : Int
proven_idris_circuitbreaker_state_closed = 0

export
proven_idris_circuitbreaker_state_open : Int
proven_idris_circuitbreaker_state_open = 1

export
proven_idris_circuitbreaker_state_halfopen : Int
proven_idris_circuitbreaker_state_halfopen = 2

export
proven_idris_circuitbreaker_is_valid_state : Int -> Int
proven_idris_circuitbreaker_is_valid_state state =
  encodeBool (state == 0 || state == 1 || state == 2)

--------------------------------------------------------------------------------
-- State Transitions
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_should_open : Int -> Int -> Int
proven_idris_circuitbreaker_should_open consecutiveFailures failureThreshold =
  encodeBool (consecutiveFailures >= failureThreshold)

export
proven_idris_circuitbreaker_should_attempt_halfopen : Int -> Int -> Int -> Int
proven_idris_circuitbreaker_should_attempt_halfopen currentTime openedAt timeout =
  let elapsed = if currentTime >= openedAt
                  then currentTime - openedAt
                  else 0
  in encodeBool (elapsed >= timeout)

export
proven_idris_circuitbreaker_should_close : Int -> Int -> Int
proven_idris_circuitbreaker_should_close consecutiveSuccesses successThreshold =
  encodeBool (consecutiveSuccesses >= successThreshold)

export
proven_idris_circuitbreaker_should_reopen : Int -> Int
proven_idris_circuitbreaker_should_reopen state failureOccurred =
  encodeBool (state == 2 && failureOccurred == 1)  -- HalfOpen + failure

--------------------------------------------------------------------------------
-- Execution Gating
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_can_execute : Int -> Int -> Int -> Int
proven_idris_circuitbreaker_can_execute state halfOpenCalls halfOpenMaxCalls =
  if state == 0 then 1  -- Closed: allow
  else if state == 1 then 0  -- Open: deny
  else if halfOpenCalls < halfOpenMaxCalls then 1  -- HalfOpen: limited
  else 0  -- HalfOpen exceeded limit

export
proven_idris_circuitbreaker_time_until_retry : Int -> Int -> Int -> Int
proven_idris_circuitbreaker_time_until_retry currentTime openedAt timeout =
  let elapsed = if currentTime >= openedAt
                  then currentTime - openedAt
                  else 0
  in if elapsed >= timeout then 0
     else timeout - elapsed

export
proven_idris_circuitbreaker_remaining_halfopen_calls : Int -> Int -> Int
proven_idris_circuitbreaker_remaining_halfopen_calls halfOpenCalls halfOpenMaxCalls =
  if halfOpenCalls >= halfOpenMaxCalls then 0
  else halfOpenMaxCalls - halfOpenCalls

--------------------------------------------------------------------------------
-- Failure/Success Recording
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_record_failure : Int -> Int
proven_idris_circuitbreaker_record_failure consecutiveFailures =
  consecutiveFailures + 1

export
proven_idris_circuitbreaker_record_success : Int -> Int
proven_idris_circuitbreaker_record_success consecutiveSuccesses =
  consecutiveSuccesses + 1

export
proven_idris_circuitbreaker_reset_failure_count : Int
proven_idris_circuitbreaker_reset_failure_count = 0

export
proven_idris_circuitbreaker_reset_success_count : Int
proven_idris_circuitbreaker_reset_success_count = 0

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_failure_rate : Int -> Int -> Double
proven_idris_circuitbreaker_failure_rate failures total =
  if total == 0 then 0.0
  else cast failures / cast total

export
proven_idris_circuitbreaker_failure_rate_percent : Int -> Int -> Double
proven_idris_circuitbreaker_failure_rate_percent failures total =
  proven_idris_circuitbreaker_failure_rate failures total * 100.0

export
proven_idris_circuitbreaker_success_rate : Int -> Int -> Double
proven_idris_circuitbreaker_success_rate successes total =
  if total == 0 then 0.0
  else cast successes / cast total

export
proven_idris_circuitbreaker_success_rate_percent : Int -> Int -> Double
proven_idris_circuitbreaker_success_rate_percent successes total =
  proven_idris_circuitbreaker_success_rate successes total * 100.0

export
proven_idris_circuitbreaker_uptime_percent : Int -> Int -> Double
proven_idris_circuitbreaker_uptime_percent closedTime totalTime =
  if totalTime == 0 then 0.0
  else cast closedTime / cast totalTime * 100.0

--------------------------------------------------------------------------------
-- Configuration Validation
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_is_valid_failure_threshold : Int -> Int
proven_idris_circuitbreaker_is_valid_failure_threshold threshold =
  encodeBool (threshold > 0 && threshold <= 1000)

export
proven_idris_circuitbreaker_is_valid_success_threshold : Int -> Int
proven_idris_circuitbreaker_is_valid_success_threshold threshold =
  encodeBool (threshold > 0 && threshold <= 1000)

export
proven_idris_circuitbreaker_is_valid_timeout : Int -> Int
proven_idris_circuitbreaker_is_valid_timeout timeout =
  encodeBool (timeout > 0 && timeout <= 3600000)  -- Max 1 hour

export
proven_idris_circuitbreaker_is_valid_halfopen_max : Int -> Int
proven_idris_circuitbreaker_is_valid_halfopen_max maxCalls =
  encodeBool (maxCalls > 0 && maxCalls <= 100)

export
proven_idris_circuitbreaker_is_valid_config : Int -> Int -> Int -> Int -> Int
proven_idris_circuitbreaker_is_valid_config failureThreshold successThreshold timeout halfOpenMax =
  let validFailure = proven_idris_circuitbreaker_is_valid_failure_threshold failureThreshold
      validSuccess = proven_idris_circuitbreaker_is_valid_success_threshold successThreshold
      validTimeout = proven_idris_circuitbreaker_is_valid_timeout timeout
      validHalfOpen = proven_idris_circuitbreaker_is_valid_halfopen_max halfOpenMax
  in if validFailure == 1 && validSuccess == 1 && validTimeout == 1 && validHalfOpen == 1
       then 1
       else 0

--------------------------------------------------------------------------------
-- Health Checks
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_is_healthy : Int -> Int -> Int
proven_idris_circuitbreaker_is_healthy state consecutiveFailures =
  encodeBool (state == 0 && consecutiveFailures == 0)  -- Closed with no failures

export
proven_idris_circuitbreaker_is_degraded : Int -> Int -> Int -> Int
proven_idris_circuitbreaker_is_degraded state consecutiveFailures failureThreshold =
  let ratio = if failureThreshold == 0 then 0
               else (consecutiveFailures * 100) `div` failureThreshold
  in encodeBool (state == 0 && ratio >= 50)  -- Closed but approaching threshold

export
proven_idris_circuitbreaker_is_failing : Int -> Int
proven_idris_circuitbreaker_is_failing state =
  encodeBool (state == 1)  -- Open

export
proven_idris_circuitbreaker_is_recovering : Int -> Int
proven_idris_circuitbreaker_is_recovering state =
  encodeBool (state == 2)  -- HalfOpen

--------------------------------------------------------------------------------
-- Capacity Planning
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_recommend_failure_threshold : Int -> Double -> Int
proven_idris_circuitbreaker_recommend_failure_threshold avgRequests errorBudget =
  -- Failure threshold = avg requests × error budget
  -- Example: 1000 requests/min, 1% error budget = 10 failures
  cast (cast avgRequests * errorBudget)

export
proven_idris_circuitbreaker_recommend_timeout : Int -> Int
proven_idris_circuitbreaker_recommend_timeout avgResponseTime =
  -- Timeout = 3x average response time (in ms)
  avgResponseTime * 3

export
proven_idris_circuitbreaker_recommend_halfopen_max : Int -> Int
proven_idris_circuitbreaker_recommend_halfopen_max successThreshold =
  -- Allow 2x success threshold during half-open testing
  successThreshold * 2

--------------------------------------------------------------------------------
-- Time Calculations
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_elapsed_time : Int -> Int -> Int
proven_idris_circuitbreaker_elapsed_time currentTime startTime =
  if currentTime >= startTime
    then currentTime - startTime
    else 0

export
proven_idris_circuitbreaker_time_in_state : Int -> Int -> Int
proven_idris_circuitbreaker_time_in_state currentTime stateEnteredAt =
  proven_idris_circuitbreaker_elapsed_time currentTime stateEnteredAt

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_default_failure_threshold : Int
proven_idris_circuitbreaker_default_failure_threshold = 5

export
proven_idris_circuitbreaker_default_success_threshold : Int
proven_idris_circuitbreaker_default_success_threshold = 2

export
proven_idris_circuitbreaker_default_timeout : Int
proven_idris_circuitbreaker_default_timeout = 60000  -- 60 seconds

export
proven_idris_circuitbreaker_default_halfopen_max : Int
proven_idris_circuitbreaker_default_halfopen_max = 3

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_circuitbreaker_friendly_error : String -> String
proven_idris_circuitbreaker_friendly_error errorMsg =
  if isInfixOf "open" (toLower errorMsg) || isInfixOf "circuit" (toLower errorMsg)
    then "Circuit breaker is open (service unavailable, will retry after timeout)"
  else if isInfixOf "half" (toLower errorMsg) || isInfixOf "testing" (toLower errorMsg)
    then "Circuit breaker is testing recovery (limited requests allowed)"
  else if isInfixOf "threshold" (toLower errorMsg)
    then "Circuit breaker threshold configuration invalid"
  else if isInfixOf "timeout" (toLower errorMsg)
    then "Circuit breaker timeout configuration invalid"
  else if isInfixOf "failure" (toLower errorMsg)
    then "Circuit breaker opened due to consecutive failures"
  else
    "Circuit breaker error"

export
proven_idris_circuitbreaker_state_message : Int -> String
proven_idris_circuitbreaker_state_message state =
  if state == 0 then "Circuit breaker: Closed (normal operation)"
  else if state == 1 then "Circuit breaker: Open (service failing, requests blocked)"
  else if state == 2 then "Circuit breaker: Half-Open (testing recovery)"
  else "Circuit breaker: Unknown state"
