-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeRetry operations
|||
||| This module exports retry logic and backoff strategy helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total and manage resilient error handling.
|||
||| Return conventions:
||| - Strategy → Int (Fixed=0, Linear=1, Exponential=2, Jittered=3)
||| - Result → Int (Success=0, Exhausted=1, NoRetry=2)
||| - Delay → Int (milliseconds)
||| - Decision → Int (0 = false/no, 1 = true/yes)
|||
||| CRITICAL: Retry logic prevents transient failures from causing system failures.
|||           Proper backoff prevents thundering herd and cascading failures.
|||
||| Backoff Strategies:
||| - Fixed: Same delay between all retries (simple, predictable)
||| - Linear: Delay increases linearly (initial + increment × attempt)
||| - Exponential: Delay doubles each time (initial × multiplier^attempt)
||| - Jittered: Exponential with randomness (prevents thundering herd)
|||
||| Retry Policies:
||| - retryOn: Predicate to determine which errors to retry
||| - maxAttempts: Maximum number of attempts (including first)
||| - maxDelay: Cap on delay to prevent unbounded waits
|||
||| State Tracking:
||| - attempt: Current attempt number (0-indexed)
||| - totalDelay: Cumulative delay across all retries
||| - lastError: Last error code encountered
module Proven.FFI.SafeRetry

import Proven.SafeRetry
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

--------------------------------------------------------------------------------
-- Backoff Strategy Encoding
--------------------------------------------------------------------------------

export
proven_idris_retry_backoff_fixed : Int
proven_idris_retry_backoff_fixed = 0

export
proven_idris_retry_backoff_linear : Int
proven_idris_retry_backoff_linear = 1

export
proven_idris_retry_backoff_exponential : Int
proven_idris_retry_backoff_exponential = 2

export
proven_idris_retry_backoff_jittered : Int
proven_idris_retry_backoff_jittered = 3

export
proven_idris_retry_is_valid_backoff_type : Int -> Int
proven_idris_retry_is_valid_backoff_type strategy =
  encodeBool (strategy >= 0 && strategy <= 3)

--------------------------------------------------------------------------------
-- Delay Calculation
--------------------------------------------------------------------------------

export
proven_idris_retry_fixed_delay : Int -> Int -> Int
proven_idris_retry_fixed_delay delay _ = delay

export
proven_idris_retry_linear_delay : Int -> Int -> Int -> Int
proven_idris_retry_linear_delay initial increment attempt =
  initial + (increment * attempt)

export
proven_idris_retry_exponential_delay : Int -> Int -> Int -> Int
proven_idris_retry_exponential_delay initial multiplier attempt =
  initial * power multiplier attempt
  where
    power : Int -> Int -> Int
    power _ 0 = 1
    power b n = if n > 0 then b * power b (n - 1) else 1

export
proven_idris_retry_delay_with_cap : Int -> Int -> Int
proven_idris_retry_delay_with_cap delay maxDelay =
  if delay > maxDelay then maxDelay else delay

export
proven_idris_retry_calculate_delay : Int -> Int -> Int -> Int -> Int -> Int
proven_idris_retry_calculate_delay backoffType initial increment multiplier attempt =
  if backoffType == 0 then initial  -- Fixed
  else if backoffType == 1 then proven_idris_retry_linear_delay initial increment attempt  -- Linear
  else if backoffType == 2 then proven_idris_retry_exponential_delay initial multiplier attempt  -- Exponential
  else if backoffType == 3 then proven_idris_retry_exponential_delay initial multiplier attempt  -- Jittered (base)
  else initial  -- Default to fixed

--------------------------------------------------------------------------------
-- Retry Decision Logic
--------------------------------------------------------------------------------

export
proven_idris_retry_can_retry : Int -> Int -> Int
proven_idris_retry_can_retry maxAttempts currentAttempt =
  encodeBool (currentAttempt + 1 < maxAttempts)

export
proven_idris_retry_remaining_attempts : Int -> Int -> Int
proven_idris_retry_remaining_attempts maxAttempts currentAttempt =
  if currentAttempt + 1 >= maxAttempts then 0
  else maxAttempts - currentAttempt - 1

export
proven_idris_retry_should_retry_on_code : Int -> Int -> Int
proven_idris_retry_should_retry_on_code errorCode retryableCode =
  encodeBool (errorCode == retryableCode)

export
proven_idris_retry_is_exhausted : Int -> Int -> Int
proven_idris_retry_is_exhausted maxAttempts currentAttempt =
  encodeBool (currentAttempt + 1 >= maxAttempts)

--------------------------------------------------------------------------------
-- State Tracking
--------------------------------------------------------------------------------

export
proven_idris_retry_next_attempt : Int -> Int
proven_idris_retry_next_attempt currentAttempt = currentAttempt + 1

export
proven_idris_retry_add_delay : Int -> Int -> Int
proven_idris_retry_add_delay totalDelay newDelay = totalDelay + newDelay

export
proven_idris_retry_initial_attempt : Int
proven_idris_retry_initial_attempt = 0

export
proven_idris_retry_initial_total_delay : Int
proven_idris_retry_initial_total_delay = 0

--------------------------------------------------------------------------------
-- Configuration Validation
--------------------------------------------------------------------------------

export
proven_idris_retry_is_valid_max_attempts : Int -> Int
proven_idris_retry_is_valid_max_attempts attempts =
  encodeBool (attempts > 0 && attempts <= 100)

export
proven_idris_retry_is_valid_initial_delay : Int -> Int
proven_idris_retry_is_valid_initial_delay delay =
  encodeBool (delay >= 0 && delay <= 300000)  -- Max 5 minutes

export
proven_idris_retry_is_valid_increment : Int -> Int
proven_idris_retry_is_valid_increment increment =
  encodeBool (increment >= 0 && increment <= 60000)  -- Max 1 minute

export
proven_idris_retry_is_valid_multiplier : Int -> Int
proven_idris_retry_is_valid_multiplier multiplier =
  encodeBool (multiplier >= 1 && multiplier <= 10)

export
proven_idris_retry_is_valid_max_delay : Int -> Int
proven_idris_retry_is_valid_max_delay maxDelay =
  encodeBool (maxDelay > 0 && maxDelay <= 3600000)  -- Max 1 hour

export
proven_idris_retry_is_valid_jitter : Int -> Int
proven_idris_retry_is_valid_jitter jitter =
  encodeBool (jitter >= 0 && jitter <= 10000)  -- Max 10 seconds jitter

--------------------------------------------------------------------------------
-- Retry Statistics
--------------------------------------------------------------------------------

export
proven_idris_retry_total_attempts : Int -> Int
proven_idris_retry_total_attempts currentAttempt = currentAttempt + 1

export
proven_idris_retry_success_rate : Int -> Int -> Double
proven_idris_retry_success_rate successes totalOps =
  if totalOps == 0 then 0.0
  else cast successes / cast totalOps

export
proven_idris_retry_success_rate_percent : Int -> Int -> Double
proven_idris_retry_success_rate_percent successes totalOps =
  proven_idris_retry_success_rate successes totalOps * 100.0

export
proven_idris_retry_average_attempts : Int -> Int -> Double
proven_idris_retry_average_attempts totalAttempts totalOps =
  if totalOps == 0 then 0.0
  else cast totalAttempts / cast totalOps

export
proven_idris_retry_average_delay : Int -> Int -> Double
proven_idris_retry_average_delay totalDelay totalOps =
  if totalOps == 0 then 0.0
  else cast totalDelay / cast totalOps

--------------------------------------------------------------------------------
-- Capacity Planning
--------------------------------------------------------------------------------

export
proven_idris_retry_max_total_delay : Int -> Int -> Int -> Int
proven_idris_retry_max_total_delay maxAttempts initialDelay multiplier =
  -- Calculate max delay for exponential backoff
  let helper : Int -> Int -> Int -> Int
      helper 0 _ acc = acc
      helper n delay acc =
        let nextDelay = delay * multiplier
        in helper (n - 1) nextDelay (acc + delay)
  in helper maxAttempts initialDelay 0

export
proven_idris_retry_recommend_max_attempts : Int -> Int
proven_idris_retry_recommend_max_attempts avgFailureRate =
  -- Recommend max attempts based on failure rate
  -- Higher failure rate needs more retries
  if avgFailureRate >= 50 then 5
  else if avgFailureRate >= 25 then 3
  else 2

export
proven_idris_retry_recommend_initial_delay : Int -> Int
proven_idris_retry_recommend_initial_delay avgResponseTime =
  -- Initial delay should be ~2x average response time
  avgResponseTime * 2

export
proven_idris_retry_recommend_multiplier : Int -> Int
proven_idris_retry_recommend_multiplier maxAttempts =
  -- Recommend multiplier based on max attempts
  -- Fewer attempts = higher multiplier for faster backoff
  if maxAttempts <= 2 then 4
  else if maxAttempts <= 4 then 2
  else 2

--------------------------------------------------------------------------------
-- Backoff Efficiency
--------------------------------------------------------------------------------

export
proven_idris_retry_is_aggressive : Int -> Int -> Int
proven_idris_retry_is_aggressive initialDelay maxAttempts =
  -- Aggressive if trying many times with short delays
  encodeBool (maxAttempts > 5 && initialDelay < 1000)

export
proven_idris_retry_is_conservative : Int -> Int -> Int
proven_idris_retry_is_conservative initialDelay maxAttempts =
  -- Conservative if few attempts with long delays
  encodeBool (maxAttempts <= 2 && initialDelay >= 5000)

export
proven_idris_retry_total_time_estimate : Int -> Int -> Int -> Int
proven_idris_retry_total_time_estimate backoffType maxAttempts initialDelay =
  -- Estimate total time for all retries (exponential with multiplier 2)
  if backoffType == 0 then initialDelay * maxAttempts  -- Fixed
  else if backoffType == 1 then  -- Linear with increment = initial
    let n = maxAttempts
        sum_n = (n * (n + 1)) `div` 2
    in initialDelay * sum_n
  else  -- Exponential/Jittered with multiplier 2
    proven_idris_retry_max_total_delay maxAttempts initialDelay 2

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

export
proven_idris_retry_is_retryable_code : Int -> Int
proven_idris_retry_is_retryable_code errorCode =
  -- Common retryable error codes (HTTP-like)
  encodeBool (
    errorCode == 429 ||  -- Too Many Requests
    errorCode == 503 ||  -- Service Unavailable
    errorCode == 504 ||  -- Gateway Timeout
    errorCode == 408 ||  -- Request Timeout
    errorCode == 502     -- Bad Gateway
  )

export
proven_idris_retry_is_permanent_failure : Int -> Int
proven_idris_retry_is_permanent_failure errorCode =
  -- Permanent failures that should not be retried
  encodeBool (
    errorCode == 400 ||  -- Bad Request
    errorCode == 401 ||  -- Unauthorized
    errorCode == 403 ||  -- Forbidden
    errorCode == 404 ||  -- Not Found
    errorCode == 405 ||  -- Method Not Allowed
    errorCode == 422     -- Unprocessable Entity
  )

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_retry_default_max_attempts : Int
proven_idris_retry_default_max_attempts = 3

export
proven_idris_retry_default_initial_delay : Int
proven_idris_retry_default_initial_delay = 100  -- 100ms

export
proven_idris_retry_default_multiplier : Int
proven_idris_retry_default_multiplier = 2

export
proven_idris_retry_default_max_delay : Int
proven_idris_retry_default_max_delay = 30000  -- 30 seconds

export
proven_idris_retry_default_linear_increment : Int
proven_idris_retry_default_linear_increment = 1000  -- 1 second

export
proven_idris_retry_default_jitter_max : Int
proven_idris_retry_default_jitter_max = 1000  -- 1 second

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_retry_friendly_error : String -> String
proven_idris_retry_friendly_error errorMsg =
  if isInfixOf "exhausted" (toLower errorMsg) || isInfixOf "max" (toLower errorMsg)
    then "All retry attempts exhausted (operation failed)"
  else if isInfixOf "no retry" (toLower errorMsg) || isInfixOf "permanent" (toLower errorMsg)
    then "Operation failed with permanent error (not retryable)"
  else if isInfixOf "delay" (toLower errorMsg) || isInfixOf "backoff" (toLower errorMsg)
    then "Invalid retry delay configuration"
  else if isInfixOf "attempts" (toLower errorMsg)
    then "Invalid max attempts configuration"
  else
    "Retry operation error"

export
proven_idris_retry_backoff_description : Int -> String
proven_idris_retry_backoff_description backoffType =
  if backoffType == 0 then "Fixed delay (same wait between retries)"
  else if backoffType == 1 then "Linear backoff (delay increases linearly)"
  else if backoffType == 2 then "Exponential backoff (delay doubles each time)"
  else if backoffType == 3 then "Jittered backoff (exponential with randomness)"
  else "Unknown backoff strategy"
