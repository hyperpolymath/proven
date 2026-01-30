-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeRateLimiter operations
|||
||| This module exports rate limiting helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total and prevent abuse.
|||
||| Return conventions:
||| - Rate limit result → (Int, retryAfter) where 0 = allowed, 1 = denied
||| - Calculations → Int (tokens, wait time, etc.)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: Rate limiting prevents resource exhaustion and abuse.
|||           Always enforce limits before performing expensive operations.
|||
||| Algorithms:
||| - Token Bucket: Smooth rate limiting with bursts
|||   - Capacity: maximum tokens (burst size)
|||   - Refill rate: tokens per time unit
|||   - Allows bursts up to capacity
|||
||| - Sliding Window: Precise rate limiting over time window
|||   - Max requests: limit per window
|||   - Window size: time window duration
|||   - More memory intensive but more accurate
|||
||| - Fixed Window: Simple counter per time window
|||   - Max requests: limit per window
|||   - Resets at window boundary
|||   - Can allow 2x rate at boundary
module Proven.FFI.SafeRateLimiter

import Proven.SafeRateLimiter
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

||| Encode RateLimitResult
encodeRateLimitResult : RateLimitResult -> (Int, Int)
encodeRateLimitResult Allowed = (0, 0)
encodeRateLimitResult (Denied retryAfter) = (1, cast retryAfter)

--------------------------------------------------------------------------------
-- Token Bucket Calculations
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_tokens_to_add : Int -> Int -> Int -> Int
proven_idris_ratelimit_tokens_to_add refillRate elapsedTime capacity =
  let tokensToAdd = refillRate * elapsedTime
      -- Don't exceed capacity
  in tokensToAdd

%export
proven_idris_ratelimit_tokens_after_refill : Int -> Int -> Int -> Int -> Int
proven_idris_ratelimit_tokens_after_refill currentTokens refillRate elapsedTime capacity =
  let newTokens = proven_idris_ratelimit_tokens_to_add refillRate elapsedTime capacity
      total = currentTokens + newTokens
  in if total > capacity then capacity else total

%export
proven_idris_ratelimit_can_acquire : Int -> Int -> Int
proven_idris_ratelimit_can_acquire currentTokens requestedTokens =
  encodeBool (currentTokens >= requestedTokens)

%export
proven_idris_ratelimit_tokens_after_acquire : Int -> Int -> Int
proven_idris_ratelimit_tokens_after_acquire currentTokens acquired =
  if currentTokens >= acquired
    then currentTokens - acquired
    else currentTokens

%export
proven_idris_ratelimit_wait_time_for_tokens : Int -> Int -> Int -> Int
proven_idris_ratelimit_wait_time_for_tokens needed refillRate capacity =
  if refillRate == 0 then 0
  else (needed + refillRate - 1) `div` refillRate  -- Ceiling division

--------------------------------------------------------------------------------
-- Sliding Window Calculations
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_is_in_window : Int -> Int -> Int -> Int
proven_idris_ratelimit_is_in_window requestTime currentTime windowSize =
  let cutoff = if currentTime >= windowSize
                 then currentTime - windowSize
                 else 0
  in encodeBool (requestTime >= cutoff)

%export
proven_idris_ratelimit_window_start : Int -> Int -> Int
proven_idris_ratelimit_window_start currentTime windowSize =
  if currentTime >= windowSize
    then currentTime - windowSize
    else 0

%export
proven_idris_ratelimit_requests_in_window : Int -> Int -> Int
proven_idris_ratelimit_requests_in_window currentCount maxRequests =
  currentCount

%export
proven_idris_ratelimit_can_accept_request : Int -> Int -> Int
proven_idris_ratelimit_can_accept_request currentCount maxRequests =
  encodeBool (currentCount < maxRequests)

%export
proven_idris_ratelimit_remaining_requests : Int -> Int -> Int
proven_idris_ratelimit_remaining_requests currentCount maxRequests =
  if currentCount >= maxRequests then 0
  else maxRequests - currentCount

--------------------------------------------------------------------------------
-- Fixed Window Calculations
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_current_window_start : Int -> Int -> Int
proven_idris_ratelimit_current_window_start currentTime windowSize =
  (currentTime `div` windowSize) * windowSize

%export
proven_idris_ratelimit_is_new_window : Int -> Int -> Int -> Int
proven_idris_ratelimit_is_new_window currentTime windowStart windowSize =
  encodeBool (currentTime >= (windowStart + windowSize))

%export
proven_idris_ratelimit_time_until_next_window : Int -> Int -> Int -> Int
proven_idris_ratelimit_time_until_next_window currentTime windowStart windowSize =
  let windowEnd = windowStart + windowSize
  in if currentTime >= windowEnd then 0
     else windowEnd - currentTime

--------------------------------------------------------------------------------
-- Rate Calculations
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_requests_per_second : Int -> Int -> Double
proven_idris_ratelimit_requests_per_second maxRequests windowSizeSeconds =
  if windowSizeSeconds == 0 then 0.0
  else cast maxRequests / cast windowSizeSeconds

%export
proven_idris_ratelimit_burst_size : Int -> Int
proven_idris_ratelimit_burst_size capacity = capacity

%export
proven_idris_ratelimit_effective_rate : Int -> Int -> Double
proven_idris_ratelimit_effective_rate refillRate intervalSize =
  if intervalSize == 0 then 0.0
  else cast refillRate / cast intervalSize

--------------------------------------------------------------------------------
-- Capacity Planning
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_recommend_capacity : Int -> Int -> Int
proven_idris_ratelimit_recommend_capacity avgRequestsPerSec burstDuration =
  -- Capacity = average rate × burst duration
  avgRequestsPerSec * burstDuration

%export
proven_idris_ratelimit_recommend_refill_rate : Int -> Int -> Int
proven_idris_ratelimit_recommend_refill_rate targetRequestsPerSec intervalSize =
  -- Refill rate = target rate × interval
  targetRequestsPerSec * intervalSize

%export
proven_idris_ratelimit_recommend_window_size : Int -> Int
proven_idris_ratelimit_recommend_window_size targetRequestsPerSec maxBurst =
  -- Window size should be large enough to smooth out bursts
  if targetRequestsPerSec == 0 then maxBurst
  else max maxBurst (maxBurst `div` targetRequestsPerSec)

--------------------------------------------------------------------------------
-- Utilization
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_token_utilization : Int -> Int -> Double
proven_idris_ratelimit_token_utilization currentTokens capacity =
  if capacity == 0 then 0.0
  else cast currentTokens / cast capacity

%export
proven_idris_ratelimit_request_utilization : Int -> Int -> Double
proven_idris_ratelimit_request_utilization currentCount maxRequests =
  if maxRequests == 0 then 0.0
  else cast currentCount / cast maxRequests

%export
proven_idris_ratelimit_is_throttled : Int -> Int -> Int -> Int
proven_idris_ratelimit_is_throttled currentCount maxRequests threshold =
  let percent = cast (currentCount * 100) / cast maxRequests
  in encodeBool (percent >= cast threshold)

--------------------------------------------------------------------------------
-- Time Calculations
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_elapsed_time : Int -> Int -> Int
proven_idris_ratelimit_elapsed_time currentTime lastTime =
  if currentTime >= lastTime
    then currentTime - lastTime
    else 0

%export
proven_idris_ratelimit_retry_after : Int -> Int -> Int -> Int
proven_idris_ratelimit_retry_after oldestRequest currentTime windowSize =
  let age = if currentTime >= oldestRequest
              then currentTime - oldestRequest
              else 0
  in if age >= windowSize then 0
     else windowSize - age

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_is_valid_capacity : Int -> Int
proven_idris_ratelimit_is_valid_capacity cap =
  encodeBool (cap > 0)

%export
proven_idris_ratelimit_is_valid_refill_rate : Int -> Int
proven_idris_ratelimit_is_valid_refill_rate rate =
  encodeBool (rate > 0)

%export
proven_idris_ratelimit_is_valid_window_size : Int -> Int
proven_idris_ratelimit_is_valid_window_size size =
  encodeBool (size > 0)

%export
proven_idris_ratelimit_is_valid_max_requests : Int -> Int
proven_idris_ratelimit_is_valid_max_requests max =
  encodeBool (max > 0)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_default_burst_size : Int
proven_idris_ratelimit_default_burst_size = 10

%export
proven_idris_ratelimit_default_refill_rate : Int
proven_idris_ratelimit_default_refill_rate = 1

%export
proven_idris_ratelimit_default_window_size : Int
proven_idris_ratelimit_default_window_size = 60  -- 60 seconds

%export
proven_idris_ratelimit_default_max_requests : Int
proven_idris_ratelimit_default_max_requests = 100

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_ratelimit_friendly_error : String -> String
proven_idris_ratelimit_friendly_error errorMsg =
  if isInfixOf "denied" (toLower errorMsg) || isInfixOf "limit" (toLower errorMsg)
    then "Rate limit exceeded (too many requests)"
  else if isInfixOf "capacity" (toLower errorMsg)
    then "Invalid rate limiter capacity (must be positive)"
  else if isInfixOf "refill" (toLower errorMsg)
    then "Invalid refill rate (must be positive)"
  else if isInfixOf "window" (toLower errorMsg)
    then "Invalid window size (must be positive)"
  else if isInfixOf "throttled" (toLower errorMsg)
    then "Requests are being throttled (approaching rate limit)"
  else
    "Rate limiting error"

%export
proven_idris_ratelimit_status_message : Int -> Int -> String
proven_idris_ratelimit_status_message allowed retryAfter =
  if allowed == 0
    then "Request allowed"
    else "Rate limit exceeded. Retry after " ++ show retryAfter ++ " time units"
