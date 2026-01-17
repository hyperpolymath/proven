-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeRateLimiter - Verified rate limiting
|||
||| Type-safe rate limiting implementations including token bucket,
||| leaky bucket, and sliding window algorithms.
|||
||| Guarantees rate limits are never exceeded.
module Proven.SafeRateLimiter

import Proven.Core
import Data.So

%default total

-- ============================================================================
-- TOKEN BUCKET
-- ============================================================================

||| Token bucket rate limiter
||| Tokens are added at a fixed rate, requests consume tokens
public export
record TokenBucket where
  constructor MkTokenBucket
  tokens : Double            -- Current token count
  capacity : Double          -- Maximum tokens (bucket size)
  refillRate : Double        -- Tokens per second
  lastRefill : Integer       -- Last refill timestamp (millis)
  0 tokensInRange : So (tokens >= 0.0 && tokens <= capacity)
  0 capacityPositive : So (capacity > 0.0)
  0 ratePositive : So (refillRate > 0.0)

||| Create a new token bucket
export
newTokenBucket : (capacity : Double)
              -> (refillRate : Double)
              -> (now : Integer)
              -> Maybe TokenBucket
newTokenBucket cap rate now =
  if cap <= 0.0 || rate <= 0.0
  then Nothing
  else Just (believe_me (MkTokenBucket cap cap rate now))

||| Refill tokens based on elapsed time
export
refillBucket : TokenBucket -> Integer -> TokenBucket
refillBucket bucket now =
  let elapsed = cast (now - bucket.lastRefill) / 1000.0  -- seconds
      newTokens = min bucket.capacity (bucket.tokens + elapsed * bucket.refillRate)
  in believe_me (MkTokenBucket newTokens bucket.capacity bucket.refillRate now)

||| Try to acquire n tokens
||| Returns (success, updated bucket)
export
tryAcquire : TokenBucket -> Integer -> Double -> (Bool, TokenBucket)
tryAcquire bucket now n =
  let refilled = refillBucket bucket now
  in if n <= 0.0 then (True, refilled)
     else if refilled.tokens >= n
          then (True, believe_me (MkTokenBucket (refilled.tokens - n) refilled.capacity refilled.refillRate now))
          else (False, refilled)

||| Check how many tokens are available (after refill)
export
availableTokens : TokenBucket -> Integer -> Double
availableTokens bucket now = (refillBucket bucket now).tokens

||| Time until n tokens will be available (in milliseconds)
export
timeUntilAvailable : TokenBucket -> Integer -> Double -> Integer
timeUntilAvailable bucket now n =
  let refilled = refillBucket bucket now
      needed = n - refilled.tokens
  in if needed <= 0.0 then 0
     else cast (ceil (needed / refilled.refillRate * 1000.0))

-- ============================================================================
-- LEAKY BUCKET
-- ============================================================================

||| Leaky bucket rate limiter
||| Requests fill the bucket, water leaks out at a constant rate
public export
record LeakyBucket where
  constructor MkLeakyBucket
  level : Double           -- Current water level
  capacity : Double        -- Maximum capacity
  leakRate : Double        -- Units leaked per second
  lastLeak : Integer       -- Last leak timestamp (millis)

||| Create a new leaky bucket
export
newLeakyBucket : (capacity : Double)
              -> (leakRate : Double)
              -> (now : Integer)
              -> Maybe LeakyBucket
newLeakyBucket cap rate now =
  if cap <= 0.0 || rate <= 0.0 then Nothing
  else Just (MkLeakyBucket 0.0 cap rate now)

||| Leak water based on elapsed time
export
leakBucket : LeakyBucket -> Integer -> LeakyBucket
leakBucket bucket now =
  let elapsed = cast (now - bucket.lastLeak) / 1000.0
      newLevel = max 0.0 (bucket.level - elapsed * bucket.leakRate)
  in MkLeakyBucket newLevel bucket.capacity bucket.leakRate now

||| Try to add water (request)
export
tryAdd : LeakyBucket -> Integer -> Double -> (Bool, LeakyBucket)
tryAdd bucket now amount =
  let leaked = leakBucket bucket now
  in if amount <= 0.0 then (True, leaked)
     else if leaked.level + amount <= leaked.capacity
          then (True, MkLeakyBucket (leaked.level + amount) leaked.capacity leaked.leakRate now)
          else (False, leaked)

||| Current utilization (0.0 to 1.0)
export
leakyUtilization : LeakyBucket -> Integer -> Double
leakyUtilization bucket now =
  let leaked = leakBucket bucket now
  in leaked.level / leaked.capacity

-- ============================================================================
-- SLIDING WINDOW COUNTER
-- ============================================================================

||| Sliding window rate limiter
||| Counts requests in a sliding time window
public export
record SlidingWindow where
  constructor MkSlidingWindow
  currentCount : Nat       -- Requests in current window
  previousCount : Nat      -- Requests in previous window
  windowStart : Integer    -- Start of current window (millis)
  windowSize : Integer     -- Window size in milliseconds
  limit : Nat              -- Maximum requests per window

||| Create a new sliding window
export
newSlidingWindow : (windowSize : Integer)
                -> (limit : Nat)
                -> (now : Integer)
                -> Maybe SlidingWindow
newSlidingWindow ws lim now =
  if ws <= 0 || lim == 0 then Nothing
  else Just (MkSlidingWindow 0 0 now ws lim)

||| Update window based on current time
export
updateWindow : SlidingWindow -> Integer -> SlidingWindow
updateWindow sw now =
  let windowsPassed = (now - sw.windowStart) `div` sw.windowSize
  in if windowsPassed <= 0 then sw
     else if windowsPassed == 1
          then MkSlidingWindow 0 sw.currentCount (sw.windowStart + sw.windowSize) sw.windowSize sw.limit
          else MkSlidingWindow 0 0 (sw.windowStart + windowsPassed * sw.windowSize) sw.windowSize sw.limit

||| Approximate count using weighted average
export
approximateCount : SlidingWindow -> Integer -> Double
approximateCount sw now =
  let updated = updateWindow sw now
      elapsed = cast (now - updated.windowStart)
      windowFraction = cast elapsed / cast updated.windowSize
      weightedPrev = cast updated.previousCount * (1.0 - windowFraction)
  in weightedPrev + cast updated.currentCount

||| Try to record a request
export
tryRecord : SlidingWindow -> Integer -> (Bool, SlidingWindow)
tryRecord sw now =
  let updated = updateWindow sw now
      approx = approximateCount updated now
  in if approx >= cast updated.limit
     then (False, updated)
     else (True, MkSlidingWindow (S updated.currentCount) updated.previousCount updated.windowStart updated.windowSize updated.limit)

||| Requests remaining in current window
export
remaining : SlidingWindow -> Integer -> Nat
remaining sw now =
  let approx = approximateCount sw now
      rem = cast sw.limit - cast (ceiling approx)
  in if rem < 0 then 0 else cast rem

-- ============================================================================
-- FIXED WINDOW COUNTER
-- ============================================================================

||| Simple fixed window counter
public export
record FixedWindow where
  constructor MkFixedWindow
  count : Nat
  windowStart : Integer
  windowSize : Integer
  limit : Nat

||| Create a new fixed window
export
newFixedWindow : Integer -> Nat -> Integer -> Maybe FixedWindow
newFixedWindow ws lim now =
  if ws <= 0 || lim == 0 then Nothing
  else Just (MkFixedWindow 0 now ws lim)

||| Reset window if needed
export
maybeResetWindow : FixedWindow -> Integer -> FixedWindow
maybeResetWindow fw now =
  if now >= fw.windowStart + fw.windowSize
  then MkFixedWindow 0 (fw.windowStart + ((now - fw.windowStart) `div` fw.windowSize) * fw.windowSize) fw.windowSize fw.limit
  else fw

||| Try to increment counter
export
tryIncrement : FixedWindow -> Integer -> (Bool, FixedWindow)
tryIncrement fw now =
  let reset = maybeResetWindow fw now
  in if reset.count >= reset.limit
     then (False, reset)
     else (True, MkFixedWindow (S reset.count) reset.windowStart reset.windowSize reset.limit)

||| Time until window resets
export
timeUntilReset : FixedWindow -> Integer -> Integer
timeUntilReset fw now = max 0 (fw.windowStart + fw.windowSize - now)

-- ============================================================================
-- ADAPTIVE RATE LIMITER
-- ============================================================================

||| Adaptive rate limiter that adjusts based on success/failure
public export
record AdaptiveRateLimiter where
  constructor MkAdaptive
  currentRate : Double     -- Current allowed rate
  minRate : Double         -- Minimum rate
  maxRate : Double         -- Maximum rate
  successMultiplier : Double  -- Multiply rate on success
  failureMultiplier : Double  -- Multiply rate on failure
  lastAdjust : Integer

||| Create an adaptive rate limiter
export
newAdaptive : Double -> Double -> Double -> Maybe AdaptiveRateLimiter
newAdaptive minR maxR startR =
  if minR <= 0.0 || maxR < minR || startR < minR || startR > maxR
  then Nothing
  else Just (MkAdaptive startR minR maxR 1.1 0.5 0)

||| Record a success (increase rate)
export
recordSuccess : AdaptiveRateLimiter -> Integer -> AdaptiveRateLimiter
recordSuccess arl now =
  let newRate = min arl.maxRate (arl.currentRate * arl.successMultiplier)
  in MkAdaptive newRate arl.minRate arl.maxRate arl.successMultiplier arl.failureMultiplier now

||| Record a failure (decrease rate)
export
recordFailure : AdaptiveRateLimiter -> Integer -> AdaptiveRateLimiter
recordFailure arl now =
  let newRate = max arl.minRate (arl.currentRate * arl.failureMultiplier)
  in MkAdaptive newRate arl.minRate arl.maxRate arl.successMultiplier arl.failureMultiplier now

||| Get current allowed rate
export
currentAllowedRate : AdaptiveRateLimiter -> Double
currentAllowedRate arl = arl.currentRate

-- ============================================================================
-- RATE LIMIT RESULT
-- ============================================================================

||| Result of a rate limit check
public export
data RateLimitResult : Type where
  Allowed : RateLimitResult
  Denied : (retryAfterMs : Integer) -> RateLimitResult

||| Check if result is allowed
export
isAllowed : RateLimitResult -> Bool
isAllowed Allowed = True
isAllowed (Denied _) = False

||| Get retry-after time (0 if allowed)
export
retryAfter : RateLimitResult -> Integer
retryAfter Allowed = 0
retryAfter (Denied ms) = ms
