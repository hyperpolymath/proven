-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeRateLimiter - Safe rate limiting algorithms
|||
||| This module provides safe rate limiting implementations
||| including token bucket and sliding window algorithms.
module Proven.SafeRateLimiter
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Rate Limiter Types
--------------------------------------------------------------------------------

||| Token bucket state
public export
record TokenBucket where
  constructor MkTokenBucket
  capacity : Nat         -- Maximum tokens
  tokens : Nat           -- Current tokens
  refillRate : Nat       -- Tokens added per interval
  lastRefillTime : Nat   -- Timestamp of last refill

||| Sliding window state
public export
record SlidingWindow where
  constructor MkSlidingWindow
  maxRequests : Nat      -- Maximum requests per window
  windowSize : Nat       -- Window size in time units
  requests : List Nat    -- Timestamps of recent requests

||| Rate limit result
public export
data RateLimitResult : Type where
  Allowed : RateLimitResult
  Denied : (retryAfter : Nat) -> RateLimitResult

public export
Eq RateLimitResult where
  Allowed == Allowed = True
  (Denied a) == (Denied b) = a == b
  _ == _ = False

--------------------------------------------------------------------------------
-- Token Bucket Algorithm
--------------------------------------------------------------------------------

||| Create a new token bucket
public export
newTokenBucket : (capacity : Nat) -> (refillRate : Nat) -> TokenBucket
newTokenBucket cap rate = MkTokenBucket cap cap rate 0

||| Refill tokens based on elapsed time
public export
refillTokens : (currentTime : Nat) -> TokenBucket -> TokenBucket
refillTokens now bucket =
  let elapsed = minus now bucket.lastRefillTime
      newTokens = bucket.refillRate * elapsed
      totalTokens = min bucket.capacity (bucket.tokens + newTokens)
  in MkTokenBucket bucket.capacity totalTokens bucket.refillRate now

||| Try to acquire tokens from the bucket
public export
tryAcquireTokens : (count : Nat) -> (currentTime : Nat) -> TokenBucket -> (RateLimitResult, TokenBucket)
tryAcquireTokens count now bucket =
  let refilled = refillTokens now bucket
  in if refilled.tokens >= count
       then (Allowed, MkTokenBucket refilled.capacity (minus refilled.tokens count) refilled.refillRate now)
       else let needed = minus count refilled.tokens
                waitTime = (needed + refilled.refillRate - 1) `div` refilled.refillRate
            in (Denied waitTime, refilled)

||| Check if request would be allowed (without consuming tokens)
public export
wouldAllow : (count : Nat) -> (currentTime : Nat) -> TokenBucket -> Bool
wouldAllow count now bucket =
  let refilled = refillTokens now bucket
  in refilled.tokens >= count

||| Get current token count
public export
currentTokens : (currentTime : Nat) -> TokenBucket -> Nat
currentTokens now bucket = (refillTokens now bucket).tokens

--------------------------------------------------------------------------------
-- Sliding Window Algorithm
--------------------------------------------------------------------------------

||| Create a new sliding window limiter
public export
newSlidingWindow : (maxRequests : Nat) -> (windowSize : Nat) -> SlidingWindow
newSlidingWindow maxReq winSize = MkSlidingWindow maxReq winSize []

||| Remove expired requests from the window
pruneWindow : (currentTime : Nat) -> SlidingWindow -> SlidingWindow
pruneWindow now window =
  let cutoff = minus now window.windowSize
      validRequests = filter (>= cutoff) window.requests
  in MkSlidingWindow window.maxRequests window.windowSize validRequests

||| Try to record a request
public export
tryRequest : (currentTime : Nat) -> SlidingWindow -> (RateLimitResult, SlidingWindow)
tryRequest now window =
  let pruned = pruneWindow now window
      currentCount = length pruned.requests
  in if currentCount < pruned.maxRequests
       then (Allowed, MkSlidingWindow pruned.maxRequests pruned.windowSize (now :: pruned.requests))
       else
         let oldest = foldl min now pruned.requests
             retryAfter = pruned.windowSize - (minus now oldest)
         in (Denied retryAfter, pruned)

||| Get current request count in window
public export
currentCount : (currentTime : Nat) -> SlidingWindow -> Nat
currentCount now window = length (pruneWindow now window).requests

||| Get remaining allowed requests
public export
remainingRequests : (currentTime : Nat) -> SlidingWindow -> Nat
remainingRequests now window =
  let pruned = pruneWindow now window
  in minus pruned.maxRequests (length pruned.requests)

--------------------------------------------------------------------------------
-- Fixed Window Counter
--------------------------------------------------------------------------------

||| Fixed window counter state
public export
record FixedWindow where
  constructor MkFixedWindow
  maxRequests : Nat
  windowSize : Nat
  windowStart : Nat
  count : Nat

||| Create a fixed window counter
public export
newFixedWindow : (maxRequests : Nat) -> (windowSize : Nat) -> FixedWindow
newFixedWindow maxReq winSize = MkFixedWindow maxReq winSize 0 0

||| Try request with fixed window
public export
tryRequestFixed : (currentTime : Nat) -> FixedWindow -> (RateLimitResult, FixedWindow)
tryRequestFixed now window =
  let windowEnd = window.windowStart + window.windowSize
      -- Check if we're in a new window
      (start, cnt) = if now >= windowEnd
                       then ((now `div` window.windowSize) * window.windowSize, 0)
                       else (window.windowStart, window.count)
  in if cnt < window.maxRequests
       then (Allowed, MkFixedWindow window.maxRequests window.windowSize start (S cnt))
       else (Denied (minus (start + window.windowSize) now), MkFixedWindow window.maxRequests window.windowSize start cnt)

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

public export
Show TokenBucket where
  show bucket = "TokenBucket(tokens=" ++ show bucket.tokens ++
                "/" ++ show bucket.capacity ++ ")"

public export
Show SlidingWindow where
  show window = "SlidingWindow(requests=" ++ show (length window.requests) ++
                "/" ++ show window.maxRequests ++ ")"
