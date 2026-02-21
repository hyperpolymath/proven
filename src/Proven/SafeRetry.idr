-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeRetry - Safe retry strategies and backoff algorithms
|||
||| This module provides configurable retry logic with various
||| backoff strategies for resilient error handling.
module Proven.SafeRetry
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Backoff Strategies
--------------------------------------------------------------------------------

||| Backoff strategy for retries
public export
data BackoffStrategy : Type where
  ||| Fixed delay between retries
  FixedBackoff : (delay : Nat) -> BackoffStrategy
  ||| Linearly increasing delay
  LinearBackoff : (initial : Nat) -> (increment : Nat) -> BackoffStrategy
  ||| Exponentially increasing delay
  ExponentialBackoff : (initial : Nat) -> (multiplier : Nat) -> BackoffStrategy
  ||| Exponential with jitter (randomness bounds)
  JitteredBackoff : (initial : Nat) -> (multiplier : Nat) -> (maxJitter : Nat) -> BackoffStrategy

public export
Show BackoffStrategy where
  show (FixedBackoff d) = "Fixed(" ++ show d ++ ")"
  show (LinearBackoff i inc) = "Linear(" ++ show i ++ ", +" ++ show inc ++ ")"
  show (ExponentialBackoff i m) = "Exponential(" ++ show i ++ ", *" ++ show m ++ ")"
  show (JitteredBackoff i m j) = "Jittered(" ++ show i ++ ", *" ++ show m ++ ", ±" ++ show j ++ ")"

--------------------------------------------------------------------------------
-- Retry Configuration
--------------------------------------------------------------------------------

||| Retry configuration
public export
record RetryConfig where
  constructor MkRetryConfig
  maxAttempts : Nat          -- Maximum number of attempts (including first)
  backoff : BackoffStrategy  -- Backoff strategy
  maxDelay : Nat             -- Cap on delay between retries
  retryOn : Nat -> Bool      -- Error codes to retry on (True = retry)

||| Default retry configuration
public export
defaultRetryConfig : RetryConfig
defaultRetryConfig = MkRetryConfig 3 (ExponentialBackoff 100 2) 30000 (const True)

||| Create a retry config with fixed delay
public export
fixedRetry : (attempts : Nat) -> (delay : Nat) -> RetryConfig
fixedRetry n d = MkRetryConfig n (FixedBackoff d) d (const True)

||| Create a retry config with exponential backoff
public export
exponentialRetry : (attempts : Nat) -> (initialDelay : Nat) -> RetryConfig
exponentialRetry n d = MkRetryConfig n (ExponentialBackoff d 2) (d * 64) (const True)

--------------------------------------------------------------------------------
-- Delay Calculation
--------------------------------------------------------------------------------

||| Calculate delay for a given attempt number
public export
calculateDelay : BackoffStrategy -> (attempt : Nat) -> Nat
calculateDelay (FixedBackoff d) _ = d
calculateDelay (LinearBackoff initial inc) attempt = initial + (inc * attempt)
calculateDelay (ExponentialBackoff initial mult) attempt = initial * (power mult attempt)
  where
    power : Nat -> Nat -> Nat
    power _ Z = 1
    power b (S n) = b * power b n
calculateDelay (JitteredBackoff initial mult _) attempt =
  -- Without actual randomness, we just use the base exponential
  initial * (power mult attempt)
  where
    power : Nat -> Nat -> Nat
    power _ Z = 1
    power b (S n) = b * power b n

||| Calculate delay with cap
public export
calculateDelayWithCap : RetryConfig -> (attempt : Nat) -> Nat
calculateDelayWithCap config attempt =
  min config.maxDelay (calculateDelay config.backoff attempt)

--------------------------------------------------------------------------------
-- Retry State
--------------------------------------------------------------------------------

||| State of retry operation
public export
record RetryState where
  constructor MkRetryState
  attempt : Nat              -- Current attempt number (0-indexed)
  totalDelay : Nat           -- Cumulative delay so far
  lastError : Maybe Nat      -- Last error code (if any)

||| Initial retry state
public export
initialState : RetryState
initialState = MkRetryState 0 0 Nothing

||| Check if more retries are allowed
public export
canRetry : RetryConfig -> RetryState -> Bool
canRetry config state = S state.attempt < config.maxAttempts

||| Check if should retry based on error
public export
shouldRetry : RetryConfig -> RetryState -> (errorCode : Nat) -> Bool
shouldRetry config state err =
  canRetry config state && config.retryOn err

||| Advance to next retry attempt
public export
nextAttempt : RetryConfig -> RetryState -> (errorCode : Nat) -> RetryState
nextAttempt config state err =
  let delay = calculateDelayWithCap config state.attempt
  in MkRetryState (S state.attempt) (state.totalDelay + delay) (Just err)

||| Get remaining attempts
public export
remainingAttempts : RetryConfig -> RetryState -> Nat
remainingAttempts config state = minus config.maxAttempts (S state.attempt)

--------------------------------------------------------------------------------
-- Retry Result
--------------------------------------------------------------------------------

||| Result of retry operation
public export
data RetryResult a : Type where
  ||| Operation succeeded
  Success : a -> RetryResult a
  ||| All retries exhausted
  Exhausted : (attempts : Nat) -> (lastError : Maybe Nat) -> RetryResult a
  ||| Operation should not be retried
  NoRetry : (errorCode : Nat) -> RetryResult a

public export
Functor RetryResult where
  map f (Success x) = Success (f x)
  map _ (Exhausted n e) = Exhausted n e
  map _ (NoRetry e) = NoRetry e

--------------------------------------------------------------------------------
-- Retry Policies
--------------------------------------------------------------------------------

||| Retry only on specific error codes
public export
retryOnCodes : List Nat -> (Nat -> Bool)
retryOnCodes codes = \err => any (== err) codes

||| Retry on all errors except specific codes
public export
retryExceptCodes : List Nat -> (Nat -> Bool)
retryExceptCodes codes = \err => not (any (== err) codes)

||| Never retry
public export
noRetry : Nat -> Bool
noRetry = const False

||| Always retry
public export
alwaysRetry : Nat -> Bool
alwaysRetry = const True

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

||| Get retry statistics
public export
record RetryStats where
  constructor MkStats
  totalAttempts : Nat
  totalDelayMs : Nat
  succeeded : Bool

||| Build stats from state
public export
buildStats : RetryState -> (succeeded : Bool) -> RetryStats
buildStats state succ = MkStats (S state.attempt) state.totalDelay succ

public export
Show RetryState where
  show state = "RetryState(attempt=" ++ show state.attempt ++
               ", totalDelay=" ++ show state.totalDelay ++ "ms)"

