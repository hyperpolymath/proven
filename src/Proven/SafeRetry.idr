-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeRetry - Verified retry policies
|||
||| Type-safe retry mechanisms with exponential backoff, jitter,
||| and bounded attempts.
|||
||| Guarantees retry limits are respected and delays are bounded.
module Proven.SafeRetry

import Proven.Core
import Data.So

%default total

-- ============================================================================
-- RETRY POLICY
-- ============================================================================

||| Types of backoff strategies
public export
data BackoffStrategy : Type where
  ||| Constant delay between retries
  Constant : BackoffStrategy
  ||| Delay doubles each retry
  Exponential : BackoffStrategy
  ||| Linear increase in delay
  Linear : BackoffStrategy
  ||| Fibonacci sequence delays
  Fibonacci : BackoffStrategy

||| Jitter strategies to prevent thundering herd
public export
data JitterStrategy : Type where
  ||| No jitter (exact delays)
  NoJitter : JitterStrategy
  ||| Add random 0-25% to delay
  FullJitter : JitterStrategy
  ||| Add random ±12.5% to delay
  EqualJitter : JitterStrategy
  ||| Decorrelated jitter (AWS style)
  DecorrelatedJitter : JitterStrategy

-- ============================================================================
-- RETRY CONFIGURATION
-- ============================================================================

||| Configuration for retry behavior
public export
record RetryConfig where
  constructor MkRetryConfig
  maxAttempts : Nat          -- Maximum number of attempts (including first)
  baseDelay : Integer        -- Base delay in milliseconds
  maxDelay : Integer         -- Maximum delay cap
  backoff : BackoffStrategy
  jitter : JitterStrategy
  multiplier : Double        -- For exponential/linear backoff
  0 attemptsPositive : So (maxAttempts > 0)
  0 baseDelayNonNeg : So (baseDelay >= 0)
  0 maxDelayPositive : So (maxDelay > 0)

||| Create a retry configuration
export
mkRetryConfig : (maxAttempts : Nat)
             -> (baseDelay : Integer)
             -> (maxDelay : Integer)
             -> (backoff : BackoffStrategy)
             -> (jitter : JitterStrategy)
             -> (multiplier : Double)
             -> Maybe RetryConfig
mkRetryConfig attempts base maxD bo ji mult =
  if attempts == 0 || base < 0 || maxD <= 0 || mult <= 0.0
  then Nothing
  else Just (believe_me (MkRetryConfig attempts base maxD bo ji mult))

||| Default retry configuration (3 attempts, exponential backoff)
export
defaultRetryConfig : RetryConfig
defaultRetryConfig = believe_me (MkRetryConfig 3 1000 30000 Exponential EqualJitter 2.0)

||| Simple constant retry (fixed delay)
export
constantRetry : Nat -> Integer -> Maybe RetryConfig
constantRetry attempts delay =
  mkRetryConfig attempts delay delay Constant NoJitter 1.0

||| Exponential backoff with jitter
export
exponentialBackoff : Nat -> Integer -> Integer -> Maybe RetryConfig
exponentialBackoff attempts base maxD =
  mkRetryConfig attempts base maxD Exponential EqualJitter 2.0

-- ============================================================================
-- DELAY CALCULATION
-- ============================================================================

||| Calculate delay for a given attempt (0-indexed)
export
calculateDelay : RetryConfig -> Nat -> Integer
calculateDelay cfg attempt =
  let rawDelay = case cfg.backoff of
        Constant => cfg.baseDelay
        Exponential =>
          let factor = pow cfg.multiplier (cast attempt)
          in cast (cast cfg.baseDelay * factor)
        Linear =>
          cfg.baseDelay + cast attempt * cast (ceiling cfg.multiplier * 1000.0)
        Fibonacci =>
          cfg.baseDelay * cast (fib (S attempt))
  in min rawDelay cfg.maxDelay
  where
    fib : Nat -> Nat
    fib Z = 1
    fib (S Z) = 1
    fib (S (S n)) = fib (S n) + fib n

||| Apply jitter to a delay
||| Takes a "random" value between 0.0 and 1.0
export
applyJitter : RetryConfig -> Integer -> Double -> Integer
applyJitter cfg delay rand =
  case cfg.jitter of
    NoJitter => delay
    FullJitter =>
      -- Random value between 0 and delay
      cast (cast delay * rand)
    EqualJitter =>
      -- delay/2 + random value between 0 and delay/2
      let half = delay `div` 2
      in half + cast (cast half * rand)
    DecorrelatedJitter =>
      -- Random between baseDelay and delay * 3
      let minD = cfg.baseDelay
          maxD = delay * 3
          range = maxD - minD
      in minD + cast (cast range * rand)

||| Calculate delay with jitter for a given attempt
export
calculateDelayWithJitter : RetryConfig -> Nat -> Double -> Integer
calculateDelayWithJitter cfg attempt rand =
  let baseDelay = calculateDelay cfg attempt
  in min cfg.maxDelay (applyJitter cfg baseDelay rand)

-- ============================================================================
-- RETRY STATE
-- ============================================================================

||| Current state of a retry operation
public export
record RetryState where
  constructor MkRetryState
  attempt : Nat              -- Current attempt (0-indexed)
  totalDelay : Integer       -- Total delay so far
  lastError : Maybe String   -- Last error message
  config : RetryConfig

||| Create initial retry state
export
initialRetryState : RetryConfig -> RetryState
initialRetryState cfg = MkRetryState 0 0 Nothing cfg

||| Check if more retries are allowed
export
canRetry : RetryState -> Bool
canRetry rs = rs.attempt < rs.config.maxAttempts

||| Get remaining attempts
export
remainingAttempts : RetryState -> Nat
remainingAttempts rs = rs.config.maxAttempts `minus` rs.attempt

||| Record a failure and advance to next attempt
export
recordRetryFailure : RetryState -> String -> Double -> (RetryState, Integer)
recordRetryFailure rs errMsg rand =
  let nextAttempt = S rs.attempt
      delay = calculateDelayWithJitter rs.config rs.attempt rand
      newTotal = rs.totalDelay + delay
  in (MkRetryState nextAttempt newTotal (Just errMsg) rs.config, delay)

-- ============================================================================
-- RETRY RESULT
-- ============================================================================

||| Result of a retry operation
public export
data RetryResult a : Type where
  ||| Operation succeeded
  RetrySuccess : (value : a) -> (attempts : Nat) -> (totalDelay : Integer) -> RetryResult a
  ||| All retries exhausted
  RetryExhausted : (lastError : String) -> (attempts : Nat) -> (totalDelay : Integer) -> RetryResult a

||| Check if retry was successful
export
isRetrySuccess : RetryResult a -> Bool
isRetrySuccess (RetrySuccess _ _ _) = True
isRetrySuccess _ = False

||| Get value or default
export
retryGetOrDefault : a -> RetryResult a -> a
retryGetOrDefault _ (RetrySuccess v _ _) = v
retryGetOrDefault def _ = def

||| Get number of attempts made
export
retryAttempts : RetryResult a -> Nat
retryAttempts (RetrySuccess _ n _) = n
retryAttempts (RetryExhausted _ n _) = n

||| Get total delay incurred
export
retryTotalDelay : RetryResult a -> Integer
retryTotalDelay (RetrySuccess _ _ d) = d
retryTotalDelay (RetryExhausted _ _ d) = d

-- ============================================================================
-- RETRY PREDICATES
-- ============================================================================

||| Predicate for determining if an error is retryable
public export
RetryPredicate : Type -> Type
RetryPredicate e = e -> Bool

||| Always retry (any error)
export
alwaysRetry : RetryPredicate e
alwaysRetry _ = True

||| Never retry
export
neverRetry : RetryPredicate e
neverRetry _ = False

||| Retry on specific errors
export
retryOn : Eq e => List e -> RetryPredicate e
retryOn retryable err = err `elem` retryable

||| Don't retry on specific errors
export
dontRetryOn : Eq e => List e -> RetryPredicate e
dontRetryOn nonRetryable err = not (err `elem` nonRetryable)

-- ============================================================================
-- DEADLINE-AWARE RETRY
-- ============================================================================

||| Retry state with deadline awareness
public export
record DeadlineRetryState where
  constructor MkDeadlineRetry
  baseState : RetryState
  deadline : Integer         -- Absolute deadline timestamp
  startTime : Integer        -- When retry started

||| Create deadline-aware retry state
export
withDeadline : RetryConfig -> Integer -> Integer -> DeadlineRetryState
withDeadline cfg now deadline =
  MkDeadlineRetry (initialRetryState cfg) deadline now

||| Check if deadline allows another retry
export
deadlineAllowsRetry : DeadlineRetryState -> Integer -> Bool
deadlineAllowsRetry drs now =
  canRetry drs.baseState && now < drs.deadline

||| Calculate delay respecting deadline
export
deadlineAwareDelay : DeadlineRetryState -> Integer -> Double -> Integer
deadlineAwareDelay drs now rand =
  let normalDelay = calculateDelayWithJitter drs.baseState.config drs.baseState.attempt rand
      remainingTime = drs.deadline - now
  in min normalDelay (max 0 remainingTime)

||| Record failure with deadline awareness
export
recordDeadlineFailure : DeadlineRetryState -> String -> Double -> (DeadlineRetryState, Integer)
recordDeadlineFailure drs errMsg rand =
  let (newBase, delay) = recordRetryFailure drs.baseState errMsg rand
  in (MkDeadlineRetry newBase drs.deadline drs.startTime, delay)

-- ============================================================================
-- RETRY BUDGET
-- ============================================================================

||| Retry budget for controlling retry rate across a system
public export
record RetryBudget where
  constructor MkRetryBudget
  tokensPerSecond : Double   -- Rate at which tokens are added
  maxTokens : Double         -- Maximum tokens (budget capacity)
  currentTokens : Double     -- Current available tokens
  lastUpdate : Integer       -- Last update timestamp

||| Create a retry budget
export
newRetryBudget : Double -> Double -> Integer -> Maybe RetryBudget
newRetryBudget rate maxT now =
  if rate <= 0.0 || maxT <= 0.0 then Nothing
  else Just (MkRetryBudget rate maxT maxT now)

||| Refill budget based on elapsed time
export
refillBudget : RetryBudget -> Integer -> RetryBudget
refillBudget rb now =
  let elapsed = cast (now - rb.lastUpdate) / 1000.0
      added = elapsed * rb.tokensPerSecond
      newTokens = min rb.maxTokens (rb.currentTokens + added)
  in { currentTokens := newTokens, lastUpdate := now } rb

||| Try to consume a token for retry
export
tryConsumeBudget : RetryBudget -> Integer -> Double -> (Bool, RetryBudget)
tryConsumeBudget rb now cost =
  let refilled = refillBudget rb now
  in if refilled.currentTokens >= cost
     then (True, { currentTokens $= (\t => t - cost) } refilled)
     else (False, refilled)

||| Check available budget
export
availableBudget : RetryBudget -> Integer -> Double
availableBudget rb now = (refillBudget rb now).currentTokens
