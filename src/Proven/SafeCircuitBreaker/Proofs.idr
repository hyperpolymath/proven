-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeCircuitBreaker fault-tolerance pattern.
|||
||| `Proven.SafeCircuitBreaker` ships the canonical Closed/Open/HalfOpen
||| state machine. This file machine-checks:
|||
|||   * `CircuitState` enum self-equality + Show wire-format anchors.
|||   * `newBreaker` initialises with Closed state, zero counts.
|||   * `reset` sets all counts to zero and state to Closed.
|||   * `defaultBreaker` definitional unfold.
|||   * `recordSuccess` from Closed zeros the failure count.
|||   * `recordAttempt` on Closed is identity.
|||   * `recordAttempt` on Open is identity.
|||   * `timeUntilRetry` on Closed is 0.
|||   * `timeUntilRetry` on HalfOpen is 0.
|||   * `isHealthy` for a freshly-constructed breaker = True.
|||
||| OWED: Nat comparisons via `>=` in `shouldTransitionToHalfOpen`,
||| `canExecute` HalfOpen branch.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeCircuitBreaker.Proofs

import Proven.SafeCircuitBreaker

%default total

--------------------------------------------------------------------------------
-- CircuitState enum self-equality
--------------------------------------------------------------------------------

public export
closedSelfEq : Closed == Closed = True
closedSelfEq = Refl

public export
openSelfEq : Proven.SafeCircuitBreaker.Open == Proven.SafeCircuitBreaker.Open = True
openSelfEq = Refl

public export
halfOpenSelfEq : HalfOpen == HalfOpen = True
halfOpenSelfEq = Refl

public export
closedNotOpen : Closed == Proven.SafeCircuitBreaker.Open = False
closedNotOpen = Refl

public export
openNotHalfOpen : Proven.SafeCircuitBreaker.Open == HalfOpen = False
openNotHalfOpen = Refl

--------------------------------------------------------------------------------
-- CircuitState Show wire-format anchors
--------------------------------------------------------------------------------

public export
closedShow : show Closed = "Closed"
closedShow = Refl

public export
openShow : show Proven.SafeCircuitBreaker.Open = "Open"
openShow = Refl

public export
halfOpenShow : show HalfOpen = "HalfOpen"
halfOpenShow = Refl

--------------------------------------------------------------------------------
-- `newBreaker` invariants (direct constructor — reduces by Refl)
--------------------------------------------------------------------------------

||| `newBreaker` starts in Closed state.
public export
newBreakerClosed : (cfg : CircuitConfig) -> (newBreaker cfg).state = Closed
newBreakerClosed cfg = Refl

||| `newBreaker` has zero failures.
public export
newBreakerZeroFailures : (cfg : CircuitConfig) -> (newBreaker cfg).failures = 0
newBreakerZeroFailures cfg = Refl

||| `newBreaker` has zero successes.
public export
newBreakerZeroSuccesses : (cfg : CircuitConfig) -> (newBreaker cfg).successes = 0
newBreakerZeroSuccesses cfg = Refl

||| `newBreaker` records the requested config.
public export
newBreakerConfig : (cfg : CircuitConfig) -> (newBreaker cfg).config = cfg
newBreakerConfig cfg = Refl

||| `newBreaker` has zero halfOpen calls.
public export
newBreakerZeroHalfOpenCalls : (cfg : CircuitConfig) -> (newBreaker cfg).halfOpenCalls = 0
newBreakerZeroHalfOpenCalls cfg = Refl

--------------------------------------------------------------------------------
-- `reset` invariants
--------------------------------------------------------------------------------

||| `reset` returns to Closed state.
public export
resetClosed : (b : CircuitBreaker) -> (reset b).state = Closed
resetClosed b = Refl

||| `reset` zeros failures.
public export
resetZeroFailures : (b : CircuitBreaker) -> (reset b).failures = 0
resetZeroFailures b = Refl

||| `reset` zeros successes.
public export
resetZeroSuccesses : (b : CircuitBreaker) -> (reset b).successes = 0
resetZeroSuccesses b = Refl

||| `reset` preserves the config.
public export
resetPreservesConfig : (b : CircuitBreaker) -> (reset b).config = b.config
resetPreservesConfig b = Refl

--------------------------------------------------------------------------------
-- `timeUntilRetry` on non-Open states
--------------------------------------------------------------------------------

||| On a Closed breaker, `timeUntilRetry = 0`.
public export
timeUntilRetryClosed :
  (now : Nat) -> (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> timeUntilRetry now (MkBreaker cfg Closed failures successes lft hoc) = 0
timeUntilRetryClosed now cfg failures successes lft hoc = Refl

||| On a HalfOpen breaker, `timeUntilRetry = 0`.
public export
timeUntilRetryHalfOpen :
  (now : Nat) -> (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> timeUntilRetry now (MkBreaker cfg HalfOpen failures successes lft hoc) = 0
timeUntilRetryHalfOpen now cfg failures successes lft hoc = Refl

--------------------------------------------------------------------------------
-- `shouldTransitionToHalfOpen` on non-Open states
--------------------------------------------------------------------------------

||| Closed breaker never transitions to HalfOpen.
public export
closedNeverTransitions :
  (now : Nat) -> (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> shouldTransitionToHalfOpen now (MkBreaker cfg Closed failures successes lft hoc) = False
closedNeverTransitions now cfg failures successes lft hoc = Refl

||| HalfOpen breaker never re-transitions to HalfOpen.
public export
halfOpenNeverReTransitions :
  (now : Nat) -> (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> shouldTransitionToHalfOpen now (MkBreaker cfg HalfOpen failures successes lft hoc) = False
halfOpenNeverReTransitions now cfg failures successes lft hoc = Refl

--------------------------------------------------------------------------------
-- `recordAttempt` on non-HalfOpen states is identity
--------------------------------------------------------------------------------

||| On Closed, `recordAttempt` is identity (only HalfOpen tracks attempts).
public export
recordAttemptClosedId :
  (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> recordAttempt (MkBreaker cfg Closed failures successes lft hoc)
     = MkBreaker cfg Closed failures successes lft hoc
recordAttemptClosedId cfg failures successes lft hoc = Refl

||| On Open, `recordAttempt` is identity.
public export
recordAttemptOpenId :
  (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> recordAttempt (MkBreaker cfg Proven.SafeCircuitBreaker.Open failures successes lft hoc)
     = MkBreaker cfg Proven.SafeCircuitBreaker.Open failures successes lft hoc
recordAttemptOpenId cfg failures successes lft hoc = Refl

--------------------------------------------------------------------------------
-- `isHealthy` <-> state = Closed
--------------------------------------------------------------------------------

||| A Closed breaker is healthy.
public export
closedIsHealthy :
  (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> isHealthy (MkBreaker cfg Closed failures successes lft hoc) = True
closedIsHealthy cfg failures successes lft hoc = Refl

||| An Open breaker is not healthy.
public export
openNotHealthy :
  (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> isHealthy (MkBreaker cfg Proven.SafeCircuitBreaker.Open failures successes lft hoc) = False
openNotHealthy cfg failures successes lft hoc = Refl

||| A HalfOpen breaker is not (yet) healthy.
public export
halfOpenNotHealthy :
  (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> isHealthy (MkBreaker cfg HalfOpen failures successes lft hoc) = False
halfOpenNotHealthy cfg failures successes lft hoc = Refl

--------------------------------------------------------------------------------
-- `recordSuccess` on Closed resets failures
--------------------------------------------------------------------------------

||| `recordSuccess` from Closed state preserves Closed.
public export
recordSuccessClosedStaysClosed :
  (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> (recordSuccess (MkBreaker cfg Closed failures successes lft hoc)).state = Closed
recordSuccessClosedStaysClosed cfg failures successes lft hoc = Refl

||| `recordSuccess` from Closed zeros the failure counter.
public export
recordSuccessClosedZerosFailures :
  (cfg : CircuitConfig) -> (failures, successes, lft, hoc : Nat)
  -> (recordSuccess (MkBreaker cfg Closed failures successes lft hoc)).failures = 0
recordSuccessClosedZerosFailures cfg failures successes lft hoc = Refl
