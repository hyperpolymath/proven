-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeSemaphore - Safe counting semaphore implementation
|||
||| This module provides a counting semaphore with bounded permits
||| and safe acquire/release operations.
module Proven.SafeSemaphore

import public Proven.Core
import Data.List

%default total

--------------------------------------------------------------------------------
-- Semaphore Type
--------------------------------------------------------------------------------

||| A counting semaphore with a maximum number of permits
public export
record Semaphore where
  constructor MkSemaphore
  maxPermits : Nat     -- Maximum number of permits
  available : Nat      -- Currently available permits
  waiters : Nat        -- Number of waiters (for diagnostics)

||| Semaphore errors
public export
data SemaphoreError
  = NoPermitsAvailable
  | InvalidPermitCount
  | TooManyReleases
  | WouldExceedMax

public export
Show SemaphoreError where
  show NoPermitsAvailable = "No permits available"
  show InvalidPermitCount = "Invalid permit count"
  show TooManyReleases = "Released more permits than acquired"
  show WouldExceedMax = "Would exceed maximum permits"

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create a semaphore with given capacity and initial permits
public export
create : (max : Nat) -> (initial : Nat) -> Maybe Semaphore
create max initial =
  if initial <= max
    then Just (MkSemaphore max initial 0)
    else Nothing

||| Create a semaphore with all permits available
public export
createFull : (max : Nat) -> Semaphore
createFull max = MkSemaphore max max 0

||| Create a semaphore with no permits available
public export
createEmpty : (max : Nat) -> Semaphore
createEmpty max = MkSemaphore max 0 0

||| Create a binary semaphore (mutex-like)
public export
binary : Bool -> Semaphore
binary locked = MkSemaphore 1 (if locked then 0 else 1) 0

--------------------------------------------------------------------------------
-- Acquire Operations
--------------------------------------------------------------------------------

||| Try to acquire one permit (non-blocking)
public export
tryAcquire : Semaphore -> Maybe Semaphore
tryAcquire sem =
  if sem.available > 0
    then Just (MkSemaphore sem.maxPermits (minus sem.available 1) sem.waiters)
    else Nothing

||| Try to acquire n permits (non-blocking)
public export
tryAcquireN : Nat -> Semaphore -> Maybe Semaphore
tryAcquireN Z sem = Just sem
tryAcquireN n sem =
  if sem.available >= n
    then Just (MkSemaphore sem.maxPermits (minus sem.available n) sem.waiters)
    else Nothing

||| Acquire with result type (for more detailed error handling)
public export
acquire : Semaphore -> Either SemaphoreError Semaphore
acquire sem =
  case tryAcquire sem of
    Nothing => Left NoPermitsAvailable
    Just s => Right s

||| Acquire n permits with result type
public export
acquireN : Nat -> Semaphore -> Either SemaphoreError Semaphore
acquireN Z sem = Right sem
acquireN n sem =
  case tryAcquireN n sem of
    Nothing => Left NoPermitsAvailable
    Just s => Right s

||| Record that a waiter is waiting (for diagnostics)
public export
addWaiter : Semaphore -> Semaphore
addWaiter sem = MkSemaphore sem.maxPermits sem.available (S sem.waiters)

||| Remove a waiter (for diagnostics)
public export
removeWaiter : Semaphore -> Semaphore
removeWaiter sem = MkSemaphore sem.maxPermits sem.available (minus sem.waiters 1)

--------------------------------------------------------------------------------
-- Release Operations
--------------------------------------------------------------------------------

||| Release one permit
public export
release : Semaphore -> Either SemaphoreError Semaphore
release sem =
  if sem.available >= sem.maxPermits
    then Left WouldExceedMax
    else Right (MkSemaphore sem.maxPermits (S sem.available) sem.waiters)

||| Release n permits
public export
releaseN : Nat -> Semaphore -> Either SemaphoreError Semaphore
releaseN Z sem = Right sem
releaseN n sem =
  if sem.available + n > sem.maxPermits
    then Left WouldExceedMax
    else Right (MkSemaphore sem.maxPermits (sem.available + n) sem.waiters)

||| Force release (unsafe - allows exceeding max for recovery)
public export
forceRelease : Semaphore -> Semaphore
forceRelease sem =
  let newAvail = min (S sem.available) sem.maxPermits
  in MkSemaphore sem.maxPermits newAvail sem.waiters

--------------------------------------------------------------------------------
-- Drain and Fill
--------------------------------------------------------------------------------

||| Drain all available permits (returns count acquired)
public export
drain : Semaphore -> (Nat, Semaphore)
drain sem = (sem.available, MkSemaphore sem.maxPermits 0 sem.waiters)

||| Fill to maximum permits (returns count added)
public export
fill : Semaphore -> (Nat, Semaphore)
fill sem =
  let added = minus sem.maxPermits sem.available
  in (added, MkSemaphore sem.maxPermits sem.maxPermits sem.waiters)

||| Reduce permits (shrink the semaphore)
public export
reducePermits : Nat -> Semaphore -> Semaphore
reducePermits n sem =
  let newMax = minus sem.maxPermits n
      newAvail = min sem.available newMax
  in MkSemaphore newMax newAvail sem.waiters

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

||| Get number of available permits
public export
availablePermits : Semaphore -> Nat
availablePermits = available

||| Get maximum permits
public export
maxPermits : Semaphore -> Nat
maxPermits sem = sem.maxPermits

||| Get number of permits in use
public export
usedPermits : Semaphore -> Nat
usedPermits sem = minus sem.maxPermits sem.available

||| Get number of waiters
public export
queueLength : Semaphore -> Nat
queueLength = waiters

||| Check if permits are available
public export
hasAvailable : Semaphore -> Bool
hasAvailable sem = sem.available > 0

||| Check if semaphore is full (all permits available)
public export
isFull : Semaphore -> Bool
isFull sem = sem.available == sem.maxPermits

||| Check if semaphore is empty (no permits available)
public export
isEmpty : Semaphore -> Bool
isEmpty sem = sem.available == 0

||| Check if there are waiters
public export
hasWaiters : Semaphore -> Bool
hasWaiters sem = sem.waiters > 0

--------------------------------------------------------------------------------
-- Utilities for Common Patterns
--------------------------------------------------------------------------------

||| Use a permit within a computation (bracket pattern simulation)
||| Returns (result, updated semaphore) or error
public export
withPermit : Semaphore -> (Semaphore -> (a, Semaphore)) -> Either SemaphoreError (a, Semaphore)
withPermit sem f =
  case acquire sem of
    Left err => Left err
    Right acquired =>
      let (result, used) = f acquired
      in case release used of
           Left _ => Right (result, used)  -- Best effort release
           Right released => Right (result, released)

||| Use n permits within a computation
public export
withPermits : Nat -> Semaphore -> (Semaphore -> (a, Semaphore)) -> Either SemaphoreError (a, Semaphore)
withPermits n sem f =
  case acquireN n sem of
    Left err => Left err
    Right acquired =>
      let (result, used) = f acquired
      in case releaseN n used of
           Left _ => Right (result, used)
           Right released => Right (result, released)

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Eq Semaphore where
  (==) (MkSemaphore m1 a1 w1) (MkSemaphore m2 a2 w2) =
    m1 == m2 && a1 == a2 && w1 == w2

public export
Show Semaphore where
  show sem = "Semaphore(" ++ show sem.available ++ "/" ++
             show sem.maxPermits ++ ", waiters=" ++ show sem.waiters ++ ")"
