-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeResource - Safe resource acquisition and release
|||
||| This module provides patterns for safe resource management,
||| ensuring resources are properly acquired and released.
module Proven.SafeResource
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Resource Handle
--------------------------------------------------------------------------------

||| Resource state
public export
data ResourceState : Type where
  Unacquired : ResourceState
  Acquired : ResourceState
  Released : ResourceState

public export
Eq ResourceState where
  Unacquired == Unacquired = True
  Acquired == Acquired = True
  Released == Released = True
  _ == _ = False

public export
Show ResourceState where
  show Unacquired = "Unacquired"
  show Acquired = "Acquired"
  show Released = "Released"

||| A tracked resource handle
public export
record ResourceHandle (id : Type) where
  constructor MkHandle
  resourceId : id
  state : ResourceState
  acquiredAt : Maybe Nat    -- Timestamp when acquired
  releasedAt : Maybe Nat    -- Timestamp when released

||| Create an unacquired resource handle
public export
newHandle : id -> ResourceHandle id
newHandle rid = MkHandle rid Unacquired Nothing Nothing

||| Mark resource as acquired
public export
markAcquired : (timestamp : Nat) -> ResourceHandle id -> ResourceHandle id
markAcquired ts h = MkHandle h.resourceId Acquired (Just ts) Nothing

||| Mark resource as released
public export
markReleased : (timestamp : Nat) -> ResourceHandle id -> ResourceHandle id
markReleased ts h = MkHandle h.resourceId Released h.acquiredAt (Just ts)

||| Check if resource is currently held
public export
isHeld : ResourceHandle id -> Bool
isHeld h = h.state == Acquired

--------------------------------------------------------------------------------
-- Resource Pool
--------------------------------------------------------------------------------

||| A pool of managed resources
public export
record ResourcePool id where
  constructor MkPool
  available : List id
  inUse : List id
  maxSize : Nat

||| Create a new resource pool
public export
newPool : (maxSize : Nat) -> (resources : List id) -> ResourcePool id
newPool maxSz res = MkPool (take maxSz res) [] maxSz

||| Acquire a resource from the pool
public export
acquire : ResourcePool id -> Maybe (id, ResourcePool id)
acquire pool =
  case pool.available of
    [] => Nothing
    (r :: rest) => Just (r, MkPool rest (r :: pool.inUse) pool.maxSize)

||| Release a resource back to the pool
public export
release : Eq id => id -> ResourcePool id -> ResourcePool id
release rid pool =
  if any (== rid) pool.inUse
    then MkPool (rid :: pool.available) 
                (filter (/= rid) pool.inUse)
                pool.maxSize
    else pool

||| Get number of available resources
public export
availableCount : ResourcePool id -> Nat
availableCount pool = length pool.available

||| Get number of resources in use
public export
inUseCount : ResourcePool id -> Nat
inUseCount pool = length pool.inUse

||| Check if pool is exhausted
public export
isExhausted : ResourcePool id -> Bool
isExhausted pool = isNil pool.available

--------------------------------------------------------------------------------
-- Lease-based Resources
--------------------------------------------------------------------------------

||| A resource lease with expiration
public export
record Lease id where
  constructor MkLease
  resourceId : id
  acquiredAt : Nat
  expiresAt : Nat
  renewable : Bool

||| Create a new lease
public export
newLease : id -> (acquiredAt : Nat) -> (duration : Nat) -> (renewable : Bool) -> Lease id
newLease rid acq dur ren = MkLease rid acq (acq + dur) ren

||| Check if lease is expired
public export
isExpired : (currentTime : Nat) -> Lease id -> Bool
isExpired now lease = now >= lease.expiresAt

||| Check if lease is valid
public export
isValid : (currentTime : Nat) -> Lease id -> Bool
isValid now lease = now < lease.expiresAt

||| Renew a lease (if renewable)
public export
renew : (currentTime : Nat) -> (duration : Nat) -> Lease id -> Maybe (Lease id)
renew now dur lease =
  if not lease.renewable
    then Nothing
    else if isExpired now lease
      then Nothing
      else Just (MkLease lease.resourceId now (now + dur) lease.renewable)

||| Get remaining time on lease
public export
remainingTime : (currentTime : Nat) -> Lease id -> Nat
remainingTime now lease =
  if now >= lease.expiresAt then 0
  else minus lease.expiresAt now

--------------------------------------------------------------------------------
-- Reference Counting
--------------------------------------------------------------------------------

||| Reference-counted resource
public export
record RefCounted a where
  constructor MkRefCounted
  value : a
  refCount : Nat

||| Create a new reference-counted value
public export
newRef : a -> RefCounted a
newRef x = MkRefCounted x 1

||| Increment reference count
public export
addRef : RefCounted a -> RefCounted a
addRef rc = MkRefCounted rc.value (S rc.refCount)

||| Decrement reference count (returns Nothing if count reaches 0)
public export
releaseRef : RefCounted a -> Maybe (RefCounted a)
releaseRef rc =
  case rc.refCount of
    Z => Nothing
    S Z => Nothing  -- Last reference, should be freed
    S n => Just (MkRefCounted rc.value n)

||| Check if this is the last reference
public export
isLastRef : RefCounted a -> Bool
isLastRef rc = rc.refCount == 1

||| Get current reference count
public export
getRefCount : RefCounted a -> Nat
getRefCount = refCount

||| Get the value (always safe)
public export
getValue : RefCounted a -> a
getValue = value

--------------------------------------------------------------------------------
-- Resource Lifecycle Events
--------------------------------------------------------------------------------

||| Resource lifecycle event
public export
data LifecycleEvent : Type where
  EvtCreated : (timestamp : Nat) -> LifecycleEvent
  EvtAcquired : (timestamp : Nat) -> (by : String) -> LifecycleEvent
  EvtReleased : (timestamp : Nat) -> (by : String) -> LifecycleEvent
  EvtDestroyed : (timestamp : Nat) -> LifecycleEvent

||| Resource with lifecycle tracking
public export
record TrackedResource id a where
  constructor MkTracked
  resourceId : id
  value : a
  events : List LifecycleEvent

||| Create a tracked resource
public export
createTracked : id -> a -> (timestamp : Nat) -> TrackedResource id a
createTracked rid val ts = MkTracked rid val [EvtCreated ts]

||| Record acquisition
public export
recordAcquisition : (timestamp : Nat) -> (owner : String) -> TrackedResource id a -> TrackedResource id a
recordAcquisition ts owner tr = MkTracked tr.resourceId tr.value (EvtAcquired ts owner :: tr.events)

||| Record release
public export
recordRelease : (timestamp : Nat) -> (owner : String) -> TrackedResource id a -> TrackedResource id a
recordRelease ts owner tr = MkTracked tr.resourceId tr.value (EvtReleased ts owner :: tr.events)

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show id => Show (ResourceHandle id) where
  show h = "Handle(" ++ show h.resourceId ++ ", " ++ show h.state ++ ")"

public export
Show id => Show (Lease id) where
  show l = "Lease(" ++ show l.resourceId ++ ", expires=" ++ show l.expiresAt ++ ")"

