-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeResource operations
|||
||| This module exports resource lifecycle management helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total.
|||
||| Return conventions:
||| - State → Int (Unacquired=0, Acquired=1, Released=2)
||| - Event → Int (Created=0, Acquired=1, Released=2, Destroyed=3)
||| - Count → Int (available, in-use, ref count)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Time → Int (timestamps, durations)
|||
||| CRITICAL: Proper resource management prevents leaks and use-after-free bugs.
|||           Always ensure resources are released after use.
|||
||| Resource Patterns:
||| - Handle: Single resource with state tracking
||| - Pool: Reusable resource pool (connection pools, thread pools)
||| - Lease: Time-limited access with expiration
||| - RefCount: Reference counting for shared resources
|||
||| Resource State Machine:
||| - Unacquired → Acquired: acquire() succeeds
||| - Acquired → Released: release() called
||| - Released → Acquired: reacquire (if supported)
||| - Invalid transitions cause errors
|||
||| Pool Behavior:
||| - Bounded: maxSize limits total resources
||| - Acquire: Move from available to inUse
||| - Release: Move from inUse back to available
||| - Exhausted: No available resources (all inUse)
|||
||| Lease Behavior:
||| - Created with duration (acquiredAt + duration = expiresAt)
||| - Valid: currentTime < expiresAt
||| - Expired: currentTime ≥ expiresAt
||| - Renewable: Can extend expiry if renewable=true and not expired
|||
||| Reference Counting:
||| - Initial refCount = 1
||| - addRef: increment count
||| - releaseRef: decrement count, free when reaches 0
||| - isLastRef: refCount == 1
module Proven.FFI.SafeResource

import Proven.SafeResource
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
-- Resource State Encoding
--------------------------------------------------------------------------------

export
proven_idris_resource_state_unacquired : Int
proven_idris_resource_state_unacquired = 0

export
proven_idris_resource_state_acquired : Int
proven_idris_resource_state_acquired = 1

export
proven_idris_resource_state_released : Int
proven_idris_resource_state_released = 2

export
proven_idris_resource_is_valid_state : Int -> Int
proven_idris_resource_is_valid_state state =
  encodeBool (state >= 0 && state <= 2)

export
proven_idris_resource_can_acquire : Int -> Int
proven_idris_resource_can_acquire state =
  encodeBool (state == 0)  -- Unacquired

export
proven_idris_resource_can_release : Int -> Int
proven_idris_resource_can_release state =
  encodeBool (state == 1)  -- Acquired

export
proven_idris_resource_is_held : Int -> Int
proven_idris_resource_is_held state =
  encodeBool (state == 1)  -- Acquired

export
proven_idris_resource_is_released : Int -> Int
proven_idris_resource_is_released state =
  encodeBool (state == 2)  -- Released

--------------------------------------------------------------------------------
-- Resource Handle Operations
--------------------------------------------------------------------------------

export
proven_idris_resource_new_handle : Int
proven_idris_resource_new_handle = 0  -- Unacquired

export
proven_idris_resource_mark_acquired : Int -> Int -> (Int, Int)
proven_idris_resource_mark_acquired _ timestamp =
  (1, timestamp)  -- (Acquired, acquiredAt)

export
proven_idris_resource_mark_released : Int -> Int -> (Int, Int)
proven_idris_resource_mark_released acquiredAt timestamp =
  (2, timestamp)  -- (Released, releasedAt)

export
proven_idris_resource_hold_duration : Int -> Int -> Int
proven_idris_resource_hold_duration acquiredAt releasedAt =
  if releasedAt >= acquiredAt
    then releasedAt - acquiredAt
    else 0

--------------------------------------------------------------------------------
-- Resource Pool Operations
--------------------------------------------------------------------------------

export
proven_idris_pool_available_count : Int -> Int
proven_idris_pool_available_count count = count

export
proven_idris_pool_in_use_count : Int -> Int
proven_idris_pool_in_use_count count = count

export
proven_idris_pool_total_size : Int -> Int -> Int
proven_idris_pool_total_size available inUse = available + inUse

export
proven_idris_pool_max_size : Int -> Int
proven_idris_pool_max_size maxSize = maxSize

export
proven_idris_pool_is_exhausted : Int -> Int
proven_idris_pool_is_exhausted availableCount =
  encodeBool (availableCount == 0)

export
proven_idris_pool_has_available : Int -> Int
proven_idris_pool_has_available availableCount =
  encodeBool (availableCount > 0)

export
proven_idris_pool_is_full : Int -> Int -> Int
proven_idris_pool_is_full inUseCount maxSize =
  encodeBool (inUseCount >= maxSize)

export
proven_idris_pool_utilization : Int -> Int -> Double
proven_idris_pool_utilization inUseCount maxSize =
  if maxSize == 0 then 0.0
  else cast inUseCount / cast maxSize

export
proven_idris_pool_utilization_percent : Int -> Int -> Double
proven_idris_pool_utilization_percent inUseCount maxSize =
  proven_idris_pool_utilization inUseCount maxSize * 100.0

export
proven_idris_pool_remaining_capacity : Int -> Int
proven_idris_pool_remaining_capacity availableCount = availableCount

--------------------------------------------------------------------------------
-- Lease Operations
--------------------------------------------------------------------------------

export
proven_idris_lease_new : Int -> Int -> Int -> (Int, Int, Int)
proven_idris_lease_new acquiredAt duration renewable =
  (acquiredAt, acquiredAt + duration, renewable)  -- (acquiredAt, expiresAt, renewable)

export
proven_idris_lease_is_expired : Int -> Int -> Int
proven_idris_lease_is_expired currentTime expiresAt =
  encodeBool (currentTime >= expiresAt)

export
proven_idris_lease_is_valid : Int -> Int -> Int
proven_idris_lease_is_valid currentTime expiresAt =
  encodeBool (currentTime < expiresAt)

export
proven_idris_lease_remaining_time : Int -> Int -> Int
proven_idris_lease_remaining_time currentTime expiresAt =
  if currentTime >= expiresAt then 0
  else expiresAt - currentTime

export
proven_idris_lease_can_renew : Int -> Int -> Int -> Int
proven_idris_lease_can_renew renewable currentTime expiresAt =
  encodeBool (renewable == 1 && currentTime < expiresAt)

export
proven_idris_lease_renew : Int -> Int -> Int -> (Int, Int)
proven_idris_lease_renew currentTime duration renewable =
  if renewable == 1
    then (currentTime, currentTime + duration)  -- (acquiredAt, expiresAt)
    else (0, 0)  -- Renewal failed

export
proven_idris_lease_elapsed_time : Int -> Int -> Int
proven_idris_lease_elapsed_time acquiredAt currentTime =
  if currentTime >= acquiredAt
    then currentTime - acquiredAt
    else 0

export
proven_idris_lease_total_duration : Int -> Int -> Int
proven_idris_lease_total_duration acquiredAt expiresAt =
  if expiresAt >= acquiredAt
    then expiresAt - acquiredAt
    else 0

export
proven_idris_lease_percent_elapsed : Int -> Int -> Int -> Double
proven_idris_lease_percent_elapsed acquiredAt currentTime expiresAt =
  let duration = expiresAt - acquiredAt
      elapsed = if currentTime > acquiredAt
                  then currentTime - acquiredAt
                  else 0
  in if duration == 0 then 100.0
     else if elapsed >= duration then 100.0
     else cast elapsed / cast duration * 100.0

--------------------------------------------------------------------------------
-- Reference Counting
--------------------------------------------------------------------------------

export
proven_idris_refcount_new : Int
proven_idris_refcount_new = 1

export
proven_idris_refcount_add_ref : Int -> Int
proven_idris_refcount_add_ref current = current + 1

export
proven_idris_refcount_release_ref : Int -> Int
proven_idris_refcount_release_ref current =
  if current > 0 then current - 1 else 0

export
proven_idris_refcount_is_last_ref : Int -> Int
proven_idris_refcount_is_last_ref refCount =
  encodeBool (refCount == 1)

export
proven_idris_refcount_should_free : Int -> Int
proven_idris_refcount_should_free refCount =
  encodeBool (refCount == 0)

export
proven_idris_refcount_get : Int -> Int
proven_idris_refcount_get refCount = refCount

export
proven_idris_refcount_is_valid : Int -> Int
proven_idris_refcount_is_valid refCount =
  encodeBool (refCount > 0)

--------------------------------------------------------------------------------
-- Lifecycle Event Encoding
--------------------------------------------------------------------------------

export
proven_idris_lifecycle_evt_created : Int
proven_idris_lifecycle_evt_created = 0

export
proven_idris_lifecycle_evt_acquired : Int
proven_idris_lifecycle_evt_acquired = 1

export
proven_idris_lifecycle_evt_released : Int
proven_idris_lifecycle_evt_released = 2

export
proven_idris_lifecycle_evt_destroyed : Int
proven_idris_lifecycle_evt_destroyed = 3

export
proven_idris_lifecycle_is_valid_event : Int -> Int
proven_idris_lifecycle_is_valid_event event =
  encodeBool (event >= 0 && event <= 3)

export
proven_idris_lifecycle_event_count : Int -> Int
proven_idris_lifecycle_event_count count = count

--------------------------------------------------------------------------------
-- Resource Validation
--------------------------------------------------------------------------------

export
proven_idris_resource_is_valid_duration : Int -> Int
proven_idris_resource_is_valid_duration duration =
  encodeBool (duration > 0 && duration <= 86400)  -- Max 24 hours

export
proven_idris_resource_is_valid_timestamp : Int -> Int
proven_idris_resource_is_valid_timestamp timestamp =
  encodeBool (timestamp >= 0)

export
proven_idris_resource_is_valid_pool_size : Int -> Int
proven_idris_resource_is_valid_pool_size size =
  encodeBool (size > 0 && size <= 10000)

export
proven_idris_resource_is_valid_refcount : Int -> Int
proven_idris_resource_is_valid_refcount refCount =
  encodeBool (refCount >= 0 && refCount <= 1000000)

--------------------------------------------------------------------------------
-- Resource Statistics
--------------------------------------------------------------------------------

export
proven_idris_resource_average_hold_time : Int -> Int -> Double
proven_idris_resource_average_hold_time totalTime resourceCount =
  if resourceCount == 0 then 0.0
  else cast totalTime / cast resourceCount

export
proven_idris_resource_acquisition_rate : Int -> Int -> Double
proven_idris_resource_acquisition_rate acquisitions timeWindow =
  if timeWindow == 0 then 0.0
  else cast acquisitions / cast timeWindow

export
proven_idris_resource_release_rate : Int -> Int -> Double
proven_idris_resource_release_rate releases timeWindow =
  if timeWindow == 0 then 0.0
  else cast releases / cast timeWindow

export
proven_idris_resource_leak_count : Int -> Int -> Int
proven_idris_resource_leak_count acquired released =
  if acquired > released then acquired - released else 0

export
proven_idris_resource_leak_rate : Int -> Int -> Double
proven_idris_resource_leak_rate acquired released =
  if acquired == 0 then 0.0
  else cast (acquired - released) / cast acquired

--------------------------------------------------------------------------------
-- Pool Planning
--------------------------------------------------------------------------------

export
proven_idris_pool_recommend_size : Int -> Double -> Int
proven_idris_pool_recommend_size peakUsage targetUtilization =
  -- Recommended size = peak / target utilization
  if targetUtilization <= 0.0 || targetUtilization > 1.0
    then peakUsage
    else cast (cast peakUsage / targetUtilization)

export
proven_idris_pool_should_expand : Int -> Int -> Int -> Int
proven_idris_pool_should_expand inUse available maxSize =
  -- Should expand if utilization > 80% and not at max
  let total = inUse + available
      utilization = if total == 0 then 0.0
                     else cast inUse / cast total
  in encodeBool (utilization > 0.8 && total < maxSize)

export
proven_idris_pool_should_shrink : Int -> Int -> Int
proven_idris_pool_should_shrink inUse available =
  -- Should shrink if many resources idle
  let total = inUse + available
      utilization = if total == 0 then 0.0
                     else cast inUse / cast total
  in encodeBool (utilization < 0.2 && available > 5)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_resource_default_lease_duration : Int
proven_idris_resource_default_lease_duration = 3600  -- 1 hour

export
proven_idris_resource_default_pool_size : Int
proven_idris_resource_default_pool_size = 10

export
proven_idris_resource_max_lease_duration : Int
proven_idris_resource_max_lease_duration = 86400  -- 24 hours

export
proven_idris_resource_max_pool_size : Int
proven_idris_resource_max_pool_size = 10000

export
proven_idris_resource_target_utilization : Double
proven_idris_resource_target_utilization = 0.75  -- 75%

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_resource_friendly_error : String -> String
proven_idris_resource_friendly_error errorMsg =
  if isInfixOf "exhausted" (toLower errorMsg) || isInfixOf "pool" (toLower errorMsg)
    then "Resource pool exhausted (no available resources)"
  else if isInfixOf "expired" (toLower errorMsg) || isInfixOf "lease" (toLower errorMsg)
    then "Resource lease expired (access denied)"
  else if isInfixOf "leak" (toLower errorMsg)
    then "Resource leak detected (acquired but not released)"
  else if isInfixOf "double" (toLower errorMsg) && isInfixOf "release" (toLower errorMsg)
    then "Double release detected (already released)"
  else if isInfixOf "use after" (toLower errorMsg)
    then "Use after release detected (invalid access)"
  else if isInfixOf "refcount" (toLower errorMsg) || isInfixOf "reference" (toLower errorMsg)
    then "Reference count error (invalid count)"
  else
    "Resource management error"

export
proven_idris_resource_state_description : Int -> String
proven_idris_resource_state_description state =
  if state == 0 then "Unacquired (ready for acquisition)"
  else if state == 1 then "Acquired (in use)"
  else if state == 2 then "Released (available for reuse)"
  else "Unknown state"
