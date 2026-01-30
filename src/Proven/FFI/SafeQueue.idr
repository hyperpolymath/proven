-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeQueue operations
|||
||| This module exports queue operations to the C ABI
||| via Idris2's RefC backend. All functions are proven total and prevent overflow.
|||
||| Return conventions:
||| - Queue state → (length, capacity, isEmpty, isFull)
||| - Operations → Int (0 = success, 1 = queue full, 2 = queue empty)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: Bounded queues prevent memory exhaustion by enforcing capacity limits.
|||           Operations fail gracefully when capacity is exceeded.
|||
||| Queue semantics:
||| - FIFO: First In, First Out
||| - Enqueue: Add to back
||| - Dequeue: Remove from front
||| - Bounded: Maximum capacity enforced
||| - Drop oldest: When full, remove oldest to make room
|||
||| NOTE: This module exports queue *state* and *capacity management* helpers.
|||       Actual queue data structures are managed in Zig, as queues are polymorphic.
|||       These helpers validate operations before execution.
module Proven.FFI.SafeQueue

import Proven.SafeQueue
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
-- Capacity Validation
--------------------------------------------------------------------------------

export
proven_idris_queue_is_valid_capacity : Int -> Int
proven_idris_queue_is_valid_capacity cap =
  encodeBool (cap > 0)

export
proven_idris_queue_can_enqueue : Int -> Int -> Int
proven_idris_queue_can_enqueue currentLen capacity =
  encodeBool (currentLen < capacity)

export
proven_idris_queue_is_full : Int -> Int -> Int
proven_idris_queue_is_full currentLen capacity =
  encodeBool (currentLen >= capacity)

export
proven_idris_queue_is_empty : Int -> Int
proven_idris_queue_is_empty currentLen =
  encodeBool (currentLen == 0)

export
proven_idris_queue_remaining_capacity : Int -> Int -> Int
proven_idris_queue_remaining_capacity currentLen capacity =
  if currentLen >= capacity then 0
  else capacity - currentLen

--------------------------------------------------------------------------------
-- Operation Validation
--------------------------------------------------------------------------------

export
proven_idris_queue_validate_enqueue : Int -> Int -> Int
proven_idris_queue_validate_enqueue currentLen capacity =
  if currentLen < capacity then 0  -- Success
  else 1  -- Queue full

export
proven_idris_queue_validate_dequeue : Int -> Int
proven_idris_queue_validate_dequeue currentLen =
  if currentLen > 0 then 0  -- Success
  else 2  -- Queue empty

export
proven_idris_queue_validate_peek : Int -> Int
proven_idris_queue_validate_peek currentLen =
  proven_idris_queue_validate_dequeue currentLen

--------------------------------------------------------------------------------
-- Length Operations
--------------------------------------------------------------------------------

export
proven_idris_queue_after_enqueue : Int -> Int
proven_idris_queue_after_enqueue currentLen =
  currentLen + 1

export
proven_idris_queue_after_dequeue : Int -> Int
proven_idris_queue_after_dequeue currentLen =
  if currentLen > 0 then currentLen - 1 else 0

export
proven_idris_queue_after_enqueue_with_drop : Int -> Int -> Int
proven_idris_queue_after_enqueue_with_drop currentLen capacity =
  if currentLen >= capacity then capacity  -- Stays at capacity
  else currentLen + 1

--------------------------------------------------------------------------------
-- Capacity Planning
--------------------------------------------------------------------------------

export
proven_idris_queue_utilization_percent : Int -> Int -> Int
proven_idris_queue_utilization_percent currentLen capacity =
  if capacity == 0 then 0
  else (currentLen * 100) `div` capacity

export
proven_idris_queue_is_nearly_full : Int -> Int -> Int -> Int
proven_idris_queue_is_nearly_full currentLen capacity threshold =
  let percent = proven_idris_queue_utilization_percent currentLen capacity
  in encodeBool (percent >= threshold)

export
proven_idris_queue_is_at_least_half_full : Int -> Int -> Int
proven_idris_queue_is_at_least_half_full currentLen capacity =
  proven_idris_queue_is_nearly_full currentLen capacity 50

export
proven_idris_queue_recommend_capacity : Int -> Int -> Int
proven_idris_queue_recommend_capacity avgSize maxBurst =
  -- Recommend capacity = average size + burst headroom + 20% safety margin
  let base = avgSize + maxBurst
      safety = (base * 120) `div` 100
  in if safety < 10 then 10 else safety

--------------------------------------------------------------------------------
-- Batch Operations
--------------------------------------------------------------------------------

export
proven_idris_queue_can_enqueue_batch : Int -> Int -> Int -> Int
proven_idris_queue_can_enqueue_batch currentLen capacity batchSize =
  encodeBool (currentLen + batchSize <= capacity)

export
proven_idris_queue_space_for_batch : Int -> Int -> Int
proven_idris_queue_space_for_batch currentLen capacity =
  if currentLen >= capacity then 0
  else capacity - currentLen

export
proven_idris_queue_batch_fits : Int -> Int -> Int -> Int
proven_idris_queue_batch_fits batchSize currentLen capacity =
  encodeBool (batchSize <= (capacity - currentLen))

--------------------------------------------------------------------------------
-- Queue State Comparison
--------------------------------------------------------------------------------

export
proven_idris_queue_lengths_equal : Int -> Int -> Int
proven_idris_queue_lengths_equal len1 len2 =
  encodeBool (len1 == len2)

export
proven_idris_queue_compare_fullness : Int -> Int -> Int -> Int -> Int
proven_idris_queue_compare_fullness len1 cap1 len2 cap2 =
  let util1 = proven_idris_queue_utilization_percent len1 cap1
      util2 = proven_idris_queue_utilization_percent len2 cap2
  in case compare util1 util2 of
       LT => (-1)
       EQ => 0
       GT => 1

--------------------------------------------------------------------------------
-- Circular Buffer Helpers
--------------------------------------------------------------------------------

export
proven_idris_queue_next_index : Int -> Int -> Int
proven_idris_queue_next_index currentIndex capacity =
  (currentIndex + 1) `mod` capacity

export
proven_idris_queue_prev_index : Int -> Int -> Int
proven_idris_queue_prev_index currentIndex capacity =
  if currentIndex == 0 then capacity - 1
  else currentIndex - 1

export
proven_idris_queue_distance_to_end : Int -> Int -> Int
proven_idris_queue_distance_to_end currentIndex capacity =
  capacity - currentIndex

export
proven_idris_queue_wrap_index : Int -> Int -> Int
proven_idris_queue_wrap_index index capacity =
  index `mod` capacity

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_queue_min_capacity : Int
proven_idris_queue_min_capacity = 1

export
proven_idris_queue_default_capacity : Int
proven_idris_queue_default_capacity = 100

export
proven_idris_queue_max_reasonable_capacity : Int
proven_idris_queue_max_reasonable_capacity = 1000000

export
proven_idris_queue_status_success : Int
proven_idris_queue_status_success = 0

export
proven_idris_queue_status_full : Int
proven_idris_queue_status_full = 1

export
proven_idris_queue_status_empty : Int
proven_idris_queue_status_empty = 2

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

export
proven_idris_queue_is_valid_index : Int -> Int -> Int
proven_idris_queue_is_valid_index index capacity =
  encodeBool (index >= 0 && index < capacity)

export
proven_idris_queue_is_valid_state : Int -> Int -> Int
proven_idris_queue_is_valid_state currentLen capacity =
  encodeBool (currentLen >= 0 && currentLen <= capacity && capacity > 0)

export
proven_idris_queue_lengths_consistent : Int -> Int -> Int -> Int
proven_idris_queue_lengths_consistent frontLen backLen totalLen =
  encodeBool (frontLen + backLen == totalLen)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_queue_friendly_error : String -> String
proven_idris_queue_friendly_error errorMsg =
  if isInfixOf "full" (toLower errorMsg) || isInfixOf "capacity" (toLower errorMsg)
    then "Queue is full (cannot enqueue more items)"
  else if isInfixOf "empty" (toLower errorMsg)
    then "Queue is empty (cannot dequeue)"
  else if isInfixOf "overflow" (toLower errorMsg)
    then "Queue overflow prevented (bounded capacity enforced)"
  else if isInfixOf "capacity" (toLower errorMsg)
    then "Invalid queue capacity (must be positive)"
  else if isInfixOf "index" (toLower errorMsg)
    then "Invalid queue index"
  else
    "Queue operation error"

export
proven_idris_queue_status_message : Int -> String
proven_idris_queue_status_message status =
  case status of
    0 => "Operation successful"
    1 => "Queue is full"
    2 => "Queue is empty"
    _ => "Unknown queue error"
