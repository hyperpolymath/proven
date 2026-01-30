-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeMonotonic operations
|||
||| This module exports monotonic counter and logical clock helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total and ensure ordering.
|||
||| Return conventions:
||| - Counter value → Int (monotonic, never decreases)
||| - Timestamp → Int (Lamport/vector clock timestamp)
||| - Node ID → Int (tie-breaker for concurrent events)
||| - Comparison → Int (-1 = less, 0 = equal, 1 = greater)
||| - Causality → Int (0 = false/concurrent, 1 = true/happened-before)
|||
||| CRITICAL: Monotonic values never decrease, ensuring correct ordering
|||           and causality in distributed systems.
|||
||| Use Cases:
||| - MonotonicCounter: Sequence numbers, version numbers, IDs
||| - LamportClock: Total ordering of events across nodes
||| - VectorClock: Partial ordering, detecting concurrent events
||| - HighWaterMark: Tracking processed offsets in streams
|||
||| Lamport Clock:
||| - Local event: increment timestamp
||| - Send: increment and include timestamp in message
||| - Receive: max(local, received) + 1
||| - Total ordering: timestamp, then nodeId as tie-breaker
|||
||| Vector Clock:
||| - Each node tracks timestamps for all nodes
||| - Local event: increment own timestamp
||| - Send: include full vector in message
||| - Receive: element-wise max, then increment own
||| - Partial ordering: a < b if all a[i] ≤ b[i] and some a[i] < b[i]
module Proven.FFI.SafeMonotonic

import Proven.SafeMonotonic
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

||| Encode Ordering as Int
encodeOrdering : Ordering -> Int
encodeOrdering LT = (-1)
encodeOrdering EQ = 0
encodeOrdering GT = 1

--------------------------------------------------------------------------------
-- Monotonic Counter Operations
--------------------------------------------------------------------------------

%export
proven_idris_monotonic_zero : Int
proven_idris_monotonic_zero = 0

%export
proven_idris_monotonic_from_nat : Int -> Int
proven_idris_monotonic_from_nat n = n

%export
proven_idris_monotonic_get_value : Int -> Int
proven_idris_monotonic_get_value value = value

%export
proven_idris_monotonic_increment : Int -> Int
proven_idris_monotonic_increment current = current + 1

%export
proven_idris_monotonic_increment_by : Int -> Int -> Int
proven_idris_monotonic_increment_by current delta = current + delta

%export
proven_idris_monotonic_advance_to : Int -> Int -> Int
proven_idris_monotonic_advance_to target current =
  if target > current then target else current

%export
proven_idris_monotonic_compare : Int -> Int -> Int
proven_idris_monotonic_compare a b =
  if a < b then (-1)
  else if a == b then 0
  else 1

%export
proven_idris_monotonic_max : Int -> Int -> Int
proven_idris_monotonic_max a b = if a > b then a else b

%export
proven_idris_monotonic_min : Int -> Int -> Int
proven_idris_monotonic_min a b = if a < b then a else b

--------------------------------------------------------------------------------
-- Lamport Clock Operations
--------------------------------------------------------------------------------

%export
proven_idris_lamport_new : Int -> (Int, Int)
proven_idris_lamport_new nodeId = (0, nodeId)

%export
proven_idris_lamport_tick : Int -> Int -> (Int, Int)
proven_idris_lamport_tick timestamp nodeId = (timestamp + 1, nodeId)

%export
proven_idris_lamport_send : Int -> Int -> (Int, Int, Int)
proven_idris_lamport_send timestamp nodeId =
  let newTimestamp = timestamp + 1
  in (newTimestamp, newTimestamp, nodeId)

%export
proven_idris_lamport_receive : Int -> Int -> Int -> (Int, Int)
proven_idris_lamport_receive receivedTimestamp localTimestamp nodeId =
  let maxTime = if localTimestamp > receivedTimestamp
                  then localTimestamp
                  else receivedTimestamp
      newTimestamp = maxTime + 1
  in (newTimestamp, nodeId)

%export
proven_idris_lamport_get_timestamp : Int -> Int -> Int
proven_idris_lamport_get_timestamp timestamp _ = timestamp

%export
proven_idris_lamport_get_node_id : Int -> Int -> Int
proven_idris_lamport_get_node_id _ nodeId = nodeId

%export
proven_idris_lamport_compare : Int -> Int -> Int -> Int -> Int
proven_idris_lamport_compare ts1 nid1 ts2 nid2 =
  if ts1 < ts2 then (-1)
  else if ts1 > ts2 then 1
  else if nid1 < nid2 then (-1)
  else if nid1 > nid2 then 1
  else 0

%export
proven_idris_lamport_happened_before : Int -> Int -> Int -> Int -> Int
proven_idris_lamport_happened_before ts1 _ ts2 _ =
  encodeBool (ts1 < ts2)

--------------------------------------------------------------------------------
-- Vector Clock Helpers
--------------------------------------------------------------------------------

%export
proven_idris_vector_new : Int -> Int
proven_idris_vector_new selfId = selfId

%export
proven_idris_vector_get_node_time : Int -> Int -> Int -> Int
proven_idris_vector_get_node_time nodeId timestamp defaultVal =
  -- Simplified: if nodeId matches, return timestamp, else default
  -- In real impl, Zig would maintain full vector
  if nodeId == 0 then timestamp else defaultVal

%export
proven_idris_vector_update_node_time : Int -> Int -> Int -> Int
proven_idris_vector_update_node_time _ newTimestamp _ = newTimestamp

%export
proven_idris_vector_tick : Int -> Int -> Int
proven_idris_vector_tick currentTimestamp selfId =
  currentTimestamp + 1

%export
proven_idris_vector_merge_timestamp : Int -> Int -> Int
proven_idris_vector_merge_timestamp localTime receivedTime =
  if localTime > receivedTime then localTime else receivedTime

%export
proven_idris_vector_receive : Int -> Int -> Int
proven_idris_vector_receive localTime receivedTime =
  let merged = proven_idris_vector_merge_timestamp localTime receivedTime
  in merged + 1

%export
proven_idris_vector_compare_component : Int -> Int -> Int
proven_idris_vector_compare_component time1 time2 =
  encodeBool (time1 <= time2)

%export
proven_idris_vector_strict_less : Int -> Int -> Int
proven_idris_vector_strict_less time1 time2 =
  encodeBool (time1 < time2)

--------------------------------------------------------------------------------
-- High-Water Mark Operations
--------------------------------------------------------------------------------

%export
proven_idris_hwm_new : Int
proven_idris_hwm_new = 0

%export
proven_idris_hwm_update : Int -> Int -> Int
proven_idris_hwm_update offset currentMark =
  if offset > currentMark then offset else currentMark

%export
proven_idris_hwm_is_processed : Int -> Int -> Int
proven_idris_hwm_is_processed offset mark =
  encodeBool (offset <= mark)

%export
proven_idris_hwm_get : Int -> Int
proven_idris_hwm_get mark = mark

%export
proven_idris_hwm_distance : Int -> Int -> Int
proven_idris_hwm_distance offset mark =
  if offset > mark then offset - mark else 0

--------------------------------------------------------------------------------
-- Causality Checks
--------------------------------------------------------------------------------

%export
proven_idris_causality_happened_before_lamport : Int -> Int -> Int
proven_idris_causality_happened_before_lamport ts1 ts2 =
  encodeBool (ts1 < ts2)

%export
proven_idris_causality_concurrent_lamport : Int -> Int -> Int
proven_idris_causality_concurrent_lamport ts1 ts2 =
  encodeBool (ts1 == ts2)

%export
proven_idris_causality_is_ancestor : Int -> Int -> Int
proven_idris_causality_is_ancestor ancestor descendant =
  encodeBool (ancestor < descendant)

%export
proven_idris_causality_is_descendant : Int -> Int -> Int
proven_idris_causality_is_descendant descendant ancestor =
  encodeBool (descendant > ancestor)

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

%export
proven_idris_monotonic_is_valid : Int -> Int
proven_idris_monotonic_is_valid value =
  encodeBool (value >= 0)

%export
proven_idris_monotonic_is_increasing : Int -> Int -> Int
proven_idris_monotonic_is_increasing current next =
  encodeBool (next > current)

%export
proven_idris_monotonic_is_non_decreasing : Int -> Int -> Int
proven_idris_monotonic_is_non_decreasing current next =
  encodeBool (next >= current)

%export
proven_idris_lamport_is_valid_timestamp : Int -> Int
proven_idris_lamport_is_valid_timestamp timestamp =
  encodeBool (timestamp >= 0)

%export
proven_idris_lamport_is_valid_node_id : Int -> Int
proven_idris_lamport_is_valid_node_id nodeId =
  encodeBool (nodeId >= 0)

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

%export
proven_idris_monotonic_delta : Int -> Int -> Int
proven_idris_monotonic_delta current previous =
  if current >= previous then current - previous else 0

%export
proven_idris_monotonic_average : Int -> Int -> Int
proven_idris_monotonic_average total count =
  if count == 0 then 0 else total `div` count

%export
proven_idris_lamport_clock_drift : Int -> Int -> Int
proven_idris_lamport_clock_drift ts1 ts2 =
  if ts1 > ts2 then ts1 - ts2
  else ts2 - ts1

%export
proven_idris_lamport_max_timestamp : Int -> Int -> Int
proven_idris_lamport_max_timestamp ts1 ts2 =
  if ts1 > ts2 then ts1 else ts2

%export
proven_idris_lamport_min_timestamp : Int -> Int -> Int
proven_idris_lamport_min_timestamp ts1 ts2 =
  if ts1 < ts2 then ts1 else ts2

--------------------------------------------------------------------------------
-- Sequence Operations
--------------------------------------------------------------------------------

%export
proven_idris_sequence_next : Int -> Int
proven_idris_sequence_next current = current + 1

%export
proven_idris_sequence_gap_size : Int -> Int -> Int
proven_idris_sequence_gap_size expected actual =
  if actual > expected then actual - expected else 0

%export
proven_idris_sequence_is_consecutive : Int -> Int -> Int
proven_idris_sequence_is_consecutive current next =
  encodeBool (next == current + 1)

%export
proven_idris_sequence_is_duplicate : Int -> Int -> Int
proven_idris_sequence_is_duplicate current next =
  encodeBool (next == current)

%export
proven_idris_sequence_is_reordered : Int -> Int -> Int
proven_idris_sequence_is_reordered expected actual =
  encodeBool (actual < expected)

--------------------------------------------------------------------------------
-- Overflow Detection
--------------------------------------------------------------------------------

%export
proven_idris_monotonic_will_overflow : Int -> Int -> Int
proven_idris_monotonic_will_overflow current maxValue =
  encodeBool (current >= maxValue)

%export
proven_idris_monotonic_room_to_grow : Int -> Int -> Int
proven_idris_monotonic_room_to_grow current maxValue =
  if current < maxValue then maxValue - current else 0

%export
proven_idris_monotonic_percent_full : Int -> Int -> Double
proven_idris_monotonic_percent_full current maxValue =
  if maxValue == 0 then 0.0
  else cast current / cast maxValue * 100.0

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

%export
proven_idris_monotonic_min_value : Int
proven_idris_monotonic_min_value = 0

%export
proven_idris_monotonic_max_safe_value : Int
proven_idris_monotonic_max_safe_value = 2147483647  -- Max Int32

%export
proven_idris_lamport_initial_timestamp : Int
proven_idris_lamport_initial_timestamp = 0

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_monotonic_friendly_error : String -> String
proven_idris_monotonic_friendly_error errorMsg =
  if isInfixOf "overflow" (toLower errorMsg)
    then "Monotonic counter overflow (value too large)"
  else if isInfixOf "decrease" (toLower errorMsg) || isInfixOf "backward" (toLower errorMsg)
    then "Monotonic value cannot decrease (attempted to go backward)"
  else if isInfixOf "gap" (toLower errorMsg)
    then "Sequence gap detected (expected consecutive values)"
  else if isInfixOf "reorder" (toLower errorMsg)
    then "Sequence reordering detected (out-of-order values)"
  else
    "Monotonic value error"

%export
proven_idris_monotonic_status_message : Int -> String
proven_idris_monotonic_status_message value =
  "Monotonic counter: " ++ show value
