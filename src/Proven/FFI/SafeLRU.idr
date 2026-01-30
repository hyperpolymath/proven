-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeLRU operations
|||
||| This module exports LRU cache helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total and manage cache capacity.
|||
||| Return conventions:
||| - Cache state → (size, capacity, isEmpty, isFull)
||| - Statistics → Double (fill ratio, hit rate)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: LRU caches bound memory usage by evicting least recently used items.
|||           Proper capacity management prevents unbounded growth.
|||
||| Cache semantics:
||| - LRU: Least Recently Used eviction policy
||| - Get: Updates access time (item becomes most recently used)
||| - Peek: Does NOT update access time
||| - Put: Evicts LRU item if at capacity
||| - Bounded: Maximum size enforced
|||
||| NOTE: This module exports cache *state* and *capacity management* helpers.
|||       Actual cache data structures are managed in Zig, as caches are polymorphic.
|||       These helpers validate operations and calculate statistics.
module Proven.FFI.SafeLRU

import Proven.SafeLRU
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
proven_idris_lru_is_valid_capacity : Int -> Int
proven_idris_lru_is_valid_capacity cap =
  encodeBool (cap > 0)

export
proven_idris_lru_is_empty : Int -> Int
proven_idris_lru_is_empty currentSize =
  encodeBool (currentSize == 0)

export
proven_idris_lru_is_full : Int -> Int -> Int
proven_idris_lru_is_full currentSize capacity =
  encodeBool (currentSize >= capacity)

export
proven_idris_lru_remaining_capacity : Int -> Int -> Int
proven_idris_lru_remaining_capacity currentSize capacity =
  if currentSize >= capacity then 0
  else capacity - currentSize

export
proven_idris_lru_can_insert_without_eviction : Int -> Int -> Int
proven_idris_lru_can_insert_without_eviction currentSize capacity =
  encodeBool (currentSize < capacity)

--------------------------------------------------------------------------------
-- Cache Statistics
--------------------------------------------------------------------------------

export
proven_idris_lru_fill_ratio : Int -> Int -> Double
proven_idris_lru_fill_ratio currentSize capacity =
  if capacity == 0 then 0.0
  else cast currentSize / cast capacity

export
proven_idris_lru_fill_ratio_percent : Int -> Int -> Double
proven_idris_lru_fill_ratio_percent currentSize capacity =
  proven_idris_lru_fill_ratio currentSize capacity * 100.0

export
proven_idris_lru_is_nearly_full : Int -> Int -> Int -> Int
proven_idris_lru_is_nearly_full currentSize capacity threshold =
  let percent = cast (currentSize * 100) / cast capacity
  in encodeBool (percent >= cast threshold)

--------------------------------------------------------------------------------
-- Hit Rate Calculation
--------------------------------------------------------------------------------

export
proven_idris_lru_hit_rate : Int -> Int -> Double
proven_idris_lru_hit_rate hits total =
  if total == 0 then 0.0
  else cast hits / cast total

export
proven_idris_lru_hit_rate_percent : Int -> Int -> Double
proven_idris_lru_hit_rate_percent hits total =
  proven_idris_lru_hit_rate hits total * 100.0

export
proven_idris_lru_miss_rate : Int -> Int -> Double
proven_idris_lru_miss_rate hits total =
  1.0 - proven_idris_lru_hit_rate hits total

export
proven_idris_lru_miss_rate_percent : Int -> Int -> Double
proven_idris_lru_miss_rate_percent hits total =
  proven_idris_lru_miss_rate hits total * 100.0

--------------------------------------------------------------------------------
-- Eviction Statistics
--------------------------------------------------------------------------------

export
proven_idris_lru_evictions_since_start : Int -> Int -> Int
proven_idris_lru_evictions_since_start totalInserts capacity =
  if totalInserts <= capacity then 0
  else totalInserts - capacity

export
proven_idris_lru_eviction_rate : Int -> Int -> Double
proven_idris_lru_eviction_rate evictions totalInserts =
  if totalInserts == 0 then 0.0
  else cast evictions / cast totalInserts

export
proven_idris_lru_will_evict : Int -> Int -> Int
proven_idris_lru_will_evict currentSize capacity =
  encodeBool (currentSize >= capacity)

--------------------------------------------------------------------------------
-- Access Counter Operations
--------------------------------------------------------------------------------

export
proven_idris_lru_next_access_counter : Int -> Int
proven_idris_lru_next_access_counter current = current + 1

export
proven_idris_lru_access_counter_overflow : Int -> Int
proven_idris_lru_access_counter_overflow counter =
  encodeBool (counter >= 2147483647)  -- Max Int32

export
proven_idris_lru_should_reset_counters : Int -> Int
proven_idris_lru_should_reset_counters counter =
  -- Recommend resetting when approaching overflow
  encodeBool (counter >= 2000000000)

--------------------------------------------------------------------------------
-- Capacity Planning
--------------------------------------------------------------------------------

export
proven_idris_lru_recommend_capacity : Int -> Double -> Int
proven_idris_lru_recommend_capacity workingSet hitRate =
  -- Recommend capacity based on working set and desired hit rate
  -- Higher hit rate needs more capacity
  let factor = if hitRate >= 0.95 then 1.5
               else if hitRate >= 0.90 then 1.3
               else if hitRate >= 0.80 then 1.2
               else 1.1
  in cast (cast workingSet * factor)

export
proven_idris_lru_optimal_capacity_for_hit_rate : Int -> Double -> Int
proven_idris_lru_optimal_capacity_for_hit_rate avgAccesses targetHitRate =
  -- Simple model: capacity = accesses × (1 / (1 - hitRate))
  let factor = 1.0 / (1.0 - targetHitRate)
  in max 10 (cast (cast avgAccesses * factor))

export
proven_idris_lru_recommend_resize : Int -> Int -> Int -> Int -> Int
proven_idris_lru_recommend_resize currentSize capacity hits total =
  let hitRate = proven_idris_lru_hit_rate hits total
      targetHitRate = 0.85  -- Target 85% hit rate
  in if hitRate < targetHitRate
       then cast (cast capacity * 1.5)  -- Increase capacity by 50%
       else capacity

--------------------------------------------------------------------------------
-- Cache Efficiency
--------------------------------------------------------------------------------

export
proven_idris_lru_is_efficient : Int -> Int -> Double -> Int
proven_idris_lru_is_efficient hits total minHitRate =
  let rate = proven_idris_lru_hit_rate hits total
  in encodeBool (rate >= minHitRate)

export
proven_idris_lru_utilization : Int -> Int -> Double
proven_idris_lru_utilization currentSize capacity =
  proven_idris_lru_fill_ratio currentSize capacity

export
proven_idris_lru_is_underutilized : Int -> Int -> Int
proven_idris_lru_is_underutilized currentSize capacity =
  let ratio = proven_idris_lru_fill_ratio currentSize capacity
  in encodeBool (ratio < 0.25)  -- Less than 25% full

export
proven_idris_lru_is_overutilized : Int -> Int -> Int
proven_idris_lru_is_overutilized currentSize capacity =
  let ratio = proven_idris_lru_fill_ratio currentSize capacity
  in encodeBool (ratio > 0.90)  -- More than 90% full

--------------------------------------------------------------------------------
-- Operation Tracking
--------------------------------------------------------------------------------

export
proven_idris_lru_operations_per_eviction : Int -> Int -> Double
proven_idris_lru_operations_per_eviction totalOps evictions =
  if evictions == 0 then 0.0
  else cast totalOps / cast evictions

export
proven_idris_lru_avg_lifetime_items : Int -> Int -> Double
proven_idris_lru_avg_lifetime_items totalInserts evictions =
  if evictions == 0 then 0.0
  else cast totalInserts / cast evictions

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

export
proven_idris_lru_compare_hit_rates : Int -> Int -> Int -> Int -> Int
proven_idris_lru_compare_hit_rates hits1 total1 hits2 total2 =
  let rate1 = proven_idris_lru_hit_rate hits1 total1
      rate2 = proven_idris_lru_hit_rate hits2 total2
  in case compare rate1 rate2 of
       LT => (-1)
       EQ => 0
       GT => 1

export
proven_idris_lru_better_hit_rate : Double -> Double -> Int
proven_idris_lru_better_hit_rate rate1 rate2 =
  encodeBool (rate1 > rate2)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_lru_min_capacity : Int
proven_idris_lru_min_capacity = 1

export
proven_idris_lru_default_capacity : Int
proven_idris_lru_default_capacity = 128

export
proven_idris_lru_max_reasonable_capacity : Int
proven_idris_lru_max_reasonable_capacity = 10000000

export
proven_idris_lru_good_hit_rate : Double
proven_idris_lru_good_hit_rate = 0.80  -- 80%

export
proven_idris_lru_excellent_hit_rate : Double
proven_idris_lru_excellent_hit_rate = 0.95  -- 95%

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_lru_friendly_error : String -> String
proven_idris_lru_friendly_error errorMsg =
  if isInfixOf "capacity" (toLower errorMsg)
    then "Invalid LRU cache capacity (must be positive)"
  else if isInfixOf "full" (toLower errorMsg)
    then "LRU cache is full (will evict least recently used item)"
  else if isInfixOf "empty" (toLower errorMsg)
    then "LRU cache is empty (no items to retrieve)"
  else if isInfixOf "hit rate" (toLower errorMsg) || isInfixOf "efficiency" (toLower errorMsg)
    then "LRU cache hit rate is low (consider increasing capacity)"
  else if isInfixOf "overflow" (toLower errorMsg)
    then "LRU cache access counter overflow (reset recommended)"
  else
    "LRU cache error"

export
proven_idris_lru_performance_message : Double -> String
proven_idris_lru_performance_message hitRate =
  if hitRate >= 0.95 then "Excellent cache performance"
  else if hitRate >= 0.80 then "Good cache performance"
  else if hitRate >= 0.60 then "Fair cache performance"
  else "Poor cache performance (consider increasing capacity)"
