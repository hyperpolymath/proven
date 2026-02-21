-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeBloom operations
|||
||| This module exports Bloom filter helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total and calculate optimal parameters.
|||
||| Return conventions:
||| - Parameters → Int (size, number of hashes)
||| - Statistics → Double (fill ratio, false positive rate)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: Bloom filters are probabilistic data structures.
|||           - No false negatives: if contains returns false, item is definitely not present
|||           - Possible false positives: if contains returns true, item might be present
|||           - Trade-off: size vs false positive rate
|||
||| Parameters:
||| - m: filter size (number of bits)
||| - n: expected number of items
||| - k: number of hash functions
||| - p: target false positive rate (0 < p < 1)
|||
||| Optimal formulas:
||| - m = -n × ln(p) / ln(2)²
||| - k = (m/n) × ln(2)
|||
||| NOTE: Actual bit array storage managed in Zig.
|||       These helpers calculate optimal parameters and validate operations.
module Proven.FFI.SafeBloom

import Proven.SafeBloom
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
-- Mathematical Constants
--------------------------------------------------------------------------------

export
proven_idris_bloom_ln2 : Double
proven_idris_bloom_ln2 = 0.6931471805599453

export
proven_idris_bloom_ln2_squared : Double
proven_idris_bloom_ln2_squared = 0.4804530139182014

--------------------------------------------------------------------------------
-- Optimal Parameter Calculation
--------------------------------------------------------------------------------

export
proven_idris_bloom_optimal_size : Int -> Double -> Int
proven_idris_bloom_optimal_size expectedItems falsePositiveRate =
  cast (optimalSize (cast expectedItems) falsePositiveRate)

export
proven_idris_bloom_optimal_hashes : Int -> Int -> Int
proven_idris_bloom_optimal_hashes filterSize expectedItems =
  cast (optimalHashes (cast filterSize) (cast expectedItems))

export
proven_idris_bloom_create_optimal : Int -> Double -> (Int, Int)
proven_idris_bloom_create_optimal expectedItems falsePositiveRate =
  let sz = proven_idris_bloom_optimal_size expectedItems falsePositiveRate
      nh = proven_idris_bloom_optimal_hashes sz expectedItems
  in (sz, nh)

--------------------------------------------------------------------------------
-- Parameter Validation
--------------------------------------------------------------------------------

export
proven_idris_bloom_is_valid_size : Int -> Int
proven_idris_bloom_is_valid_size size =
  encodeBool (size > 0 && size < 1000000000)  -- Reasonable upper limit

export
proven_idris_bloom_is_valid_num_hashes : Int -> Int
proven_idris_bloom_is_valid_num_hashes numHashes =
  encodeBool (numHashes > 0 && numHashes <= 20)  -- Practical range

export
proven_idris_bloom_is_valid_fp_rate : Double -> Int
proven_idris_bloom_is_valid_fp_rate rate =
  encodeBool (rate > 0.0 && rate < 1.0)

export
proven_idris_bloom_params_compatible : Int -> Int -> Int -> Int -> Int
proven_idris_bloom_params_compatible size1 hashes1 size2 hashes2 =
  encodeBool (size1 == size2 && hashes1 == hashes2)

--------------------------------------------------------------------------------
-- False Positive Rate Estimation
--------------------------------------------------------------------------------

export
proven_idris_bloom_estimate_fp_rate : Int -> Int -> Int -> Double
proven_idris_bloom_estimate_fp_rate filterSize numHashes bitsSet =
  let k = cast numHashes
      m = cast filterSize
      ones = cast bitsSet
  in pow (ones / m) k

export
proven_idris_bloom_expected_fp_rate : Int -> Int -> Int -> Double
proven_idris_bloom_expected_fp_rate filterSize numHashes itemsInserted =
  -- Expected FP rate after n insertions: (1 - e^(-kn/m))^k
  let k = cast numHashes
      m = cast filterSize
      n = cast itemsInserted
      p = 1.0 - exp (negate k * n / m)
  in pow p k

--------------------------------------------------------------------------------
-- Fill Ratio and Saturation
--------------------------------------------------------------------------------

export
proven_idris_bloom_fill_ratio : Int -> Int -> Double
proven_idris_bloom_fill_ratio bitsSet filterSize =
  if filterSize == 0 then 0.0
  else cast bitsSet / cast filterSize

export
proven_idris_bloom_fill_ratio_percent : Int -> Int -> Double
proven_idris_bloom_fill_ratio_percent bitsSet filterSize =
  proven_idris_bloom_fill_ratio bitsSet filterSize * 100.0

export
proven_idris_bloom_is_saturated : Int -> Int -> Int
proven_idris_bloom_is_saturated bitsSet filterSize =
  encodeBool (bitsSet >= filterSize)

export
proven_idris_bloom_is_nearly_saturated : Int -> Int -> Int -> Int
proven_idris_bloom_is_nearly_saturated bitsSet filterSize threshold =
  let percent = cast (bitsSet * 100) / cast filterSize
  in encodeBool (percent >= cast threshold)

--------------------------------------------------------------------------------
-- Expected Bits Set
--------------------------------------------------------------------------------

export
proven_idris_bloom_expected_bits_set : Int -> Int -> Int -> Int
proven_idris_bloom_expected_bits_set filterSize numHashes itemsInserted =
  -- Expected bits set after n insertions: m × (1 - (1 - 1/m)^(kn))
  let k = cast numHashes
      m = cast filterSize
      n = cast itemsInserted
      prob = 1.0 - pow (1.0 - 1.0 / m) (k * n)
  in cast (m * prob)

export
proven_idris_bloom_expected_empty_bits : Int -> Int -> Int -> Int
proven_idris_bloom_expected_empty_bits filterSize numHashes itemsInserted =
  let expected = proven_idris_bloom_expected_bits_set filterSize numHashes itemsInserted
  in filterSize - expected

--------------------------------------------------------------------------------
-- Capacity Planning
--------------------------------------------------------------------------------

export
proven_idris_bloom_max_items_for_rate : Int -> Int -> Double -> Int
proven_idris_bloom_max_items_for_rate filterSize numHashes maxFpRate =
  -- Solve for n given m, k, and target p: n ≈ -m × ln(p) / (k × ln(2))
  let m = cast filterSize
      k = cast numHashes
      p = maxFpRate
  in cast (negate m * log p / (k * 0.6931471805599453))

export
proven_idris_bloom_recommend_resize_at : Int -> Int -> Double -> Int
proven_idris_bloom_recommend_resize_at filterSize numHashes maxFpRate =
  -- Recommend resizing when 75% of max capacity is reached
  let max = proven_idris_bloom_max_items_for_rate filterSize numHashes maxFpRate
  in (max * 75) `div` 100

--------------------------------------------------------------------------------
-- Hash Function Helpers
--------------------------------------------------------------------------------

export
proven_idris_bloom_hash_seed : Int -> Int
proven_idris_bloom_hash_seed i = i

export
proven_idris_bloom_num_hash_indices : Int -> Int
proven_idris_bloom_num_hash_indices numHashes = numHashes

export
proven_idris_bloom_bit_index_in_range : Int -> Int -> Int
proven_idris_bloom_bit_index_in_range index filterSize =
  encodeBool (index >= 0 && index < filterSize)

--------------------------------------------------------------------------------
-- Memory Usage
--------------------------------------------------------------------------------

export
proven_idris_bloom_memory_bits : Int -> Int
proven_idris_bloom_memory_bits filterSize = filterSize

export
proven_idris_bloom_memory_bytes : Int -> Int
proven_idris_bloom_memory_bytes filterSize =
  (filterSize + 7) `div` 8  -- Round up to nearest byte

export
proven_idris_bloom_memory_kb : Int -> Double
proven_idris_bloom_memory_kb filterSize =
  cast (proven_idris_bloom_memory_bytes filterSize) / 1024.0

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

export
proven_idris_bloom_more_accurate : Double -> Double -> Int
proven_idris_bloom_more_accurate fpRate1 fpRate2 =
  encodeBool (fpRate1 < fpRate2)

export
proven_idris_bloom_compare_accuracy : Double -> Double -> Int
proven_idris_bloom_compare_accuracy fpRate1 fpRate2 =
  case compare fpRate1 fpRate2 of
    LT => (-1)  -- First is more accurate (lower FP rate)
    EQ => 0
    GT => 1

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

export
proven_idris_bloom_bits_per_item : Int -> Int -> Double
proven_idris_bloom_bits_per_item filterSize expectedItems =
  if expectedItems == 0 then 0.0
  else cast filterSize / cast expectedItems

export
proven_idris_bloom_hashes_per_item : Int -> Int
proven_idris_bloom_hashes_per_item numHashes = numHashes

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_bloom_friendly_error : String -> String
proven_idris_bloom_friendly_error errorMsg =
  if isInfixOf "saturated" (toLower errorMsg) || isInfixOf "full" (toLower errorMsg)
    then "Bloom filter is saturated (all bits set, high false positive rate)"
  else if isInfixOf "false positive" (toLower errorMsg) || isInfixOf "fp rate" (toLower errorMsg)
    then "False positive rate exceeded threshold"
  else if isInfixOf "incompatible" (toLower errorMsg) || isInfixOf "parameters" (toLower errorMsg)
    then "Bloom filters have incompatible parameters (different size or hash count)"
  else if isInfixOf "size" (toLower errorMsg)
    then "Invalid Bloom filter size (must be positive)"
  else if isInfixOf "hashes" (toLower errorMsg)
    then "Invalid number of hash functions"
  else
    "Bloom filter error"
