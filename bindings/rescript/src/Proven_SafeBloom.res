// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

open Proven_Bitwise

/**
 * SafeBloom - Probabilistic set membership that cannot crash.
 *
 * Provides a safe Bloom filter implementation for approximate set membership
 * testing. Bloom filters offer O(1) insertions and lookups with configurable
 * false positive rates, using no false negatives.
 *
 * Key properties:
 * - If contains() returns false, the item is definitely NOT in the set
 * - If contains() returns true, the item is PROBABLY in the set
 * - False positive rate depends on size, hash count, and number of items
 */

// ============================================================================
// Error types
// ============================================================================

/** Error types for bloom filter operations */
type bloomError =
  | InvalidSize
  | InvalidHashCount
  | InvalidFalsePositiveRate

/** Convert error to human-readable string */
let errorToString = (error: bloomError): string => {
  switch error {
  | InvalidSize => "Invalid filter size"
  | InvalidHashCount => "Invalid hash count"
  | InvalidFalsePositiveRate => "Invalid false positive rate (must be between 0 and 1)"
  }
}

// ============================================================================
// Bloom Filter Implementation
// ============================================================================

/** A Bloom filter for probabilistic set membership */
type bloomFilter = {
  mutable bits: array<bool>,
  size: int,
  numHashes: int,
  mutable itemCount: int,
}

/** Create a new Bloom filter with given size and number of hash functions */
let make = (size: int, numHashes: int): result<bloomFilter, bloomError> => {
  if size <= 0 {
    Error(InvalidSize)
  } else if numHashes <= 0 {
    Error(InvalidHashCount)
  } else {
    Ok({
      bits: Belt.Array.make(size, false),
      size,
      numHashes,
      itemCount: 0,
    })
  }
}

/** Compute a hash value using FNV-1a algorithm variant */
let fnv1aHash = (data: string, seed: int): int => {
  let prime = 16777619
  let offset = 2166136261
  let hash = ref(lxor(offset, seed))

  for i in 0 to Js.String2.length(data) - 1 {
    let byte = Js.String2.charCodeAt(data, i)->Belt.Float.toInt
    hash := lxor(hash.contents, byte)
    hash := land(hash.contents * prime, 0x7FFFFFFF) // Keep it positive
  }

  hash.contents
}

/** Compute hash at a specific index using double hashing */
let computeHashIndex = (data: string, hashIndex: int, size: int): int => {
  let h1 = fnv1aHash(data, 0)
  let h2 = fnv1aHash(data, h1)
  let combined = h1 + hashIndex * h2
  mod(land(combined, 0x7FFFFFFF), size)
}

/** Insert an item into the filter */
let insert = (filter: bloomFilter, data: string): unit => {
  for i in 0 to filter.numHashes - 1 {
    let index = computeHashIndex(data, i, filter.size)
    Belt.Array.setUnsafe(filter.bits, index, true)
  }
  filter.itemCount = filter.itemCount + 1
}

/** Insert bytes into the filter (converts to string internally) */
let insertBytes = (filter: bloomFilter, bytes: array<int>): unit => {
  let data = Belt.Array.map(bytes, b => Js.String2.fromCharCode(b))->Js.Array2.joinWith("")
  insert(filter, data)
}

/** Check if an item might be in the filter
 *
 * Returns:
 * - false: item is DEFINITELY NOT in the set
 * - true: item is PROBABLY in the set (may be false positive)
 */
let contains = (filter: bloomFilter, data: string): bool => {
  let found = ref(true)
  let i = ref(0)
  while found.contents && i.contents < filter.numHashes {
    let index = computeHashIndex(data, i.contents, filter.size)
    if !Belt.Array.getUnsafe(filter.bits, index) {
      found := false
    }
    i := i.contents + 1
  }
  found.contents
}

/** Check if bytes might be in the filter */
let containsBytes = (filter: bloomFilter, bytes: array<int>): bool => {
  let data = Belt.Array.map(bytes, b => Js.String2.fromCharCode(b))->Js.Array2.joinWith("")
  contains(filter, data)
}

/** Count the number of set bits */
let countOnes = (filter: bloomFilter): int => {
  let count = ref(0)
  for i in 0 to filter.size - 1 {
    if Belt.Array.getUnsafe(filter.bits, i) {
      count := count.contents + 1
    }
  }
  count.contents
}

/** Get the fill ratio (proportion of bits that are set) */
let fillRatio = (filter: bloomFilter): float => {
  Belt.Int.toFloat(countOnes(filter)) /. Belt.Int.toFloat(filter.size)
}

/** Estimate the current false positive rate
 *
 * This is based on the formula: (1 - e^(-kn/m))^k
 * where k = number of hashes, n = items inserted, m = filter size
 */
let estimatedFalsePositiveRate = (filter: bloomFilter): float => {
  let ratio = fillRatio(filter)
  Js.Math.pow_float(~base=ratio, ~exp=Belt.Int.toFloat(filter.numHashes))
}

/** Clear the filter, removing all items */
let clear = (filter: bloomFilter): unit => {
  for i in 0 to filter.size - 1 {
    Belt.Array.setUnsafe(filter.bits, i, false)
  }
  filter.itemCount = 0
}

/** Get the number of items that have been inserted */
let getItemCount = (filter: bloomFilter): int => {
  filter.itemCount
}

/** Get the filter size (number of bits) */
let getSize = (filter: bloomFilter): int => {
  filter.size
}

/** Get the number of hash functions */
let getNumHashes = (filter: bloomFilter): int => {
  filter.numHashes
}

// ============================================================================
// Union and Intersection
// ============================================================================

/** Create the union of two filters (OR operation)
 *
 * Both filters must have the same size and hash count
 */
let union = (filterA: bloomFilter, filterB: bloomFilter): option<bloomFilter> => {
  if filterA.size != filterB.size || filterA.numHashes != filterB.numHashes {
    None
  } else {
    let newBits = Belt.Array.make(filterA.size, false)
    for i in 0 to filterA.size - 1 {
      let a = Belt.Array.getUnsafe(filterA.bits, i)
      let b = Belt.Array.getUnsafe(filterB.bits, i)
      Belt.Array.setUnsafe(newBits, i, a || b)
    }
    Some({
      bits: newBits,
      size: filterA.size,
      numHashes: filterA.numHashes,
      itemCount: filterA.itemCount + filterB.itemCount, // Approximate
    })
  }
}

/** Create the intersection of two filters (AND operation)
 *
 * Both filters must have the same size and hash count
 */
let intersection = (filterA: bloomFilter, filterB: bloomFilter): option<bloomFilter> => {
  if filterA.size != filterB.size || filterA.numHashes != filterB.numHashes {
    None
  } else {
    let newBits = Belt.Array.make(filterA.size, false)
    for i in 0 to filterA.size - 1 {
      let a = Belt.Array.getUnsafe(filterA.bits, i)
      let b = Belt.Array.getUnsafe(filterB.bits, i)
      Belt.Array.setUnsafe(newBits, i, a && b)
    }
    Some({
      bits: newBits,
      size: filterA.size,
      numHashes: filterA.numHashes,
      itemCount: 0, // Cannot determine exact count
    })
  }
}

/** Merge another filter into this one (in-place union) */
let unionWith = (filter: bloomFilter, other: bloomFilter): result<unit, bloomError> => {
  if filter.size != other.size || filter.numHashes != other.numHashes {
    Error(InvalidSize)
  } else {
    for i in 0 to filter.size - 1 {
      let a = Belt.Array.getUnsafe(filter.bits, i)
      let b = Belt.Array.getUnsafe(other.bits, i)
      Belt.Array.setUnsafe(filter.bits, i, a || b)
    }
    filter.itemCount = filter.itemCount + other.itemCount
    Ok()
  }
}

/** Intersect with another filter (in-place intersection) */
let intersectWith = (filter: bloomFilter, other: bloomFilter): result<unit, bloomError> => {
  if filter.size != other.size || filter.numHashes != other.numHashes {
    Error(InvalidSize)
  } else {
    for i in 0 to filter.size - 1 {
      let a = Belt.Array.getUnsafe(filter.bits, i)
      let b = Belt.Array.getUnsafe(other.bits, i)
      Belt.Array.setUnsafe(filter.bits, i, a && b)
    }
    Ok()
  }
}

// ============================================================================
// Optimal Parameter Calculation
// ============================================================================

/** Calculate optimal filter size for given parameters
 *
 * expectedItems: number of items expected to be inserted
 * falsePositiveRate: desired false positive rate (0 < rate < 1)
 *
 * Formula: m = -n * ln(p) / (ln(2)^2)
 */
let optimalSize = (expectedItems: int, falsePositiveRate: float): result<int, bloomError> => {
  if expectedItems <= 0 {
    Error(InvalidSize)
  } else if falsePositiveRate <= 0.0 || falsePositiveRate >= 1.0 {
    Error(InvalidFalsePositiveRate)
  } else {
    let n = Belt.Int.toFloat(expectedItems)
    let ln2Squared = 0.4804530139182014 // ln(2)^2
    let m = -.n *. Js.Math.log(falsePositiveRate) /. ln2Squared
    Ok(Js.Math.ceil_int(m))
  }
}

/** Calculate optimal number of hash functions
 *
 * filterSize: size of the bloom filter in bits
 * expectedItems: number of items expected to be inserted
 *
 * Formula: k = (m/n) * ln(2)
 */
let optimalHashes = (filterSize: int, expectedItems: int): result<int, bloomError> => {
  if filterSize <= 0 {
    Error(InvalidSize)
  } else if expectedItems <= 0 {
    Error(InvalidSize)
  } else {
    let m = Belt.Int.toFloat(filterSize)
    let n = Belt.Int.toFloat(expectedItems)
    let ln2 = 0.6931471805599453
    let k = m /. n *. ln2
    Ok(max(1, Js.Math.ceil_int(k)))
  }
}

/** Create an optimally-sized Bloom filter for expected items and false positive rate */
let makeOptimal = (expectedItems: int, falsePositiveRate: float): result<bloomFilter, bloomError> => {
  switch optimalSize(expectedItems, falsePositiveRate) {
  | Error(e) => Error(e)
  | Ok(size) =>
    switch optimalHashes(size, expectedItems) {
    | Error(e) => Error(e)
    | Ok(hashes) => make(size, hashes)
    }
  }
}

// ============================================================================
// FFI bindings to Zig (for WASM integration)
// ============================================================================

/** Status codes from Zig FFI */
type ffiStatus = {
  status: int,
  value: int,
}

/** Status with bool value from Zig FFI */
type ffiStatusBool = {
  status: int,
  value: bool,
}

/** FFI binding to Zig bloom filter operations */
@module("proven") external ffiBloomInsert: (int, int, int) => int = "proven_bloom_insert"
@module("proven") external ffiBloomContains: (int, int, int) => ffiStatusBool = "proven_bloom_contains"
@module("proven") external ffiBloomCountOnes: int => int = "proven_bloom_count_ones"
@module("proven") external ffiBloomFillRatio: int => float = "proven_bloom_fill_ratio"
@module("proven") external ffiBloomEstimatedFPR: int => float = "proven_bloom_estimated_fpr"
@module("proven") external ffiBloomClear: int => unit = "proven_bloom_clear"
@module("proven") external ffiBloomUnion: (int, int) => int = "proven_bloom_union"
@module("proven") external ffiBloomIntersect: (int, int) => int = "proven_bloom_intersect"
