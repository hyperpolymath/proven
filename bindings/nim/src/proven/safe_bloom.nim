# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe Bloom filter implementation.

import std/[options, hashes, math]

type
  BloomFilter* = object
    ## Bloom filter with configurable size and hash count.
    bits: seq[bool]
    size: int
    numHashes: int

proc optimalSize*(expectedItems: int, falsePositiveRate: float64): int =
  ## Calculate optimal size for given parameters.
  let n = expectedItems.float64
  let p = falsePositiveRate
  let ln2Sq = 0.4804530139182014  # ln(2)^2
  let m = -(n * ln(p)) / ln2Sq
  ceil(m).int

proc optimalHashes*(filterSize, expectedItems: int): int =
  ## Calculate optimal hash count.
  let m = filterSize.float64
  let n = expectedItems.float64
  let k = (m / n) * 0.6931471805599453  # ln(2)
  max(1, ceil(k).int)

proc newBloomFilter*(size, numHashes: int): BloomFilter =
  ## Create a new Bloom filter with the given size and hash count.
  BloomFilter(
    bits: newSeq[bool](size),
    size: size,
    numHashes: numHashes
  )

proc newBloomFilterWithRate*(expectedItems: int, falsePositiveRate: float64): BloomFilter =
  ## Create with optimal parameters for expected items and false positive rate.
  let size = optimalSize(expectedItems, falsePositiveRate)
  let numHashes = optimalHashes(size, expectedItems)
  newBloomFilter(size, numHashes)

proc computeHash(bf: BloomFilter, value: string, seed: int): int =
  ## Compute hash for a value with a seed.
  let h1 = hash(value)
  let h2 = hash(seed)
  abs((h1 + seed * h2)) mod bf.size

proc insert*[T](bf: var BloomFilter, item: T) =
  ## Insert an item into the filter.
  let s = $item
  for i in 0..<bf.numHashes:
    let h = bf.computeHash(s, i)
    bf.bits[h] = true

proc contains*[T](bf: BloomFilter, item: T): bool =
  ## Check if an item might be in the filter.
  ## Returns false if definitely not present, true if possibly present.
  let s = $item
  for i in 0..<bf.numHashes:
    let h = bf.computeHash(s, i)
    if not bf.bits[h]:
      return false
  true

proc countOnes*(bf: BloomFilter): int =
  ## Count set bits.
  result = 0
  for b in bf.bits:
    if b:
      result += 1

proc fillRatio*(bf: BloomFilter): float64 =
  ## Get fill ratio.
  bf.countOnes().float64 / bf.size.float64

proc estimatedFpr*(bf: BloomFilter): float64 =
  ## Estimate false positive rate.
  pow(bf.fillRatio(), bf.numHashes.float64)

proc clear*(bf: var BloomFilter) =
  ## Clear the filter.
  for i in 0..<bf.size:
    bf.bits[i] = false

proc unionWith*(bf: var BloomFilter, other: BloomFilter) =
  ## Union of two filters (must be same size).
  if bf.size == other.size:
    for i in 0..<bf.size:
      bf.bits[i] = bf.bits[i] or other.bits[i]

proc intersectWith*(bf: var BloomFilter, other: BloomFilter) =
  ## Intersection of two filters (must be same size).
  if bf.size == other.size:
    for i in 0..<bf.size:
      bf.bits[i] = bf.bits[i] and other.bits[i]

proc getSize*(bf: BloomFilter): int =
  ## Get the size of the filter.
  bf.size

proc getNumHashes*(bf: BloomFilter): int =
  ## Get the number of hash functions.
  bf.numHashes
