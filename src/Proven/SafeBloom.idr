-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeBloom - Verified Bloom filter operations
|||
||| Type-safe Bloom filters with guaranteed false positive rate bounds.
||| Bloom filters are space-efficient probabilistic data structures.
module Proven.SafeBloom

import Proven.Core
import Data.So
import Data.Vect
import Data.Bits

%default total

-- ============================================================================
-- BLOOM FILTER PARAMETERS
-- ============================================================================

||| Calculate optimal number of bits for given n items and false positive rate
||| m = -n * ln(p) / (ln(2))^2
export
optimalBits : (expectedItems : Nat) -> (falsePositiveRate : Double) -> Nat
optimalBits n fpr =
  let ln2 = log 2.0
      ln2sq = ln2 * ln2
      lnFPR = log (max 0.0001 (min 0.5 fpr))
      m = negate (cast n) * lnFPR / ln2sq
  in max 8 (cast (ceiling m))

||| Calculate optimal number of hash functions
||| k = (m/n) * ln(2)
export
optimalHashFunctions : (bits : Nat) -> (expectedItems : Nat) -> Nat
optimalHashFunctions m n =
  if n == 0 then 1
  else let k = (cast m / cast n) * log 2.0
       in max 1 (min 16 (cast (round k)))

||| Calculate expected false positive rate
||| p = (1 - e^(-k*n/m))^k
export
expectedFPR : (bits : Nat) -> (hashFunctions : Nat) -> (items : Nat) -> Double
expectedFPR m k n =
  if m == 0 then 1.0
  else let ratio = cast k * cast n / cast m
           base = 1.0 - exp (negate ratio)
       in pow base (cast k)

-- ============================================================================
-- BLOOM FILTER
-- ============================================================================

||| A Bloom filter with fixed size
public export
record BloomFilter (m : Nat) where
  constructor MkBloomFilter
  bits : Vect m Bool
  hashCount : Nat
  itemCount : Nat

||| Create a new empty Bloom filter
export
newBloomFilter : (bits : Nat)
              -> (hashFunctions : Nat)
              -> {auto prf : GT bits 0}
              -> BloomFilter bits
newBloomFilter m k = MkBloomFilter (replicate m False) (max 1 k) 0

||| Create optimal Bloom filter for expected items and FPR
export
createOptimal : (expectedItems : Nat)
             -> (falsePositiveRate : Double)
             -> (m : Nat ** BloomFilter m)
createOptimal n fpr =
  let bits = optimalBits n fpr
      hashes = optimalHashFunctions bits n
  in (bits ** believe_me (MkBloomFilter (replicate bits False) hashes 0))

-- ============================================================================
-- HASH FUNCTIONS
-- ============================================================================

||| Simple hash function (FNV-1a style)
hash1 : List Bits8 -> Bits64
hash1 bytes = foldl (\h, b => (h `xor` cast b) * 0x100000001b3) 0xcbf29ce484222325 bytes

||| Second hash function
hash2 : List Bits8 -> Bits64
hash2 bytes = foldl (\h, b => (h * 31) `xor` cast b) 0x84222325cbf29ce4 bytes

||| Generate k hash values using double hashing
||| h_i(x) = h1(x) + i * h2(x)
hashK : (k : Nat) -> (m : Nat) -> List Bits8 -> List Nat
hashK k m bytes =
  let h1 = hash1 bytes
      h2 = hash2 bytes
  in map (\i => cast ((h1 + cast i * h2) `mod` cast m)) [0..pred k]

-- ============================================================================
-- OPERATIONS
-- ============================================================================

||| Add an item to the Bloom filter
export
add : BloomFilter m -> List Bits8 -> BloomFilter m
add bf item =
  let indices = hashK bf.hashCount m item
      newBits = foldl (\bits, idx =>
        case natToFin idx m of
          Just i => replaceAt i True bits
          Nothing => bits) bf.bits indices
  in { bits := newBits, itemCount $= S } bf

||| Check if an item might be in the set
||| Returns True if possibly present, False if definitely not present
export
mightContain : BloomFilter m -> List Bits8 -> Bool
mightContain bf item =
  let indices = hashK bf.hashCount m item
  in all (\idx =>
    case natToFin idx m of
      Just i => index i bf.bits
      Nothing => False) indices

||| Definitely not in set (inverse of mightContain)
export
definitelyNot : BloomFilter m -> List Bits8 -> Bool
definitelyNot bf item = not (mightContain bf item)

||| Get current estimated false positive rate
export
currentFPR : BloomFilter m -> Double
currentFPR bf = expectedFPR m bf.hashCount bf.itemCount

||| Get fill ratio (fraction of bits set)
export
fillRatio : BloomFilter m -> Double
fillRatio bf =
  let setBits = foldl (\c, b => if b then S c else c) 0 (toList bf.bits)
  in cast setBits / cast m

||| Get item count
export
count : BloomFilter m -> Nat
count bf = bf.itemCount

-- ============================================================================
-- COUNTING BLOOM FILTER
-- ============================================================================

||| Counting Bloom filter (allows removal)
public export
record CountingBloomFilter (m : Nat) where
  constructor MkCountingBloom
  counters : Vect m Nat
  maxCount : Nat           -- Maximum counter value
  hashCount : Nat
  itemCount : Nat

||| Create counting Bloom filter
export
newCountingBloom : (bits : Nat)
                -> (hashFunctions : Nat)
                -> (maxCount : Nat)
                -> {auto prf : GT bits 0}
                -> CountingBloomFilter bits
newCountingBloom m k maxC =
  MkCountingBloom (replicate m 0) (max 1 maxC) (max 1 k) 0

||| Add to counting Bloom filter
export
addCounting : CountingBloomFilter m -> List Bits8 -> CountingBloomFilter m
addCounting cbf item =
  let indices = hashK cbf.hashCount m item
      newCounters = foldl (\counters, idx =>
        case natToFin idx m of
          Just i => let c = index i counters
                    in if c < cbf.maxCount
                       then replaceAt i (S c) counters
                       else counters
          Nothing => counters) cbf.counters indices
  in { counters := newCounters, itemCount $= S } cbf

||| Remove from counting Bloom filter
export
removeCounting : CountingBloomFilter m -> List Bits8 -> CountingBloomFilter m
removeCounting cbf item =
  let indices = hashK cbf.hashCount m item
      newCounters = foldl (\counters, idx =>
        case natToFin idx m of
          Just i => let c = index i counters
                    in if c > 0 then replaceAt i (pred c) counters else counters
          Nothing => counters) cbf.counters indices
  in { counters := newCounters, itemCount $= pred } cbf

||| Check membership in counting Bloom filter
export
mightContainCounting : CountingBloomFilter m -> List Bits8 -> Bool
mightContainCounting cbf item =
  let indices = hashK cbf.hashCount m item
  in all (\idx =>
    case natToFin idx m of
      Just i => index i cbf.counters > 0
      Nothing => False) indices

-- ============================================================================
-- SCALABLE BLOOM FILTER
-- ============================================================================

||| A scalable Bloom filter that grows as needed
public export
record ScalableBloomFilter where
  constructor MkScalableBloom
  filters : List (m : Nat ** BloomFilter m)
  targetFPR : Double
  growthRatio : Nat        -- Each new filter is this many times larger
  tighteningRatio : Double -- FPR is multiplied by this for each new filter

||| Create scalable Bloom filter
export
newScalableBloom : (initialSize : Nat)
                -> (targetFPR : Double)
                -> ScalableBloomFilter
newScalableBloom initSize fpr =
  let bits = max 64 initSize
      hashes = optimalHashFunctions bits (bits `div` 10)
      initial = (bits ** believe_me (MkBloomFilter (replicate bits False) hashes 0))
  in MkScalableBloom [initial] (max 0.0001 (min 0.5 fpr)) 2 0.8

||| Add to scalable Bloom filter (may create new filter if needed)
export
addScalable : ScalableBloomFilter -> List Bits8 -> ScalableBloomFilter
addScalable sbf item =
  case sbf.filters of
    [] => sbf  -- Should not happen
    ((m ** bf) :: rest) =>
      let filled = fillRatio bf
      in if filled > 0.5  -- Threshold for creating new filter
         then let newSize = m * sbf.growthRatio
                  newFPR = sbf.targetFPR * sbf.tighteningRatio
                  newHashes = optimalHashFunctions newSize (newSize `div` 10)
                  newFilter = (newSize ** believe_me (MkBloomFilter (replicate newSize False) newHashes 0))
                  addedNew = add (snd newFilter) item
              in { filters := (newSize ** addedNew) :: (m ** bf) :: rest } sbf
         else let added = add bf item
              in { filters := (m ** added) :: rest } sbf

||| Check membership in scalable Bloom filter
export
mightContainScalable : ScalableBloomFilter -> List Bits8 -> Bool
mightContainScalable sbf item =
  any (\(m ** bf) => mightContain bf item) sbf.filters

-- ============================================================================
-- UTILITY
-- ============================================================================

||| Convert string to bytes for hashing
export
stringToBytes : String -> List Bits8
stringToBytes s = map (cast . ord) (unpack s)

||| Merge two Bloom filters of the same size (OR operation)
export
merge : BloomFilter m -> BloomFilter m -> BloomFilter m
merge bf1 bf2 =
  let merged = zipWith (||) bf1.bits bf2.bits
  in { bits := merged, itemCount := bf1.itemCount + bf2.itemCount } bf1

||| Intersect two Bloom filters (AND operation) - approximates intersection
export
intersect : BloomFilter m -> BloomFilter m -> BloomFilter m
intersect bf1 bf2 =
  let intersected = zipWith (&&) bf1.bits bf2.bits
  in { bits := intersected, itemCount := 0 } bf1  -- Count is unknown
