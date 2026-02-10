-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeBloom - Safe Bloom filter implementation
|||
||| This module provides a probabilistic set membership data structure
||| with guaranteed no false negatives.
module Proven.SafeBloom
import Data.String
import Data.List

import public Proven.Core
import Data.Vect
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- Bloom Filter Type
--------------------------------------------------------------------------------

||| A Bloom filter with fixed size
public export
record BloomFilter where
  constructor MkBloom
  bits : List Bool
  size : Nat
  numHashes : Nat

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create an empty Bloom filter
public export
empty : (size : Nat) -> (numHashes : Nat) -> BloomFilter
empty sz nh = MkBloom (replicate sz False) sz nh

||| Calculate optimal size for given expected items and false positive rate
public export
optimalSize : (expectedItems : Nat) -> (falsePositiveRate : Double) -> Nat
optimalSize n p =
  let ln2sq = 0.4804530139182014  -- ln(2)^2
      m = negate (cast n * log p) / ln2sq
  in cast (ceiling m)

||| Calculate optimal number of hash functions
public export
optimalHashes : (filterSize : Nat) -> (expectedItems : Nat) -> Nat
optimalHashes m n =
  let k = (cast m / cast n) * 0.6931471805599453  -- ln(2)
  in max 1 (cast (ceiling k))

||| Create a Bloom filter with optimal parameters
public export
withRate : (expectedItems : Nat) -> (falsePositiveRate : Double) -> BloomFilter
withRate n p =
  let sz = optimalSize n p
      nh = optimalHashes sz n
  in empty sz nh

--------------------------------------------------------------------------------
-- Hash Functions
--------------------------------------------------------------------------------

||| Simple hash function for strings
simpleHash : Nat -> String -> Nat
simpleHash seed s = go seed (unpack s)
  where
    go : Nat -> List Char -> Nat
    go h [] = h
    go h (c :: cs) =
      let h' = (h * 31 + cast (ord c)) `mod` 0xFFFFFFFF
      in go h' cs

||| Generate hash indices for a value
hashIndices : Nat -> Nat -> String -> List Nat
hashIndices size numHashes value = map (\i => simpleHash i value `mod` size) [0..minus numHashes 1]

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

||| Insert a string into the Bloom filter
public export
insert : String -> BloomFilter -> BloomFilter
insert value bf =
  let indices = hashIndices bf.size bf.numHashes value
      newBits = foldl (\bits, idx => setAt idx bits) bf.bits indices
  in MkBloom newBits bf.size bf.numHashes
  where
    setAt : Nat -> List Bool -> List Bool
    setAt _ [] = []
    setAt Z (_ :: xs) = True :: xs
    setAt (S n) (x :: xs) = x :: setAt n xs

||| Check if a value might be in the filter
||| Returns False if definitely not present, True if possibly present
public export
isInfixOf : String -> BloomFilter -> Bool
isInfixOf value bf =
  let indices = hashIndices bf.size bf.numHashes value
  in all (\idx => getAt idx bf.bits) indices
  where
    getAt : Nat -> List Bool -> Bool
    getAt _ [] = False
    getAt Z (x :: _) = x
    getAt (S n) (_ :: xs) = getAt n xs

||| Count the number of bits set
public export
countOnes : BloomFilter -> Nat
countOnes bf = length (filter id bf.bits)

||| Estimate the current false positive rate
public export
estimatedFalsePositiveRate : BloomFilter -> Double
estimatedFalsePositiveRate bf =
  let k = cast bf.numHashes
      m = cast bf.size
      ones = cast (countOnes bf)
  in pow (ones / m) k

||| Union of two Bloom filters (must have same parameters)
public export
union : BloomFilter -> BloomFilter -> Maybe BloomFilter
union a b =
  if a.size /= b.size || a.numHashes /= b.numHashes
    then Nothing
    else Just (MkBloom (zipWith (||) a.bits b.bits) a.size a.numHashes)

||| Intersection of two Bloom filters
public export
intersection : BloomFilter -> BloomFilter -> Maybe BloomFilter
intersection a b =
  if a.size /= b.size || a.numHashes /= b.numHashes
    then Nothing
    else Just (MkBloom (zipWith (&&) a.bits b.bits) a.size a.numHashes)

||| Clear the Bloom filter
public export
clear : BloomFilter -> BloomFilter
clear bf = MkBloom (replicate bf.size False) bf.size bf.numHashes

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

||| Get the fill ratio (percentage of bits set)
public export
fillRatio : BloomFilter -> Double
fillRatio bf = cast (countOnes bf) / cast bf.size

||| Check if filter is saturated (all bits set)
public export
isSaturated : BloomFilter -> Bool
isSaturated bf = countOnes bf == bf.size

public export
Show BloomFilter where
  show bf = "BloomFilter(size=" ++ show bf.size ++
            ", hashes=" ++ show bf.numHashes ++
            ", fill=" ++ show (fillRatio bf * 100.0) ++ "%)"
