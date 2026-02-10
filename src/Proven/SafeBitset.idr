-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeBitset - Safe bit manipulation operations
|||
||| This module provides bounded bitset operations with compile-time
||| and runtime bounds checking for safe bit manipulation.
module Proven.SafeBitset
import Data.String
import Data.List

import public Proven.Core
import Data.Bits
import Data.List

%default total

--------------------------------------------------------------------------------
-- Bitset Type
--------------------------------------------------------------------------------

||| A bitset with a fixed capacity (number of bits)
public export
record Bitset where
  constructor MkBitset
  capacity : Nat  -- Maximum number of bits
  bits : Integer  -- Bit storage (arbitrary precision)

||| Bitset errors
public export
data BitsetError
  = IndexOutOfBounds Nat Nat  -- index, capacity
  | InvalidRange Nat Nat      -- start, end
  | CapacityMismatch Nat Nat  -- expected, actual

public export
Show BitsetError where
  show (IndexOutOfBounds i c) =
    "Bit index " ++ show i ++ " out of bounds (capacity: " ++ show c ++ ")"
  show (InvalidRange s e) =
    "Invalid range [" ++ show s ++ ", " ++ show e ++ ")"
  show (CapacityMismatch e a) =
    "Capacity mismatch: expected " ++ show e ++ ", got " ++ show a

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create an empty bitset with given capacity
public export
empty : (capacity : Nat) -> Bitset
empty cap = MkBitset cap 0

||| Create a full bitset (all bits set)
public export
full : (capacity : Nat) -> Bitset
full cap = MkBitset cap (shiftL 1 (cast cap) - 1)

||| Create a bitset from an Integer
public export
fromInteger : (capacity : Nat) -> Integer -> Bitset
fromInteger cap n =
  let mask = shiftL 1 (cast cap) - 1
  in MkBitset cap (n .&. mask)

||| Create a bitset with a single bit set
public export
singleton : (capacity : Nat) -> (bit : Nat) -> Maybe Bitset
singleton cap bit =
  if bit < cap
    then Just (MkBitset cap (shiftL 1 (cast bit)))
    else Nothing

||| Create from a list of bit indices
public export
fromList : (capacity : Nat) -> List Nat -> Either BitsetError Bitset
fromList cap indices =
  case find (>= cap) indices of
    Just i => Left (IndexOutOfBounds i cap)
    Nothing =>
      let bits = foldl (\acc, i => acc .|. shiftL 1 (cast i)) 0 indices
      in Right (MkBitset cap bits)

||| Convert to a list of set bit indices
public export
toList : Bitset -> List Nat
toList bs = filter (test bs) [0 .. minus bs.capacity 1]
  where
    test : Bitset -> Nat -> Bool
    test bs i = testBit bs.bits (cast i)

--------------------------------------------------------------------------------
-- Single Bit Operations
--------------------------------------------------------------------------------

||| Set a bit (bounds checked)
public export
set : Nat -> Bitset -> Either BitsetError Bitset
set i bs =
  if i >= bs.capacity
    then Left (IndexOutOfBounds i bs.capacity)
    else Right (MkBitset bs.capacity (bs.bits .|. shiftL 1 (cast i)))

||| Clear a bit (bounds checked)
public export
clear : Nat -> Bitset -> Either BitsetError Bitset
clear i bs =
  if i >= bs.capacity
    then Left (IndexOutOfBounds i bs.capacity)
    else Right (MkBitset bs.capacity (bs.bits .&. complement (shiftL 1 (cast i))))

||| Toggle a bit (bounds checked)
public export
toggle : Nat -> Bitset -> Either BitsetError Bitset
toggle i bs =
  if i >= bs.capacity
    then Left (IndexOutOfBounds i bs.capacity)
    else Right (MkBitset bs.capacity (bs.bits `xor` shiftL 1 (cast i)))

||| Test if a bit is set (bounds checked)
public export
test : Nat -> Bitset -> Either BitsetError Bool
test i bs =
  if i >= bs.capacity
    then Left (IndexOutOfBounds i bs.capacity)
    else Right (testBit bs.bits (cast i))

||| Unchecked set (use only when index is known valid)
public export
unsafeSet : Nat -> Bitset -> Bitset
unsafeSet i bs = MkBitset bs.capacity (bs.bits .|. shiftL 1 (cast i))

||| Unchecked clear
public export
unsafeClear : Nat -> Bitset -> Bitset
unsafeClear i bs = MkBitset bs.capacity (bs.bits .&. complement (shiftL 1 (cast i)))

||| Unchecked test
public export
unsafeTest : Nat -> Bitset -> Bool
unsafeTest i bs = testBit bs.bits (cast i)

--------------------------------------------------------------------------------
-- Bitwise Operations
--------------------------------------------------------------------------------

||| Bitwise AND (requires same capacity)
public export
and : Bitset -> Bitset -> Either BitsetError Bitset
and bs1 bs2 =
  if bs1.capacity /= bs2.capacity
    then Left (CapacityMismatch bs1.capacity bs2.capacity)
    else Right (MkBitset bs1.capacity (bs1.bits .&. bs2.bits))

||| Bitwise OR (requires same capacity)
public export
or : Bitset -> Bitset -> Either BitsetError Bitset
or bs1 bs2 =
  if bs1.capacity /= bs2.capacity
    then Left (CapacityMismatch bs1.capacity bs2.capacity)
    else Right (MkBitset bs1.capacity (bs1.bits .|. bs2.bits))

||| Bitwise XOR (requires same capacity)
public export
xor : Bitset -> Bitset -> Either BitsetError Bitset
xor bs1 bs2 =
  if bs1.capacity /= bs2.capacity
    then Left (CapacityMismatch bs1.capacity bs2.capacity)
    else Right (MkBitset bs1.capacity (bs1.bits `xor` bs2.bits))

||| Bitwise NOT (complement)
public export
not : Bitset -> Bitset
not bs =
  let mask = shiftL 1 (cast bs.capacity) - 1
  in MkBitset bs.capacity (complement bs.bits .&. mask)

||| Bitwise AND-NOT (difference)
public export
andNot : Bitset -> Bitset -> Either BitsetError Bitset
andNot bs1 bs2 =
  if bs1.capacity /= bs2.capacity
    then Left (CapacityMismatch bs1.capacity bs2.capacity)
    else Right (MkBitset bs1.capacity (bs1.bits .&. complement bs2.bits))

--------------------------------------------------------------------------------
-- Range Operations
--------------------------------------------------------------------------------

||| Set a range of bits [start, end)
public export
setRange : Nat -> Nat -> Bitset -> Either BitsetError Bitset
setRange start end bs =
  if start > end then Left (InvalidRange start end)
  else if end > bs.capacity then Left (IndexOutOfBounds end bs.capacity)
  else
    let mask = (shiftL 1 (cast (minus end start)) - 1) `shiftL` cast start
    in Right (MkBitset bs.capacity (bs.bits .|. mask))

||| Clear a range of bits [start, end)
public export
clearRange : Nat -> Nat -> Bitset -> Either BitsetError Bitset
clearRange start end bs =
  if start > end then Left (InvalidRange start end)
  else if end > bs.capacity then Left (IndexOutOfBounds end bs.capacity)
  else
    let mask = (shiftL 1 (cast (minus end start)) - 1) `shiftL` cast start
    in Right (MkBitset bs.capacity (bs.bits .&. complement mask))

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

||| Count number of set bits (population count)
public export
popCount : Bitset -> Nat
popCount bs = countBits bs.bits 0
  where
    countBits : Integer -> Nat -> Nat
    countBits 0 acc = acc
    countBits n acc = countBits (n .&. (n - 1)) (S acc)

||| Count number of clear bits
public export
clearCount : Bitset -> Nat
clearCount bs = minus bs.capacity (popCount bs)

||| Check if bitset is empty (no bits set)
public export
isEmpty : Bitset -> Bool
isEmpty bs = bs.bits == 0

||| Check if bitset is full (all bits set)
public export
isFull : Bitset -> Bool
isFull bs = bs.bits == shiftL 1 (cast bs.capacity) - 1

||| Check if any bit is set
public export
any : Bitset -> Bool
any bs = bs.bits /= 0

||| Check if all bits are set
public export
all : Bitset -> Bool
all = isFull

||| Check if no bits are set
public export
none : Bitset -> Bool
none = isEmpty

||| Get capacity
public export
capacity : Bitset -> Nat
capacity = capacity

||| Find first set bit (least significant)
public export
firstSet : Bitset -> Maybe Nat
firstSet bs =
  if bs.bits == 0 then Nothing
  else Just (findFirst bs.bits 0)
  where
    findFirst : Integer -> Nat -> Nat
    findFirst n i =
      if testBit n 0 then i
      else findFirst (shiftR n 1) (S i)

||| Find last set bit (most significant)
public export
lastSet : Bitset -> Maybe Nat
lastSet bs =
  if bs.bits == 0 then Nothing
  else Just (findLast bs.bits 0)
  where
    findLast : Integer -> Nat -> Nat
    findLast 0 i = minus i 1
    findLast 1 i = i
    findLast n i = findLast (shiftR n 1) (S i)

||| Find first clear bit
public export
firstClear : Bitset -> Maybe Nat
firstClear bs = firstSet (not bs)

--------------------------------------------------------------------------------
-- Set Operations (treating as sets)
--------------------------------------------------------------------------------

||| Check if bs1 is a subset of bs2
public export
isSubsetOf : Bitset -> Bitset -> Either BitsetError Bool
isSubsetOf bs1 bs2 =
  if bs1.capacity /= bs2.capacity
    then Left (CapacityMismatch bs1.capacity bs2.capacity)
    else Right (bs1.bits .&. bs2.bits == bs1.bits)

||| Check if bitsets are disjoint (no common bits)
public export
isDisjoint : Bitset -> Bitset -> Either BitsetError Bool
isDisjoint bs1 bs2 =
  if bs1.capacity /= bs2.capacity
    then Left (CapacityMismatch bs1.capacity bs2.capacity)
    else Right (bs1.bits .&. bs2.bits == 0)

||| Check if bitsets intersect (have common bits)
public export
intersects : Bitset -> Bitset -> Either BitsetError Bool
intersects bs1 bs2 =
  if bs1.capacity /= bs2.capacity
    then Left (CapacityMismatch bs1.capacity bs2.capacity)
    else Right (bs1.bits .&. bs2.bits /= 0)

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

||| Convert to Integer
public export
toInteger : Bitset -> Integer
toInteger = bits

||| Convert to binary string
public export
toBinaryString : Bitset -> String
toBinaryString bs = pack (reverse (toBits bs.bits bs.capacity))
  where
    toBits : Integer -> Nat -> List Char
    toBits _ Z = []
    toBits n (S k) = (if testBit n 0 then '1' else '0') :: toBits (shiftR n 1) k

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Eq Bitset where
  (==) bs1 bs2 = bs1.capacity == bs2.capacity && bs1.bits == bs2.bits

public export
Show Bitset where
  show bs = "Bitset(" ++ toBinaryString bs ++ ")"
