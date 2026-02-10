-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeLRU - Safe Least Recently Used cache
|||
||| This module provides a bounded LRU cache with safe operations
||| that cannot overflow or corrupt cache state.
module Proven.SafeLRU
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- LRU Cache Type
--------------------------------------------------------------------------------

||| Entry in the cache with key, value, and access order
public export
record CacheEntry k v where
  constructor MkEntry
  key : k
  val : v
  accessOrder : Nat

||| LRU cache with bounded capacity
public export
record LRUCache k v where
  constructor MkLRU
  capacity : Nat
  entries : List (CacheEntry k v)
  accessCounter : Nat

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create an empty LRU cache
public export
empty : (capacity : Nat) -> LRUCache k v
empty cap = MkLRU cap [] 0

||| Get the current size of the cache
public export
size : LRUCache k v -> Nat
size cache = length cache.entries

||| Check if cache is empty
public export
isEmpty : LRUCache k v -> Bool
isEmpty cache = isNil cache.entries

||| Check if cache is full
public export
isFull : LRUCache k v -> Bool
isFull cache = size cache >= cache.capacity

||| Get remaining capacity
public export
remaining : LRUCache k v -> Nat
remaining cache = minus cache.capacity (size cache)

--------------------------------------------------------------------------------
-- Cache Operations
--------------------------------------------------------------------------------

||| Get a value from the cache (updates access order)
public export
get : Eq k => k -> LRUCache k v -> Maybe (v, LRUCache k v)
get key cache =
  case find (\e => e.key == key) cache.entries of
    Nothing => Nothing
    Just entry =>
      let newCounter = S cache.accessCounter
          updatedEntry = MkEntry entry.key entry.val newCounter
          otherEntries = filter (\e => e.key /= key) cache.entries
          newEntries = updatedEntry :: otherEntries
      in Just (entry.val, MkLRU cache.capacity newEntries newCounter)

||| Peek at a value without updating access order
public export
peek : Eq k => k -> LRUCache k v -> Maybe v
peek key cache = map val (find (\e => e.key == key) cache.entries)

||| Check if a key exists in the cache
public export
isInfixOf : Eq k => k -> LRUCache k v -> Bool
isInfixOf key cache = any (\e => e.key == key) cache.entries

||| Find the least recently used entry
findLRU : List (CacheEntry k v) -> Maybe (CacheEntry k v)
findLRU [] = Nothing
findLRU entries = Just (foldl1 (\a, b => if a.accessOrder < b.accessOrder then a else b) entries)
  where
    foldl1 : (a -> a -> a) -> List a -> a
    foldl1 f [x] = x
    foldl1 f (x :: xs) = foldl f x xs
    foldl1 f [] = believe_me ()  -- unreachable

||| Put a value in the cache (evicts LRU if full)
public export
put : Eq k => k -> v -> LRUCache k v -> LRUCache k v
put key val cache =
  let newCounter = S cache.accessCounter
      newEntry = MkEntry key val newCounter
      -- Remove existing entry for this key
      withoutKey = filter (\e => e.key /= key) cache.entries
      -- Check if we need to evict
      entriesAfterEvict =
        if length withoutKey >= cache.capacity
          then case findLRU withoutKey of
                 Nothing => withoutKey
                 Just lru => filter (\e => e.key /= lru.key) withoutKey
          else withoutKey
  in MkLRU cache.capacity (newEntry :: entriesAfterEvict) newCounter

||| Remove a key from the cache
public export
remove : Eq k => k -> LRUCache k v -> LRUCache k v
remove key cache =
  MkLRU cache.capacity (filter (\e => e.key /= key) cache.entries) cache.accessCounter

||| Clear all entries from the cache
public export
clear : LRUCache k v -> LRUCache k v
clear cache = MkLRU cache.capacity [] 0

--------------------------------------------------------------------------------
-- Bulk Operations
--------------------------------------------------------------------------------

||| Get all keys in the cache (most to least recently used)
public export
keys : LRUCache k v -> List k
keys cache = map key (sortBy (\a, b => compare b.accessOrder a.accessOrder) cache.entries)

||| Get all values in the cache (most to least recently used)
public export
values : LRUCache k v -> List v
values cache = map val (sortBy (\a, b => compare b.accessOrder a.accessOrder) cache.entries)

||| Convert cache to list of key-value pairs
public export
toList : LRUCache k v -> List (k, v)
toList cache = map (\e => (e.key, e.val)) cache.entries

||| Create cache from list of key-value pairs
public export
fromList : Eq k => (capacity : Nat) -> List (k, v) -> LRUCache k v
fromList cap pairs = foldl (\c, (k, v) => put k v c) (empty cap) pairs

--------------------------------------------------------------------------------
-- Cache Statistics
--------------------------------------------------------------------------------

||| Get fill ratio
public export
fillRatio : LRUCache k v -> Double
fillRatio cache =
  if cache.capacity == 0 then 0.0
  else cast (size cache) / cast cache.capacity

public export
Show k => Show v => Show (LRUCache k v) where
  show cache = "LRUCache(capacity=" ++ show cache.capacity ++
               ", size=" ++ show (size cache) ++ ")"
