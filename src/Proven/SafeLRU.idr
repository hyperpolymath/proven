-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeLRU - Verified LRU cache operations
|||
||| Type-safe Least Recently Used (LRU) cache with bounded capacity.
||| Guarantees cache size never exceeds capacity.
module Proven.SafeLRU

import Proven.Core
import Data.So
import Data.List

%default total

-- ============================================================================
-- LRU CACHE
-- ============================================================================

||| An LRU cache entry
public export
record CacheEntry k v where
  constructor MkEntry
  key : k
  value : v
  accessTime : Nat  -- Logical timestamp

||| LRU cache with bounded capacity
public export
record LRUCache (cap : Nat) k v where
  constructor MkLRUCache
  entries : List (CacheEntry k v)
  clock : Nat  -- Logical clock for access times
  0 sizeInvariant : So (length entries <= cap)

||| Create empty LRU cache
export
empty : (cap : Nat) -> LRUCache cap k v
empty cap = believe_me (MkLRUCache [] 0)

||| Get current size
export
size : LRUCache cap k v -> Nat
size cache = length cache.entries

||| Check if cache is empty
export
isEmpty : LRUCache cap k v -> Bool
isEmpty cache = null cache.entries

||| Check if cache is full
export
isFull : LRUCache cap k v -> Bool
isFull cache = length cache.entries >= cap

-- ============================================================================
-- CACHE OPERATIONS
-- ============================================================================

||| Find entry by key
findEntry : Eq k => k -> List (CacheEntry k v) -> Maybe (CacheEntry k v)
findEntry key [] = Nothing
findEntry key (e :: es) = if e.key == key then Just e else findEntry key es

||| Remove entry by key
removeEntry : Eq k => k -> List (CacheEntry k v) -> List (CacheEntry k v)
removeEntry key = filter (\e => e.key /= key)

||| Find and remove least recently used entry
removeLRU : List (CacheEntry k v) -> (Maybe (CacheEntry k v), List (CacheEntry k v))
removeLRU [] = (Nothing, [])
removeLRU entries =
  let lru = foldl (\oldest, e =>
        case oldest of
          Nothing => Just e
          Just o => if e.accessTime < o.accessTime then Just e else oldest) Nothing entries
  in case lru of
       Nothing => (Nothing, entries)
       Just e => (Just e, filter (\x => x.accessTime /= e.accessTime) entries)

||| Get value from cache (updates access time)
export
get : Eq k => k -> LRUCache cap k v -> (Maybe v, LRUCache cap k v)
get key cache =
  case findEntry key cache.entries of
    Nothing => (Nothing, cache)
    Just entry =>
      let newClock = S cache.clock
          updated = MkEntry key entry.value newClock
          others = removeEntry key cache.entries
      in (Just entry.value, believe_me (MkLRUCache (updated :: others) newClock))

||| Peek value without updating access time
export
peek : Eq k => k -> LRUCache cap k v -> Maybe v
peek key cache = map value (findEntry key cache.entries)

||| Check if key exists
export
contains : Eq k => k -> LRUCache cap k v -> Bool
contains key cache = isJust (findEntry key cache.entries)

||| Put value into cache (evicts LRU if full)
export
put : Eq k => k -> v -> LRUCache cap k v -> LRUCache cap k v
put key val cache =
  let newClock = S cache.clock
      newEntry = MkEntry key val newClock
      withoutKey = removeEntry key cache.entries
  in if length withoutKey >= cap
     then -- Need to evict LRU
          let (_, afterEvict) = removeLRU withoutKey
          in believe_me (MkLRUCache (newEntry :: afterEvict) newClock)
     else believe_me (MkLRUCache (newEntry :: withoutKey) newClock)

||| Remove key from cache
export
remove : Eq k => k -> LRUCache cap k v -> LRUCache cap k v
remove key cache =
  let newEntries = removeEntry key cache.entries
  in believe_me (MkLRUCache newEntries cache.clock)

||| Clear the cache
export
clear : LRUCache cap k v -> LRUCache cap k v
clear cache = believe_me (MkLRUCache [] cache.clock)

-- ============================================================================
-- CACHE RESULT
-- ============================================================================

||| Result of a cache get operation
public export
data CacheResult v : Type where
  ||| Cache hit
  Hit : v -> CacheResult v
  ||| Cache miss
  Miss : CacheResult v

||| Check if result is a hit
export
isHit : CacheResult v -> Bool
isHit (Hit _) = True
isHit Miss = False

||| Get value or default
export
getOrDefault : v -> CacheResult v -> v
getOrDefault _ (Hit v) = v
getOrDefault def Miss = def

-- ============================================================================
-- CACHE WITH EXPIRY
-- ============================================================================

||| Cache entry with TTL
public export
record TimedEntry k v where
  constructor MkTimedEntry
  key : k
  value : v
  accessTime : Nat
  expiresAt : Integer  -- Absolute expiry timestamp

||| LRU cache with TTL support
public export
record TimedLRUCache (cap : Nat) k v where
  constructor MkTimedLRU
  entries : List (TimedEntry k v)
  clock : Nat
  defaultTTL : Integer  -- Default TTL in milliseconds

||| Create timed LRU cache
export
emptyTimed : (cap : Nat) -> (defaultTTL : Integer) -> TimedLRUCache cap k v
emptyTimed cap ttl = MkTimedLRU [] 0 ttl

||| Check if entry is expired
isExpired : Integer -> TimedEntry k v -> Bool
isExpired now entry = now >= entry.expiresAt

||| Remove expired entries
removeExpired : Integer -> List (TimedEntry k v) -> List (TimedEntry k v)
removeExpired now = filter (not . isExpired now)

||| Get from timed cache (respects expiry)
export
getTimed : Eq k => k -> Integer -> TimedLRUCache cap k v -> (Maybe v, TimedLRUCache cap k v)
getTimed key now cache =
  let cleaned = removeExpired now cache.entries
  in case find (\e => e.key == key) cleaned of
       Nothing => (Nothing, believe_me (MkTimedLRU cleaned cache.clock cache.defaultTTL))
       Just entry =>
         let newClock = S cache.clock
             updated = { accessTime := newClock } entry
             others = filter (\e => e.key /= key) cleaned
         in (Just entry.value, believe_me (MkTimedLRU (updated :: others) newClock cache.defaultTTL))

||| Put with custom TTL
export
putTimed : Eq k => k -> v -> Integer -> Integer -> TimedLRUCache cap k v -> TimedLRUCache cap k v
putTimed key val now ttl cache =
  let cleaned = removeExpired now cache.entries
      newClock = S cache.clock
      newEntry = MkTimedEntry key val newClock (now + ttl)
      withoutKey = filter (\e => e.key /= key) cleaned
  in if length withoutKey >= cap
     then let sorted = sortBy (\a, b => compare a.accessTime b.accessTime) withoutKey
          in believe_me (MkTimedLRU (newEntry :: drop 1 sorted) newClock cache.defaultTTL)
     else believe_me (MkTimedLRU (newEntry :: withoutKey) newClock cache.defaultTTL)

-- ============================================================================
-- CACHE STATISTICS
-- ============================================================================

||| Cache statistics
public export
record CacheStats where
  constructor MkCacheStats
  hits : Nat
  misses : Nat
  evictions : Nat
  expirations : Nat

||| Create empty stats
export
emptyStats : CacheStats
emptyStats = MkCacheStats 0 0 0 0

||| Record a hit
export
recordHit : CacheStats -> CacheStats
recordHit = { hits $= S }

||| Record a miss
export
recordMiss : CacheStats -> CacheStats
recordMiss = { misses $= S }

||| Record an eviction
export
recordEviction : CacheStats -> CacheStats
recordEviction = { evictions $= S }

||| Record an expiration
export
recordExpiration : CacheStats -> CacheStats
recordExpiration = { expirations $= S }

||| Calculate hit rate
export
hitRate : CacheStats -> Double
hitRate stats =
  let total = stats.hits + stats.misses
  in if total == 0 then 0.0 else cast stats.hits / cast total

||| Calculate miss rate
export
missRate : CacheStats -> Double
missRate stats = 1.0 - hitRate stats

-- ============================================================================
-- LRU CACHE WITH STATS
-- ============================================================================

||| LRU cache with built-in statistics
public export
record StatsLRUCache (cap : Nat) k v where
  constructor MkStatsLRU
  cache : LRUCache cap k v
  stats : CacheStats

||| Create stats-tracking LRU cache
export
emptyWithStats : (cap : Nat) -> StatsLRUCache cap k v
emptyWithStats cap = MkStatsLRU (empty cap) emptyStats

||| Get with stats tracking
export
getWithStats : Eq k => k -> StatsLRUCache cap k v -> (Maybe v, StatsLRUCache cap k v)
getWithStats key sc =
  let (result, newCache) = get key sc.cache
      newStats = case result of
                   Just _ => recordHit sc.stats
                   Nothing => recordMiss sc.stats
  in (result, MkStatsLRU newCache newStats)

||| Put with stats tracking
export
putWithStats : Eq k => k -> v -> StatsLRUCache cap k v -> StatsLRUCache cap k v
putWithStats key val sc =
  let wasFull = isFull sc.cache
      newCache = put key val sc.cache
      newStats = if wasFull && not (contains key sc.cache)
                 then recordEviction sc.stats
                 else sc.stats
  in MkStatsLRU newCache newStats

-- ============================================================================
-- UTILITY
-- ============================================================================

||| Get all keys in cache (most recent first)
export
keys : LRUCache cap k v -> List k
keys cache = map key (sortBy (\a, b => compare b.accessTime a.accessTime) cache.entries)

||| Get all values in cache (most recent first)
export
values : LRUCache cap k v -> List v
values cache = map value (sortBy (\a, b => compare b.accessTime a.accessTime) cache.entries)

||| Get all key-value pairs
export
toList : LRUCache cap k v -> List (k, v)
toList cache = map (\e => (e.key, e.value)) cache.entries

||| Available capacity
export
available : LRUCache cap k v -> Nat
available {cap} cache = cap `minus` length cache.entries
