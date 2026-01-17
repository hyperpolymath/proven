// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeLru - LRU (Least Recently Used) cache that cannot crash.
 *
 * Provides a safe bounded LRU cache implementation with automatic eviction
 * of least recently used entries when capacity is exceeded. All operations
 * return Result or Option types to handle edge cases safely.
 *
 * Key properties:
 * - Fixed capacity with automatic eviction
 * - O(n) lookup (for simplicity; use Map-based version for O(1))
 * - get() updates access order; peek() does not
 * - Thread-safe for single-threaded JS environments
 */

// ============================================================================
// Error types
// ============================================================================

/** Error types for LRU cache operations */
type lruError =
  | InvalidCapacity
  | KeyNotFound

/** Convert error to human-readable string */
let errorToString = (error: lruError): string => {
  switch error {
  | InvalidCapacity => "Invalid cache capacity"
  | KeyNotFound => "Key not found in cache"
  }
}

// ============================================================================
// LRU Cache Implementation
// ============================================================================

/** An entry in the LRU cache */
type lruEntry<'k, 'v> = {
  key: 'k,
  value: 'v,
  mutable accessOrder: int,
}

/** An LRU cache with bounded capacity */
type lruCache<'k, 'v> = {
  mutable entries: array<option<lruEntry<'k, 'v>>>,
  mutable counter: int,
  mutable length: int,
  capacity: int,
}

/** Create a new LRU cache with given capacity */
let make = (capacity: int): result<lruCache<'k, 'v>, lruError> => {
  if capacity <= 0 {
    Error(InvalidCapacity)
  } else {
    Ok({
      entries: Belt.Array.make(capacity, None),
      counter: 0,
      length: 0,
      capacity,
    })
  }
}

/** Get the current number of entries in the cache */
let size = (cache: lruCache<'k, 'v>): int => {
  cache.length
}

/** Check if the cache is empty */
let isEmpty = (cache: lruCache<'k, 'v>): bool => {
  cache.length == 0
}

/** Check if the cache is full */
let isFull = (cache: lruCache<'k, 'v>): bool => {
  cache.length >= cache.capacity
}

/** Get the capacity of the cache */
let getCapacity = (cache: lruCache<'k, 'v>): int => {
  cache.capacity
}

/** Get the fill ratio (0.0 to 1.0) */
let fillRatio = (cache: lruCache<'k, 'v>): float => {
  Belt.Int.toFloat(cache.length) /. Belt.Int.toFloat(cache.capacity)
}

/** Internal: Find entry index by key */
let findEntryIndex = (cache: lruCache<'k, 'v>, key: 'k): option<int> => {
  let found = ref(None)
  let i = ref(0)
  while found.contents->Belt.Option.isNone && i.contents < cache.capacity {
    switch Belt.Array.getUnsafe(cache.entries, i.contents) {
    | Some(entry) =>
      if entry.key == key {
        found := Some(i.contents)
      }
    | None => ()
    }
    i := i.contents + 1
  }
  found.contents
}

/** Internal: Find the LRU entry index */
let findLruIndex = (cache: lruCache<'k, 'v>): int => {
  let lruIndex = ref(0)
  let minOrder = ref(max_int)

  for i in 0 to cache.capacity - 1 {
    switch Belt.Array.getUnsafe(cache.entries, i) {
    | Some(entry) =>
      if entry.accessOrder < minOrder.contents {
        minOrder := entry.accessOrder
        lruIndex := i
      }
    | None =>
      // Empty slot is preferred
      lruIndex := i
      minOrder := -1
    }
  }

  lruIndex.contents
}

/** Get a value from the cache, updating access order
 *
 * This marks the entry as recently used, affecting eviction order.
 * Use peek() if you don't want to affect the access order.
 */
let get = (cache: lruCache<'k, 'v>, key: 'k): option<'v> => {
  switch findEntryIndex(cache, key) {
  | None => None
  | Some(index) =>
    switch Belt.Array.getUnsafe(cache.entries, index) {
    | None => None
    | Some(entry) =>
      cache.counter = cache.counter + 1
      entry.accessOrder = cache.counter
      Some(entry.value)
    }
  }
}

/** Peek at a value without updating access order
 *
 * This does not affect the eviction order.
 */
let peek = (cache: lruCache<'k, 'v>, key: 'k): option<'v> => {
  switch findEntryIndex(cache, key) {
  | None => None
  | Some(index) =>
    switch Belt.Array.getUnsafe(cache.entries, index) {
    | None => None
    | Some(entry) => Some(entry.value)
    }
  }
}

/** Put a value into the cache, evicting LRU entry if full
 *
 * Returns the evicted entry if one was evicted, None otherwise.
 */
let put = (cache: lruCache<'k, 'v>, key: 'k, value: 'v): option<('k, 'v)> => {
  cache.counter = cache.counter + 1

  // Check if key already exists
  switch findEntryIndex(cache, key) {
  | Some(index) =>
    // Update existing entry
    switch Belt.Array.getUnsafe(cache.entries, index) {
    | Some(entry) =>
      Belt.Array.setUnsafe(
        cache.entries,
        index,
        Some({
          key,
          value,
          accessOrder: cache.counter,
        }),
      )
      None
    | None => None
    }
  | None =>
    // Find slot (empty or LRU)
    let targetIndex = findLruIndex(cache)
    let evicted = Belt.Array.getUnsafe(cache.entries, targetIndex)

    Belt.Array.setUnsafe(
      cache.entries,
      targetIndex,
      Some({
        key,
        value,
        accessOrder: cache.counter,
      }),
    )

    switch evicted {
    | None =>
      cache.length = cache.length + 1
      None
    | Some(entry) => Some((entry.key, entry.value))
    }
  }
}

/** Remove a key from the cache
 *
 * Returns true if the key was found and removed, false otherwise.
 */
let remove = (cache: lruCache<'k, 'v>, key: 'k): bool => {
  switch findEntryIndex(cache, key) {
  | None => false
  | Some(index) =>
    Belt.Array.setUnsafe(cache.entries, index, None)
    cache.length = cache.length - 1
    true
  }
}

/** Check if a key exists in the cache (without affecting access order) */
let contains = (cache: lruCache<'k, 'v>, key: 'k): bool => {
  findEntryIndex(cache, key)->Belt.Option.isSome
}

/** Clear all entries from the cache */
let clear = (cache: lruCache<'k, 'v>): unit => {
  for i in 0 to cache.capacity - 1 {
    Belt.Array.setUnsafe(cache.entries, i, None)
  }
  cache.length = 0
  cache.counter = 0
}

/** Get all keys in the cache (in no particular order) */
let keys = (cache: lruCache<'k, 'v>): array<'k> => {
  let result = []
  for i in 0 to cache.capacity - 1 {
    switch Belt.Array.getUnsafe(cache.entries, i) {
    | Some(entry) => {
        let _ = Js.Array2.push(result, entry.key)
      }
    | None => ()
    }
  }
  result
}

/** Get all values in the cache (in no particular order) */
let values = (cache: lruCache<'k, 'v>): array<'v> => {
  let result = []
  for i in 0 to cache.capacity - 1 {
    switch Belt.Array.getUnsafe(cache.entries, i) {
    | Some(entry) => {
        let _ = Js.Array2.push(result, entry.value)
      }
    | None => ()
    }
  }
  result
}

/** Get all entries as key-value pairs (in no particular order) */
let entries = (cache: lruCache<'k, 'v>): array<('k, 'v)> => {
  let result = []
  for i in 0 to cache.capacity - 1 {
    switch Belt.Array.getUnsafe(cache.entries, i) {
    | Some(entry) => {
        let _ = Js.Array2.push(result, (entry.key, entry.value))
      }
    | None => ()
    }
  }
  result
}

/** Apply a function to each entry in the cache */
let forEach = (cache: lruCache<'k, 'v>, fn: ('k, 'v) => unit): unit => {
  for i in 0 to cache.capacity - 1 {
    switch Belt.Array.getUnsafe(cache.entries, i) {
    | Some(entry) => fn(entry.key, entry.value)
    | None => ()
    }
  }
}

/** Get entries sorted by access order (most recent first) */
let entriesByAccessOrder = (cache: lruCache<'k, 'v>): array<('k, 'v)> => {
  let validEntries = []
  for i in 0 to cache.capacity - 1 {
    switch Belt.Array.getUnsafe(cache.entries, i) {
    | Some(entry) => {
        let _ = Js.Array2.push(validEntries, entry)
      }
    | None => ()
    }
  }

  // Sort by access order (descending - most recent first)
  let sorted = Js.Array2.sortInPlaceWith(validEntries, (a, b) => b.accessOrder - a.accessOrder)

  Belt.Array.map(sorted, entry => (entry.key, entry.value))
}

/** Get the most recently used entry */
let getMostRecentlyUsed = (cache: lruCache<'k, 'v>): option<('k, 'v)> => {
  let mru = ref(None)
  let maxOrder = ref(-1)

  for i in 0 to cache.capacity - 1 {
    switch Belt.Array.getUnsafe(cache.entries, i) {
    | Some(entry) =>
      if entry.accessOrder > maxOrder.contents {
        maxOrder := entry.accessOrder
        mru := Some((entry.key, entry.value))
      }
    | None => ()
    }
  }

  mru.contents
}

/** Get the least recently used entry (next to be evicted) */
let getLeastRecentlyUsed = (cache: lruCache<'k, 'v>): option<('k, 'v)> => {
  if isEmpty(cache) {
    None
  } else {
    let lru = ref(None)
    let minOrder = ref(max_int)

    for i in 0 to cache.capacity - 1 {
      switch Belt.Array.getUnsafe(cache.entries, i) {
      | Some(entry) =>
        if entry.accessOrder < minOrder.contents {
          minOrder := entry.accessOrder
          lru := Some((entry.key, entry.value))
        }
      | None => ()
      }
    }

    lru.contents
  }
}

/** Get or compute a value (cache-aside pattern)
 *
 * If the key exists, returns the cached value.
 * If not, calls the compute function, caches the result, and returns it.
 */
let getOrCompute = (cache: lruCache<'k, 'v>, key: 'k, compute: 'k => 'v): 'v => {
  switch get(cache, key) {
  | Some(value) => value
  | None =>
    let value = compute(key)
    let _ = put(cache, key, value)
    value
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

/** Status with optional value from Zig FFI */
type ffiStatusOpt = {
  status: int,
  hasValue: bool,
  value: int,
}

/** FFI binding to Zig LRU cache operations */
@module("proven") external ffiLruGet: (int, int) => ffiStatusOpt = "proven_lru_get"
@module("proven") external ffiLruPeek: (int, int) => ffiStatusOpt = "proven_lru_peek"
@module("proven") external ffiLruPut: (int, int, int) => ffiStatusOpt = "proven_lru_put"
@module("proven") external ffiLruRemove: (int, int) => bool = "proven_lru_remove"
@module("proven") external ffiLruContains: (int, int) => bool = "proven_lru_contains"
@module("proven") external ffiLruSize: int => int = "proven_lru_size"
@module("proven") external ffiLruCapacity: int => int = "proven_lru_capacity"
@module("proven") external ffiLruClear: int => unit = "proven_lru_clear"
@module("proven") external ffiLruFillRatio: int => float = "proven_lru_fill_ratio"
