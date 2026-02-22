# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe LRU (Least Recently Used) cache.
# Thin wrapper over libproven FFI -- all logic lives in Idris.
#
# Note: The C API uses uint64 keys and int64 values.  This wrapper
# exposes the same key/value types.  For arbitrary key types, callers
# should hash them to uint64 externally.

import std/options
import lib_proven

type
  LruCache* = object
    ## LRU cache with bounded capacity, backed by libproven's verified
    ## implementation.  Keys are uint64 and values are int64.
    handle: ptr ProvenLRUCache

  LruCacheError* = object of CatchableError
    ## Error raised when LRU cache creation fails.

proc `=destroy`*(cache: LruCache) =
  ## Destructor: automatically frees the underlying C LRU cache.
  if cache.handle != nil:
    provenLruFree(cache.handle)

proc `=copy`*(dest: var LruCache, src: LruCache) {.error:
  "LruCache cannot be copied; it owns an opaque C resource".}

proc `=sink`*(dest: var LruCache, src: LruCache) =
  ## Move semantics for LruCache.
  `=destroy`(dest)
  dest.handle = src.handle

proc newLruCache*(capacity: int): LruCache =
  ## Create a new LRU cache with the given capacity.
  ## Maximum capacity is 100,000 as enforced by libproven.
  let handle = provenLruCreate(csize_t(capacity))
  if handle == nil:
    raise newException(LruCacheError,
      "Failed to create LRU cache (allocation or invalid capacity)")
  LruCache(handle: handle)

proc get*(cache: LruCache, key: uint64): Option[int64] =
  ## Get a value by key, promoting it to most recently used.
  ## Returns None if key is not found.
  if cache.handle == nil:
    return none(int64)
  let res = provenLruGet(cache.handle, key)
  if res.status == PROVEN_OK:
    some(res.value)
  else:
    none(int64)

proc put*(cache: LruCache, key: uint64, value: int64): bool =
  ## Insert or update a key-value pair.  If the cache is full, the least
  ## recently used entry is evicted.  Returns true on success.
  if cache.handle == nil:
    return false
  let status = provenLruPut(cache.handle, key, value)
  status == PROVEN_OK

proc contains*(cache: LruCache, key: uint64): bool =
  ## Check if a key exists in the cache.
  ## Note: this does NOT promote the entry (it calls get which does promote,
  ## but there is no peek in the C API).
  if cache.handle == nil:
    return false
  let res = provenLruGet(cache.handle, key)
  res.status == PROVEN_OK

proc len*(cache: LruCache): int =
  ## Get current number of entries in the cache.
  if cache.handle != nil:
    int(cache.handle[].count)
  else:
    0

proc capacity*(cache: LruCache): int =
  ## Get the cache capacity.
  if cache.handle != nil:
    int(cache.handle[].capacity)
  else:
    0

proc isEmpty*(cache: LruCache): bool =
  ## Check if cache is empty.
  cache.len == 0

proc isFull*(cache: LruCache): bool =
  ## Check if cache is at capacity.
  if cache.handle != nil:
    cache.handle[].count >= cache.handle[].capacity
  else:
    true

proc isValid*(cache: LruCache): bool =
  ## Check if the cache handle is valid (non-nil).
  cache.handle != nil
