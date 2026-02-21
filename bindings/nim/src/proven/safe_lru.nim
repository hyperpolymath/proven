# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe LRU (Least Recently Used) cache implementation.

import std/[options, tables]

type
  LruNode[K, V] = ref object
    key: K
    value: V
    prev: LruNode[K, V]
    next: LruNode[K, V]

  LruCache*[K, V] = object
    ## LRU cache with bounded capacity.
    map: Table[K, LruNode[K, V]]
    head: LruNode[K, V]
    tail: LruNode[K, V]
    capacity: int
    length: int

proc newLruCache*[K, V](capacity: int): LruCache[K, V] =
  ## Create a new LRU cache with the given capacity.
  LruCache[K, V](
    map: initTable[K, LruNode[K, V]](),
    head: nil,
    tail: nil,
    capacity: capacity,
    length: 0
  )

proc len*[K, V](cache: LruCache[K, V]): int =
  ## Get current number of entries.
  cache.length

proc isEmpty*[K, V](cache: LruCache[K, V]): bool =
  ## Check if cache is empty.
  cache.length == 0

proc isFull*[K, V](cache: LruCache[K, V]): bool =
  ## Check if cache is full.
  cache.length >= cache.capacity

proc getCapacity*[K, V](cache: LruCache[K, V]): int =
  ## Get capacity.
  cache.capacity

proc moveToFront[K, V](cache: var LruCache[K, V], node: LruNode[K, V]) =
  ## Move a node to the front of the list.
  if cache.head == node:
    return

  # Unlink from current position
  if node.prev != nil:
    node.prev.next = node.next
  if node.next != nil:
    node.next.prev = node.prev

  if cache.tail == node:
    cache.tail = node.prev

  # Link at front
  node.prev = nil
  node.next = cache.head
  if cache.head != nil:
    cache.head.prev = node
  cache.head = node

  if cache.tail == nil:
    cache.tail = node

proc evictLru[K, V](cache: var LruCache[K, V]): Option[V] =
  ## Evict the least recently used entry.
  if cache.tail == nil:
    return none(V)

  let node = cache.tail
  result = some(node.value)

  # Unlink tail
  cache.tail = node.prev
  if cache.tail != nil:
    cache.tail.next = nil
  else:
    cache.head = nil

  cache.map.del(node.key)
  cache.length -= 1

proc get*[K, V](cache: var LruCache[K, V], key: K): Option[V] =
  ## Get a value by key, marking it as recently used.
  if not cache.map.hasKey(key):
    return none(V)

  let node = cache.map[key]
  cache.moveToFront(node)
  result = some(node.value)

proc peek*[K, V](cache: LruCache[K, V], key: K): Option[V] =
  ## Get a value without updating recency.
  if not cache.map.hasKey(key):
    return none(V)
  result = some(cache.map[key].value)

proc put*[K, V](cache: var LruCache[K, V], key: K, value: V): Option[V] =
  ## Insert a key-value pair, evicting LRU if necessary.
  ## Returns the old value if key existed, or evicted value if cache was full.

  # If key exists, update and move to front
  if cache.map.hasKey(key):
    let node = cache.map[key]
    let oldValue = node.value
    node.value = value
    cache.moveToFront(node)
    return some(oldValue)

  # Need to evict?
  var evicted = none(V)
  if cache.isFull():
    evicted = cache.evictLru()

  # Create new node
  let node = LruNode[K, V](
    key: key,
    value: value,
    prev: nil,
    next: cache.head
  )

  # Update head's prev
  if cache.head != nil:
    cache.head.prev = node

  # Update head
  cache.head = node

  # If first node, also set tail
  if cache.tail == nil:
    cache.tail = node

  cache.map[key] = node
  cache.length += 1

  evicted

proc remove*[K, V](cache: var LruCache[K, V], key: K): Option[V] =
  ## Remove a key from the cache.
  if not cache.map.hasKey(key):
    return none(V)

  let node = cache.map[key]
  result = some(node.value)

  # Unlink
  if node.prev != nil:
    node.prev.next = node.next
  else:
    cache.head = node.next

  if node.next != nil:
    node.next.prev = node.prev
  else:
    cache.tail = node.prev

  cache.map.del(key)
  cache.length -= 1

proc contains*[K, V](cache: LruCache[K, V], key: K): bool =
  ## Check if key exists.
  cache.map.hasKey(key)

proc clear*[K, V](cache: var LruCache[K, V]) =
  ## Clear all entries.
  cache.map.clear()
  cache.head = nil
  cache.tail = nil
  cache.length = 0

proc keys*[K, V](cache: LruCache[K, V]): seq[K] =
  ## Get all keys in order (most to least recently used).
  result = @[]
  var node = cache.head
  while node != nil:
    result.add(node.key)
    node = node.next
