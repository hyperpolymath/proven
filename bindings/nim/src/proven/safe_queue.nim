# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe bounded queue operations.

import std/[options, algorithm]

type
  BoundedQueue*[T] = object
    ## Bounded FIFO queue with safe operations.
    data: seq[Option[T]]
    head: int
    tail: int
    length: int
    capacity: int

  PriorityQueue*[T] = object
    ## Priority queue (min-heap).
    data: seq[T]
    capacity: int

# BoundedQueue implementation
proc newBoundedQueue*[T](capacity: int): BoundedQueue[T] =
  ## Create a new bounded queue with the given capacity.
  var data = newSeq[Option[T]](capacity)
  for i in 0..<capacity:
    data[i] = none(T)
  BoundedQueue[T](
    data: data,
    head: 0,
    tail: 0,
    length: 0,
    capacity: capacity
  )

proc len*[T](q: BoundedQueue[T]): int =
  ## Get current length.
  q.length

proc isEmpty*[T](q: BoundedQueue[T]): bool =
  ## Check if queue is empty.
  q.length == 0

proc isFull*[T](q: BoundedQueue[T]): bool =
  ## Check if queue is full.
  q.length >= q.capacity

proc remaining*[T](q: BoundedQueue[T]): int =
  ## Get remaining capacity.
  q.capacity - q.length

proc enqueue*[T](q: var BoundedQueue[T], value: T): bool =
  ## Enqueue an element (returns false if full).
  if q.isFull():
    return false
  q.data[q.tail] = some(value)
  q.tail = (q.tail + 1) mod q.capacity
  q.length += 1
  true

proc dequeue*[T](q: var BoundedQueue[T]): Option[T] =
  ## Dequeue an element (returns None if empty).
  if q.isEmpty():
    return none(T)
  result = q.data[q.head]
  q.data[q.head] = none(T)
  q.head = (q.head + 1) mod q.capacity
  q.length -= 1

proc peek*[T](q: BoundedQueue[T]): Option[T] =
  ## Peek at front without removing.
  if q.isEmpty():
    return none(T)
  q.data[q.head]

proc enqueueDroppingOldest*[T](q: var BoundedQueue[T], value: T) =
  ## Enqueue, dropping oldest if full.
  if q.isFull():
    discard q.dequeue()
  discard q.enqueue(value)

proc clear*[T](q: var BoundedQueue[T]) =
  ## Clear all elements.
  for i in 0..<q.capacity:
    q.data[i] = none(T)
  q.head = 0
  q.tail = 0
  q.length = 0

# PriorityQueue implementation
proc newPriorityQueue*[T](capacity: int): PriorityQueue[T] =
  ## Create a new priority queue with the given capacity.
  PriorityQueue[T](
    data: newSeqOfCap[T](capacity),
    capacity: capacity
  )

proc len*[T](pq: PriorityQueue[T]): int =
  ## Get current length.
  pq.data.len

proc isEmpty*[T](pq: PriorityQueue[T]): bool =
  ## Check if queue is empty.
  pq.data.len == 0

proc isFull*[T](pq: PriorityQueue[T]): bool =
  ## Check if queue is full.
  pq.data.len >= pq.capacity

proc siftUp[T](pq: var PriorityQueue[T], idx: int) =
  ## Sift up to maintain heap property.
  var idx = idx
  while idx > 0:
    let parent = (idx - 1) div 2
    if pq.data[idx] >= pq.data[parent]:
      break
    swap(pq.data[idx], pq.data[parent])
    idx = parent

proc siftDown[T](pq: var PriorityQueue[T], idx: int) =
  ## Sift down to maintain heap property.
  var idx = idx
  while true:
    let left = 2 * idx + 1
    let right = 2 * idx + 2
    var smallest = idx

    if left < pq.data.len and pq.data[left] < pq.data[smallest]:
      smallest = left
    if right < pq.data.len and pq.data[right] < pq.data[smallest]:
      smallest = right

    if smallest == idx:
      break
    swap(pq.data[idx], pq.data[smallest])
    idx = smallest

proc push*[T](pq: var PriorityQueue[T], value: T): bool =
  ## Push an element (returns false if full).
  if pq.isFull():
    return false
  pq.data.add(value)
  pq.siftUp(pq.data.len - 1)
  true

proc pop*[T](pq: var PriorityQueue[T]): Option[T] =
  ## Pop the minimum element (returns None if empty).
  if pq.isEmpty():
    return none(T)
  result = some(pq.data[0])
  pq.data[0] = pq.data[^1]
  pq.data.setLen(pq.data.len - 1)
  if pq.data.len > 0:
    pq.siftDown(0)

proc peek*[T](pq: PriorityQueue[T]): Option[T] =
  ## Peek at minimum without removing.
  if pq.isEmpty():
    return none(T)
  some(pq.data[0])

proc clear*[T](pq: var PriorityQueue[T]) =
  ## Clear all elements.
  pq.data.setLen(0)
