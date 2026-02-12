# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe buffer operations with bounds checking.

import std/[options]

type
  BoundedBuffer*[T] = object
    ## A bounded buffer with safe operations.
    data: seq[T]
    capacity: int

  RingBuffer*[T] = object
    ## Ring buffer for FIFO operations.
    data: seq[Option[T]]
    head: int
    tail: int
    length: int
    capacity: int

# BoundedBuffer implementation
proc newBoundedBuffer*[T](capacity: int): BoundedBuffer[T] =
  ## Create a new bounded buffer with the given capacity.
  BoundedBuffer[T](
    data: newSeqOfCap[T](capacity),
    capacity: capacity
  )

proc len*[T](buf: BoundedBuffer[T]): int =
  ## Get current length.
  buf.data.len

proc isEmpty*[T](buf: BoundedBuffer[T]): bool =
  ## Check if buffer is empty.
  buf.data.len == 0

proc isFull*[T](buf: BoundedBuffer[T]): bool =
  ## Check if buffer is full.
  buf.data.len >= buf.capacity

proc remaining*[T](buf: BoundedBuffer[T]): int =
  ## Get remaining capacity.
  buf.capacity - buf.data.len

proc push*[T](buf: var BoundedBuffer[T], value: T): bool =
  ## Push an element (returns false if full).
  if buf.isFull():
    return false
  buf.data.add(value)
  true

proc pop*[T](buf: var BoundedBuffer[T]): Option[T] =
  ## Pop an element (returns None if empty).
  if buf.isEmpty():
    return none(T)
  result = some(buf.data.pop())

proc get*[T](buf: BoundedBuffer[T], index: int): Option[T] =
  ## Get element at index (returns None if out of bounds).
  if index < 0 or index >= buf.data.len:
    return none(T)
  result = some(buf.data[index])

proc set*[T](buf: var BoundedBuffer[T], index: int, value: T): bool =
  ## Set element at index (returns false if out of bounds).
  if index < 0 or index >= buf.data.len:
    return false
  buf.data[index] = value
  true

proc clear*[T](buf: var BoundedBuffer[T]) =
  ## Clear all elements.
  buf.data.setLen(0)

proc toSeq*[T](buf: BoundedBuffer[T]): seq[T] =
  ## Convert to sequence.
  buf.data

# RingBuffer implementation
proc newRingBuffer*[T](capacity: int): RingBuffer[T] =
  ## Create a new ring buffer with the given capacity.
  var data = newSeq[Option[T]](capacity)
  for i in 0..<capacity:
    data[i] = none(T)
  RingBuffer[T](
    data: data,
    head: 0,
    tail: 0,
    length: 0,
    capacity: capacity
  )

proc len*[T](buf: RingBuffer[T]): int =
  ## Get current length.
  buf.length

proc isEmpty*[T](buf: RingBuffer[T]): bool =
  ## Check if buffer is empty.
  buf.length == 0

proc isFull*[T](buf: RingBuffer[T]): bool =
  ## Check if buffer is full.
  buf.length >= buf.capacity

proc enqueue*[T](buf: var RingBuffer[T], value: T): bool =
  ## Enqueue an element (returns false if full).
  if buf.isFull():
    return false
  buf.data[buf.tail] = some(value)
  buf.tail = (buf.tail + 1) mod buf.capacity
  buf.length += 1
  true

proc dequeue*[T](buf: var RingBuffer[T]): Option[T] =
  ## Dequeue an element (returns None if empty).
  if buf.isEmpty():
    return none(T)
  result = buf.data[buf.head]
  buf.data[buf.head] = none(T)
  buf.head = (buf.head + 1) mod buf.capacity
  buf.length -= 1

proc peek*[T](buf: RingBuffer[T]): Option[T] =
  ## Peek at front without removing.
  if buf.isEmpty():
    return none(T)
  buf.data[buf.head]

proc enqueueDroppingOldest*[T](buf: var RingBuffer[T], value: T) =
  ## Enqueue, dropping oldest if full.
  if buf.isFull():
    discard buf.dequeue()
  discard buf.enqueue(value)

proc clear*[T](buf: var RingBuffer[T]) =
  ## Clear all elements.
  for i in 0..<buf.capacity:
    buf.data[i] = none(T)
  buf.head = 0
  buf.tail = 0
  buf.length = 0
