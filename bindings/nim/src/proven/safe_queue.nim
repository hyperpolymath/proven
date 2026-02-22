# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe bounded queue operations.
# Thin wrapper over libproven FFI -- all logic lives in Idris.
#
# Note: The C API provides a FIFO queue of int64 values.
# For generic types, callers should marshal to/from int64.

import std/options
import lib_proven

type
  BoundedQueue* = object
    ## Bounded FIFO queue backed by libproven's verified implementation.
    ## Elements are int64 values.
    handle: ptr ProvenBoundedQueue

  QueueError* = object of CatchableError
    ## Error raised when queue creation fails.

proc `=destroy`*(q: BoundedQueue) =
  ## Destructor: automatically frees the underlying C queue.
  if q.handle != nil:
    provenQueueFree(q.handle)

proc `=copy`*(dest: var BoundedQueue, src: BoundedQueue) {.error:
  "BoundedQueue cannot be copied; it owns an opaque C resource".}

proc `=sink`*(dest: var BoundedQueue, src: BoundedQueue) =
  ## Move semantics for BoundedQueue.
  `=destroy`(dest)
  dest.handle = src.handle

proc newBoundedQueue*(capacity: int): BoundedQueue =
  ## Create a new bounded FIFO queue with the given capacity.
  ## Maximum capacity is 1,000,000 as enforced by libproven.
  let handle = provenQueueCreate(csize_t(capacity))
  if handle == nil:
    raise newException(QueueError,
      "Failed to create bounded queue (allocation or invalid capacity)")
  BoundedQueue(handle: handle)

proc push*(q: BoundedQueue, value: int64): bool =
  ## Push a value onto the queue.
  ## Returns true on success, false if the queue is full.
  if q.handle == nil:
    return false
  provenQueuePush(q.handle, value)

proc pop*(q: BoundedQueue): Option[int64] =
  ## Pop the oldest value from the queue (FIFO order).
  ## Returns None if the queue is empty.
  if q.handle == nil:
    return none(int64)
  let res = provenQueuePop(q.handle)
  if res.status == PROVEN_OK:
    some(res.value)
  else:
    none(int64)

proc len*(q: BoundedQueue): int =
  ## Get the current number of elements in the queue.
  if q.handle != nil:
    int(provenQueueSize(q.handle))
  else:
    0

proc isEmpty*(q: BoundedQueue): bool =
  ## Check if the queue is empty.
  q.len == 0

proc isFull*(q: BoundedQueue): bool =
  ## Check if the queue is full.
  if q.handle != nil:
    q.handle[].count >= q.handle[].capacity
  else:
    true

proc capacity*(q: BoundedQueue): int =
  ## Get the queue capacity.
  if q.handle != nil:
    int(q.handle[].capacity)
  else:
    0

proc remaining*(q: BoundedQueue): int =
  ## Get remaining capacity.
  if q.handle != nil:
    int(q.handle[].capacity) - int(provenQueueSize(q.handle))
  else:
    0

proc pushDroppingOldest*(q: BoundedQueue, value: int64) =
  ## Push a value, dropping the oldest element if the queue is full.
  if q.handle != nil:
    if q.isFull:
      discard q.pop()
    discard provenQueuePush(q.handle, value)

proc isValid*(q: BoundedQueue): bool =
  ## Check if the queue handle is valid (non-nil).
  q.handle != nil
