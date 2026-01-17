// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeBuffer - Bounded buffer operations that cannot crash.
 *
 * Provides safe bounded buffer and ring buffer implementations with
 * bounds checking on all operations. All operations return Result
 * types to handle edge cases like buffer full/empty conditions.
 */

// ============================================================================
// Error types
// ============================================================================

/** Error types for buffer operations */
type bufferError =
  | OutOfBounds
  | BufferFull
  | BufferEmpty
  | AllocationFailed

/** Convert error to human-readable string */
let errorToString = (error: bufferError): string => {
  switch error {
  | OutOfBounds => "Index out of bounds"
  | BufferFull => "Buffer is full"
  | BufferEmpty => "Buffer is empty"
  | AllocationFailed => "Memory allocation failed"
  }
}

// ============================================================================
// Bounded Buffer
// ============================================================================

/** A bounded buffer with fixed capacity */
type boundedBuffer<'a> = {
  mutable data: array<option<'a>>,
  mutable length: int,
  capacity: int,
}

/** Create a new bounded buffer with given capacity */
let makeBoundedBuffer = (capacity: int): result<boundedBuffer<'a>, bufferError> => {
  if capacity <= 0 {
    Error(AllocationFailed)
  } else {
    Ok({
      data: Belt.Array.make(capacity, None),
      length: 0,
      capacity,
    })
  }
}

/** Get the current length of the buffer */
let length = (buffer: boundedBuffer<'a>): int => {
  buffer.length
}

/** Check if the buffer is empty */
let isEmpty = (buffer: boundedBuffer<'a>): bool => {
  buffer.length == 0
}

/** Check if the buffer is full */
let isFull = (buffer: boundedBuffer<'a>): bool => {
  buffer.length >= buffer.capacity
}

/** Get remaining capacity */
let remaining = (buffer: boundedBuffer<'a>): int => {
  buffer.capacity - buffer.length
}

/** Get the capacity of the buffer */
let getCapacity = (buffer: boundedBuffer<'a>): int => {
  buffer.capacity
}

/** Push an element to the end of the buffer */
let push = (buffer: boundedBuffer<'a>, value: 'a): result<unit, bufferError> => {
  if isFull(buffer) {
    Error(BufferFull)
  } else {
    Belt.Array.setUnsafe(buffer.data, buffer.length, Some(value))
    buffer.length = buffer.length + 1
    Ok()
  }
}

/** Pop an element from the end of the buffer */
let pop = (buffer: boundedBuffer<'a>): result<'a, bufferError> => {
  if isEmpty(buffer) {
    Error(BufferEmpty)
  } else {
    buffer.length = buffer.length - 1
    switch Belt.Array.getUnsafe(buffer.data, buffer.length) {
    | Some(value) =>
      Belt.Array.setUnsafe(buffer.data, buffer.length, None)
      Ok(value)
    | None => Error(BufferEmpty)
    }
  }
}

/** Get element at index (0-based) */
let get = (buffer: boundedBuffer<'a>, index: int): result<'a, bufferError> => {
  if index < 0 || index >= buffer.length {
    Error(OutOfBounds)
  } else {
    switch Belt.Array.getUnsafe(buffer.data, index) {
    | Some(value) => Ok(value)
    | None => Error(OutOfBounds)
    }
  }
}

/** Set element at index (0-based), must be within current length */
let set = (buffer: boundedBuffer<'a>, index: int, value: 'a): result<unit, bufferError> => {
  if index < 0 || index >= buffer.length {
    Error(OutOfBounds)
  } else {
    Belt.Array.setUnsafe(buffer.data, index, Some(value))
    Ok()
  }
}

/** Clear all elements from the buffer */
let clear = (buffer: boundedBuffer<'a>): unit => {
  for i in 0 to buffer.length - 1 {
    Belt.Array.setUnsafe(buffer.data, i, None)
  }
  buffer.length = 0
}

/** Peek at the last element without removing it */
let peekLast = (buffer: boundedBuffer<'a>): option<'a> => {
  if isEmpty(buffer) {
    None
  } else {
    Belt.Array.getUnsafe(buffer.data, buffer.length - 1)
  }
}

/** Peek at the first element without removing it */
let peekFirst = (buffer: boundedBuffer<'a>): option<'a> => {
  if isEmpty(buffer) {
    None
  } else {
    Belt.Array.getUnsafe(buffer.data, 0)
  }
}

/** Convert buffer to array (only valid elements) */
let toArray = (buffer: boundedBuffer<'a>): array<'a> => {
  let result = []
  for i in 0 to buffer.length - 1 {
    switch Belt.Array.getUnsafe(buffer.data, i) {
    | Some(value) => {
        let _ = Js.Array2.push(result, value)
      }
    | None => ()
    }
  }
  result
}

/** Create buffer from array, fails if array exceeds capacity */
let fromArray = (arr: array<'a>, capacity: int): result<boundedBuffer<'a>, bufferError> => {
  let arrLen = Belt.Array.length(arr)
  if arrLen > capacity {
    Error(BufferFull)
  } else {
    switch makeBoundedBuffer(capacity) {
    | Error(e) => Error(e)
    | Ok(buffer) =>
      Belt.Array.forEachWithIndex(arr, (i, value) => {
        Belt.Array.setUnsafe(buffer.data, i, Some(value))
      })
      buffer.length = arrLen
      Ok(buffer)
    }
  }
}

// ============================================================================
// Ring Buffer (FIFO)
// ============================================================================

/** A ring buffer for FIFO operations */
type ringBuffer<'a> = {
  mutable data: array<option<'a>>,
  mutable head: int,
  mutable tail: int,
  mutable length: int,
  capacity: int,
}

/** Create a new ring buffer with given capacity */
let makeRingBuffer = (capacity: int): result<ringBuffer<'a>, bufferError> => {
  if capacity <= 0 {
    Error(AllocationFailed)
  } else {
    Ok({
      data: Belt.Array.make(capacity, None),
      head: 0,
      tail: 0,
      length: 0,
      capacity,
    })
  }
}

/** Get the current length of the ring buffer */
let ringLength = (buffer: ringBuffer<'a>): int => {
  buffer.length
}

/** Check if the ring buffer is empty */
let ringIsEmpty = (buffer: ringBuffer<'a>): bool => {
  buffer.length == 0
}

/** Check if the ring buffer is full */
let ringIsFull = (buffer: ringBuffer<'a>): bool => {
  buffer.length >= buffer.capacity
}

/** Get remaining capacity of ring buffer */
let ringRemaining = (buffer: ringBuffer<'a>): int => {
  buffer.capacity - buffer.length
}

/** Enqueue an element (add to back) */
let enqueue = (buffer: ringBuffer<'a>, value: 'a): result<unit, bufferError> => {
  if ringIsFull(buffer) {
    Error(BufferFull)
  } else {
    Belt.Array.setUnsafe(buffer.data, buffer.tail, Some(value))
    buffer.tail = mod(buffer.tail + 1, buffer.capacity)
    buffer.length = buffer.length + 1
    Ok()
  }
}

/** Enqueue, dropping oldest element if full */
let enqueueDropOldest = (buffer: ringBuffer<'a>, value: 'a): unit => {
  if ringIsFull(buffer) {
    // Drop the oldest element (at head)
    buffer.head = mod(buffer.head + 1, buffer.capacity)
    buffer.length = buffer.length - 1
  }
  Belt.Array.setUnsafe(buffer.data, buffer.tail, Some(value))
  buffer.tail = mod(buffer.tail + 1, buffer.capacity)
  buffer.length = buffer.length + 1
}

/** Dequeue an element (remove from front) */
let dequeue = (buffer: ringBuffer<'a>): result<'a, bufferError> => {
  if ringIsEmpty(buffer) {
    Error(BufferEmpty)
  } else {
    switch Belt.Array.getUnsafe(buffer.data, buffer.head) {
    | Some(value) =>
      Belt.Array.setUnsafe(buffer.data, buffer.head, None)
      buffer.head = mod(buffer.head + 1, buffer.capacity)
      buffer.length = buffer.length - 1
      Ok(value)
    | None => Error(BufferEmpty)
    }
  }
}

/** Peek at the front element without removing it */
let ringPeek = (buffer: ringBuffer<'a>): option<'a> => {
  if ringIsEmpty(buffer) {
    None
  } else {
    Belt.Array.getUnsafe(buffer.data, buffer.head)
  }
}

/** Peek at the back element without removing it */
let ringPeekBack = (buffer: ringBuffer<'a>): option<'a> => {
  if ringIsEmpty(buffer) {
    None
  } else {
    let backIndex = mod(buffer.tail - 1 + buffer.capacity, buffer.capacity)
    Belt.Array.getUnsafe(buffer.data, backIndex)
  }
}

/** Clear all elements from the ring buffer */
let ringClear = (buffer: ringBuffer<'a>): unit => {
  for i in 0 to buffer.capacity - 1 {
    Belt.Array.setUnsafe(buffer.data, i, None)
  }
  buffer.head = 0
  buffer.tail = 0
  buffer.length = 0
}

/** Convert ring buffer to array (in FIFO order) */
let ringToArray = (buffer: ringBuffer<'a>): array<'a> => {
  let result = []
  let i = ref(buffer.head)
  let count = ref(0)
  while count.contents < buffer.length {
    switch Belt.Array.getUnsafe(buffer.data, i.contents) {
    | Some(value) => {
        let _ = Js.Array2.push(result, value)
      }
    | None => ()
    }
    i := mod(i.contents + 1, buffer.capacity)
    count := count.contents + 1
  }
  result
}

// ============================================================================
// FFI bindings to Zig (for WASM integration)
// ============================================================================

/** Status codes from Zig FFI */
type ffiStatus = {
  status: int,
  value: int,
}

/** FFI binding to Zig bounded buffer operations */
@module("proven") external ffiBufferPush: (int, int) => ffiStatus = "proven_buffer_push"
@module("proven") external ffiBufferPop: int => ffiStatus = "proven_buffer_pop"
@module("proven") external ffiBufferGet: (int, int) => ffiStatus = "proven_buffer_get"
@module("proven") external ffiBufferLength: int => int = "proven_buffer_length"
@module("proven") external ffiBufferCapacity: int => int = "proven_buffer_capacity"
@module("proven") external ffiBufferIsFull: int => bool = "proven_buffer_is_full"
@module("proven") external ffiBufferIsEmpty: int => bool = "proven_buffer_is_empty"

/** FFI binding to Zig ring buffer operations */
@module("proven") external ffiRingEnqueue: (int, int) => ffiStatus = "proven_ring_enqueue"
@module("proven") external ffiRingDequeue: int => ffiStatus = "proven_ring_dequeue"
@module("proven") external ffiRingPeek: int => ffiStatus = "proven_ring_peek"
