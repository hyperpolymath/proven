// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeQueue - Bounded queue operations that cannot crash.
 *
 * Provides safe bounded FIFO queue and priority queue implementations.
 * All operations return Result types to handle edge cases like
 * queue full/empty conditions safely.
 */

// ============================================================================
// Error types
// ============================================================================

/** Error types for queue operations */
type queueError =
  | QueueFull
  | QueueEmpty
  | InvalidCapacity

/** Convert error to human-readable string */
let errorToString = (error: queueError): string => {
  switch error {
  | QueueFull => "Queue is full"
  | QueueEmpty => "Queue is empty"
  | InvalidCapacity => "Invalid capacity specified"
  }
}

// ============================================================================
// Bounded FIFO Queue
// ============================================================================

/** A bounded FIFO queue with fixed capacity */
type boundedQueue<'a> = {
  mutable data: array<option<'a>>,
  mutable head: int,
  mutable tail: int,
  mutable length: int,
  capacity: int,
}

/** Create a new bounded queue with given capacity */
let make = (capacity: int): result<boundedQueue<'a>, queueError> => {
  if capacity <= 0 {
    Error(InvalidCapacity)
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

/** Get the current number of elements in the queue */
let length = (queue: boundedQueue<'a>): int => {
  queue.length
}

/** Check if the queue is empty */
let isEmpty = (queue: boundedQueue<'a>): bool => {
  queue.length == 0
}

/** Check if the queue is full */
let isFull = (queue: boundedQueue<'a>): bool => {
  queue.length >= queue.capacity
}

/** Get remaining capacity */
let remaining = (queue: boundedQueue<'a>): int => {
  queue.capacity - queue.length
}

/** Get the total capacity of the queue */
let getCapacity = (queue: boundedQueue<'a>): int => {
  queue.capacity
}

/** Enqueue an element to the back of the queue */
let enqueue = (queue: boundedQueue<'a>, value: 'a): result<unit, queueError> => {
  if isFull(queue) {
    Error(QueueFull)
  } else {
    Belt.Array.setUnsafe(queue.data, queue.tail, Some(value))
    queue.tail = mod(queue.tail + 1, queue.capacity)
    queue.length = queue.length + 1
    Ok()
  }
}

/** Enqueue an element, dropping the oldest if full */
let enqueueDropOldest = (queue: boundedQueue<'a>, value: 'a): option<'a> => {
  let dropped = if isFull(queue) {
    let oldValue = Belt.Array.getUnsafe(queue.data, queue.head)
    queue.head = mod(queue.head + 1, queue.capacity)
    queue.length = queue.length - 1
    oldValue
  } else {
    None
  }
  Belt.Array.setUnsafe(queue.data, queue.tail, Some(value))
  queue.tail = mod(queue.tail + 1, queue.capacity)
  queue.length = queue.length + 1
  dropped
}

/** Dequeue an element from the front of the queue */
let dequeue = (queue: boundedQueue<'a>): result<'a, queueError> => {
  if isEmpty(queue) {
    Error(QueueEmpty)
  } else {
    switch Belt.Array.getUnsafe(queue.data, queue.head) {
    | Some(value) =>
      Belt.Array.setUnsafe(queue.data, queue.head, None)
      queue.head = mod(queue.head + 1, queue.capacity)
      queue.length = queue.length - 1
      Ok(value)
    | None => Error(QueueEmpty)
    }
  }
}

/** Peek at the front element without removing it */
let peek = (queue: boundedQueue<'a>): option<'a> => {
  if isEmpty(queue) {
    None
  } else {
    Belt.Array.getUnsafe(queue.data, queue.head)
  }
}

/** Peek at the back element without removing it */
let peekBack = (queue: boundedQueue<'a>): option<'a> => {
  if isEmpty(queue) {
    None
  } else {
    let backIndex = mod(queue.tail - 1 + queue.capacity, queue.capacity)
    Belt.Array.getUnsafe(queue.data, backIndex)
  }
}

/** Clear all elements from the queue */
let clear = (queue: boundedQueue<'a>): unit => {
  for i in 0 to queue.capacity - 1 {
    Belt.Array.setUnsafe(queue.data, i, None)
  }
  queue.head = 0
  queue.tail = 0
  queue.length = 0
}

/** Convert queue to array (in FIFO order) */
let toArray = (queue: boundedQueue<'a>): array<'a> => {
  let result = []
  let i = ref(queue.head)
  let count = ref(0)
  while count.contents < queue.length {
    switch Belt.Array.getUnsafe(queue.data, i.contents) {
    | Some(value) => {
        let _ = Js.Array2.push(result, value)
      }
    | None => ()
    }
    i := mod(i.contents + 1, queue.capacity)
    count := count.contents + 1
  }
  result
}

/** Create queue from array, fails if array exceeds capacity */
let fromArray = (arr: array<'a>, capacity: int): result<boundedQueue<'a>, queueError> => {
  let arrLen = Belt.Array.length(arr)
  if arrLen > capacity {
    Error(QueueFull)
  } else {
    switch make(capacity) {
    | Error(e) => Error(e)
    | Ok(queue) =>
      Belt.Array.forEach(arr, value => {
        let _ = enqueue(queue, value)
      })
      Ok(queue)
    }
  }
}

/** Apply a function to each element in queue order */
let forEach = (queue: boundedQueue<'a>, fn: 'a => unit): unit => {
  let i = ref(queue.head)
  let count = ref(0)
  while count.contents < queue.length {
    switch Belt.Array.getUnsafe(queue.data, i.contents) {
    | Some(value) => fn(value)
    | None => ()
    }
    i := mod(i.contents + 1, queue.capacity)
    count := count.contents + 1
  }
}

/** Get the fill ratio (0.0 to 1.0) */
let fillRatio = (queue: boundedQueue<'a>): float => {
  Belt.Int.toFloat(queue.length) /. Belt.Int.toFloat(queue.capacity)
}

// ============================================================================
// Priority Queue (Min-Heap)
// ============================================================================

/** A bounded priority queue (min-heap) */
type priorityQueue<'a> = {
  mutable data: array<option<'a>>,
  mutable length: int,
  capacity: int,
  compare: ('a, 'a) => int,
}

/** Create a new priority queue with custom comparison function
 *
 * The compare function should return:
 * - negative if first arg has higher priority (comes first)
 * - positive if second arg has higher priority
 * - zero if equal priority
 */
let makePriorityQueue = (capacity: int, compare: ('a, 'a) => int): result<priorityQueue<'a>, queueError> => {
  if capacity <= 0 {
    Error(InvalidCapacity)
  } else {
    Ok({
      data: Belt.Array.make(capacity, None),
      length: 0,
      capacity,
      compare,
    })
  }
}

/** Create a min-priority queue (smallest values have highest priority) */
let makeMinQueue = (capacity: int): result<priorityQueue<int>, queueError> => {
  makePriorityQueue(capacity, (a, b) => a - b)
}

/** Create a max-priority queue (largest values have highest priority) */
let makeMaxQueue = (capacity: int): result<priorityQueue<int>, queueError> => {
  makePriorityQueue(capacity, (a, b) => b - a)
}

/** Get the number of elements in the priority queue */
let pqLength = (pq: priorityQueue<'a>): int => {
  pq.length
}

/** Check if priority queue is empty */
let pqIsEmpty = (pq: priorityQueue<'a>): bool => {
  pq.length == 0
}

/** Check if priority queue is full */
let pqIsFull = (pq: priorityQueue<'a>): bool => {
  pq.length >= pq.capacity
}

/** Internal: sift up to restore heap property */
let rec siftUp = (pq: priorityQueue<'a>, index: int): unit => {
  if index > 0 {
    let parentIndex = (index - 1) / 2
    switch (Belt.Array.getUnsafe(pq.data, index), Belt.Array.getUnsafe(pq.data, parentIndex)) {
    | (Some(current), Some(parent)) =>
      if pq.compare(current, parent) < 0 {
        // Swap
        Belt.Array.setUnsafe(pq.data, index, Some(parent))
        Belt.Array.setUnsafe(pq.data, parentIndex, Some(current))
        siftUp(pq, parentIndex)
      }
    | _ => ()
    }
  }
}

/** Internal: sift down to restore heap property */
let rec siftDown = (pq: priorityQueue<'a>, index: int): unit => {
  let leftChild = 2 * index + 1
  let rightChild = 2 * index + 2
  let smallestIndex = ref(index)

  // Check left child
  if leftChild < pq.length {
    switch (Belt.Array.getUnsafe(pq.data, leftChild), Belt.Array.getUnsafe(pq.data, smallestIndex.contents)) {
    | (Some(left), Some(smallest)) =>
      if pq.compare(left, smallest) < 0 {
        smallestIndex := leftChild
      }
    | _ => ()
    }
  }

  // Check right child
  if rightChild < pq.length {
    switch (Belt.Array.getUnsafe(pq.data, rightChild), Belt.Array.getUnsafe(pq.data, smallestIndex.contents)) {
    | (Some(right), Some(smallest)) =>
      if pq.compare(right, smallest) < 0 {
        smallestIndex := rightChild
      }
    | _ => ()
    }
  }

  // Swap if needed
  if smallestIndex.contents != index {
    switch (Belt.Array.getUnsafe(pq.data, index), Belt.Array.getUnsafe(pq.data, smallestIndex.contents)) {
    | (Some(current), Some(smallest)) =>
      Belt.Array.setUnsafe(pq.data, index, Some(smallest))
      Belt.Array.setUnsafe(pq.data, smallestIndex.contents, Some(current))
      siftDown(pq, smallestIndex.contents)
    | _ => ()
    }
  }
}

/** Push an element into the priority queue */
let pqPush = (pq: priorityQueue<'a>, value: 'a): result<unit, queueError> => {
  if pqIsFull(pq) {
    Error(QueueFull)
  } else {
    Belt.Array.setUnsafe(pq.data, pq.length, Some(value))
    pq.length = pq.length + 1
    siftUp(pq, pq.length - 1)
    Ok()
  }
}

/** Pop the highest priority element */
let pqPop = (pq: priorityQueue<'a>): result<'a, queueError> => {
  if pqIsEmpty(pq) {
    Error(QueueEmpty)
  } else {
    switch Belt.Array.getUnsafe(pq.data, 0) {
    | Some(result) =>
      pq.length = pq.length - 1
      if pq.length > 0 {
        // Move last element to root and sift down
        Belt.Array.setUnsafe(pq.data, 0, Belt.Array.getUnsafe(pq.data, pq.length))
        Belt.Array.setUnsafe(pq.data, pq.length, None)
        siftDown(pq, 0)
      } else {
        Belt.Array.setUnsafe(pq.data, 0, None)
      }
      Ok(result)
    | None => Error(QueueEmpty)
    }
  }
}

/** Peek at the highest priority element without removing it */
let pqPeek = (pq: priorityQueue<'a>): option<'a> => {
  if pqIsEmpty(pq) {
    None
  } else {
    Belt.Array.getUnsafe(pq.data, 0)
  }
}

/** Clear all elements from the priority queue */
let pqClear = (pq: priorityQueue<'a>): unit => {
  for i in 0 to pq.length - 1 {
    Belt.Array.setUnsafe(pq.data, i, None)
  }
  pq.length = 0
}

// ============================================================================
// FFI bindings to Zig (for WASM integration)
// ============================================================================

/** Status codes from Zig FFI */
type ffiStatus = {
  status: int,
  value: int,
}

/** FFI binding to Zig bounded queue operations */
@module("proven") external ffiQueueEnqueue: (int, int) => ffiStatus = "proven_queue_enqueue"
@module("proven") external ffiQueueDequeue: int => ffiStatus = "proven_queue_dequeue"
@module("proven") external ffiQueuePeek: int => ffiStatus = "proven_queue_peek"
@module("proven") external ffiQueueLength: int => int = "proven_queue_length"
@module("proven") external ffiQueueCapacity: int => int = "proven_queue_capacity"
@module("proven") external ffiQueueIsFull: int => bool = "proven_queue_is_full"
@module("proven") external ffiQueueIsEmpty: int => bool = "proven_queue_is_empty"
@module("proven") external ffiQueueClear: int => unit = "proven_queue_clear"

/** FFI binding to Zig priority queue operations */
@module("proven") external ffiPQPush: (int, int) => ffiStatus = "proven_pq_push"
@module("proven") external ffiPQPop: int => ffiStatus = "proven_pq_pop"
@module("proven") external ffiPQPeek: int => ffiStatus = "proven_pq_peek"
