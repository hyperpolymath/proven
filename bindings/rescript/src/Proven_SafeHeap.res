// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeHeap - Binary heap operations that cannot crash.
 *
 * Provides bounded min-heap and max-heap implementations with safe operations.
 * All operations return Result types instead of throwing exceptions.
 * Capacity is bounded to prevent unbounded memory growth.
 */

/** Error types for heap operations */
type heapError =
  | HeapFull
  | HeapEmpty
  | IndexOutOfBounds

/** Heap order direction */
type heapOrder =
  | Min
  | Max

/** A bounded binary heap */
type t<'a> = {
  mutable data: array<'a>,
  mutable length: int,
  capacity: int,
  order: heapOrder,
}

/** Create a new empty min-heap with specified capacity */
let makeMin = (capacity: int): t<'a> => {
  {
    data: [],
    length: 0,
    capacity: capacity,
    order: Min,
  }
}

/** Create a new empty max-heap with specified capacity */
let makeMax = (capacity: int): t<'a> => {
  {
    data: [],
    length: 0,
    capacity: capacity,
    order: Max,
  }
}

/** Get the number of elements in the heap */
let length = (heap: t<'a>): int => {
  heap.length
}

/** Check if the heap is empty */
let isEmpty = (heap: t<'a>): bool => {
  heap.length == 0
}

/** Check if the heap is full */
let isFull = (heap: t<'a>): bool => {
  heap.length >= heap.capacity
}

/** Get remaining capacity */
let remaining = (heap: t<'a>): int => {
  heap.capacity - heap.length
}

/** Get the maximum capacity */
let getCapacity = (heap: t<'a>): int => {
  heap.capacity
}

/** Compare function based on heap order (for comparable types) */
let compare = (order: heapOrder, a: 'a, b: 'a): bool => {
  switch order {
  | Min => a < b
  | Max => a > b
  }
}

/** Sift up operation to maintain heap property */
let rec siftUp = (heap: t<'a>, index: int): unit => {
  if index > 0 {
    let parentIndex = (index - 1) / 2
    let current = Belt.Array.getUnsafe(heap.data, index)
    let parent = Belt.Array.getUnsafe(heap.data, parentIndex)
    if compare(heap.order, current, parent) {
      Belt.Array.setUnsafe(heap.data, index, parent)
      Belt.Array.setUnsafe(heap.data, parentIndex, current)
      siftUp(heap, parentIndex)
    }
  }
}

/** Sift down operation to maintain heap property */
let rec siftDown = (heap: t<'a>, index: int): unit => {
  let leftIndex = 2 * index + 1
  let rightIndex = 2 * index + 2
  let targetIndex = ref(index)

  if leftIndex < heap.length {
    let target = Belt.Array.getUnsafe(heap.data, targetIndex.contents)
    let left = Belt.Array.getUnsafe(heap.data, leftIndex)
    if compare(heap.order, left, target) {
      targetIndex := leftIndex
    }
  }

  if rightIndex < heap.length {
    let target = Belt.Array.getUnsafe(heap.data, targetIndex.contents)
    let right = Belt.Array.getUnsafe(heap.data, rightIndex)
    if compare(heap.order, right, target) {
      targetIndex := rightIndex
    }
  }

  if targetIndex.contents != index {
    let current = Belt.Array.getUnsafe(heap.data, index)
    let target = Belt.Array.getUnsafe(heap.data, targetIndex.contents)
    Belt.Array.setUnsafe(heap.data, index, target)
    Belt.Array.setUnsafe(heap.data, targetIndex.contents, current)
    siftDown(heap, targetIndex.contents)
  }
}

/** Push an element onto the heap (fails if full) */
let push = (heap: t<'a>, value: 'a): result<unit, heapError> => {
  if isFull(heap) {
    Error(HeapFull)
  } else {
    // Expand array if needed
    if heap.length >= Belt.Array.length(heap.data) {
      heap.data = Belt.Array.concat(heap.data, [value])
    } else {
      Belt.Array.setUnsafe(heap.data, heap.length, value)
    }
    heap.length = heap.length + 1
    siftUp(heap, heap.length - 1)
    Ok()
  }
}

/** Push an element, dropping the root if full and new element should replace it
 * For min-heap: drops minimum if new element is larger
 * For max-heap: drops maximum if new element is smaller
 */
let pushBounded = (heap: t<'a>, value: 'a): unit => {
  if !isFull(heap) {
    let _ = push(heap, value)
  } else if heap.length > 0 {
    let root = Belt.Array.getUnsafe(heap.data, 0)
    let shouldReplace = switch heap.order {
    | Min => value > root
    | Max => value < root
    }
    if shouldReplace {
      Belt.Array.setUnsafe(heap.data, 0, value)
      siftDown(heap, 0)
    }
  }
}

/** Pop the root element (fails if empty) */
let pop = (heap: t<'a>): result<'a, heapError> => {
  if isEmpty(heap) {
    Error(HeapEmpty)
  } else {
    let result = Belt.Array.getUnsafe(heap.data, 0)
    heap.length = heap.length - 1
    if heap.length > 0 {
      let last = Belt.Array.getUnsafe(heap.data, heap.length)
      Belt.Array.setUnsafe(heap.data, 0, last)
      siftDown(heap, 0)
    }
    Ok(result)
  }
}

/** Peek at the root element without removing (fails if empty) */
let peek = (heap: t<'a>): result<'a, heapError> => {
  if isEmpty(heap) {
    Error(HeapEmpty)
  } else {
    Ok(Belt.Array.getUnsafe(heap.data, 0))
  }
}

/** Peek at a specific index (fails if out of bounds) */
let peekAt = (heap: t<'a>, index: int): result<'a, heapError> => {
  if index < 0 || index >= heap.length {
    Error(IndexOutOfBounds)
  } else {
    Ok(Belt.Array.getUnsafe(heap.data, index))
  }
}

/** Remove and return root, then push new value (fails if empty)
 * More efficient than separate pop + push
 */
let replace = (heap: t<'a>, value: 'a): result<'a, heapError> => {
  if isEmpty(heap) {
    Error(HeapEmpty)
  } else {
    let result = Belt.Array.getUnsafe(heap.data, 0)
    Belt.Array.setUnsafe(heap.data, 0, value)
    siftDown(heap, 0)
    Ok(result)
  }
}

/** Clear all elements from the heap */
let clear = (heap: t<'a>): unit => {
  heap.length = 0
  heap.data = []
}

/** Get all elements as an array (not in heap order) */
let items = (heap: t<'a>): array<'a> => {
  Belt.Array.slice(heap.data, ~offset=0, ~len=heap.length)
}

/** Convert heap to sorted array (destructive - empties the heap) */
let toSortedArray = (heap: t<'a>): array<'a> => {
  let result = []
  let resultRef = ref(result)
  while !isEmpty(heap) {
    switch pop(heap) {
    | Ok(value) => resultRef := Belt.Array.concat(resultRef.contents, [value])
    | Error(_) => ()
    }
  }
  resultRef.contents
}

/** Check if heap invariant is maintained */
let isValid = (heap: t<'a>): bool => {
  if heap.length <= 1 {
    true
  } else {
    let valid = ref(true)
    let parentCount = (heap.length - 1) / 2 + 1
    for i in 0 to parentCount - 1 {
      if valid.contents {
        let leftIndex = 2 * i + 1
        let rightIndex = 2 * i + 2
        let parent = Belt.Array.getUnsafe(heap.data, i)
        if leftIndex < heap.length {
          let left = Belt.Array.getUnsafe(heap.data, leftIndex)
          if compare(heap.order, left, parent) {
            valid := false
          }
        }
        if valid.contents && rightIndex < heap.length {
          let right = Belt.Array.getUnsafe(heap.data, rightIndex)
          if compare(heap.order, right, parent) {
            valid := false
          }
        }
      }
    }
    valid.contents
  }
}

/** Top-K tracker using a min-heap */
module TopK = {
  type tracker<'a> = {
    heap: t<'a>,
    k: int,
  }

  /** Create a new top-K tracker */
  let make = (k: int): tracker<'a> => {
    {
      heap: makeMin(k),
      k: k,
    }
  }

  /** Add a value to the tracker */
  let add = (tracker: tracker<'a>, value: 'a): unit => {
    pushBounded(tracker.heap, value)
  }

  /** Get the current top-K values (not sorted) */
  let getTopK = (tracker: tracker<'a>): array<'a> => {
    items(tracker.heap)
  }

  /** Get the threshold value (minimum among top-K) */
  let threshold = (tracker: tracker<'a>): option<'a> => {
    switch peek(tracker.heap) {
    | Ok(v) => Some(v)
    | Error(_) => None
    }
  }

  /** Check if a value would be included in top-K */
  let wouldInclude = (tracker: tracker<'a>, value: 'a): bool => {
    if !isFull(tracker.heap) {
      true
    } else {
      switch peek(tracker.heap) {
      | Ok(minValue) => value > minValue
      | Error(_) => true
      }
    }
  }

  /** Get the count of tracked elements */
  let count = (tracker: tracker<'a>): int => {
    length(tracker.heap)
  }

  /** Clear all tracked elements */
  let clear = (tracker: tracker<'a>): unit => {
    clear(tracker.heap)
  }
}

/** Priority queue using a min-heap (lower values have higher priority) */
module PriorityQueue = {
  type item<'a> = {
    priority: int,
    value: 'a,
  }

  type queue<'a> = t<item<'a>>

  /** Create a new priority queue with specified capacity */
  let make = (capacity: int): queue<'a> => {
    makeMin(capacity)
  }

  /** Enqueue an item with priority */
  let enqueue = (queue: queue<'a>, priority: int, value: 'a): result<unit, heapError> => {
    push(queue, {priority: priority, value: value})
  }

  /** Dequeue the highest priority item */
  let dequeue = (queue: queue<'a>): result<'a, heapError> => {
    switch pop(queue) {
    | Ok(item) => Ok(item.value)
    | Error(e) => Error(e)
    }
  }

  /** Peek at the highest priority item */
  let peekValue = (queue: queue<'a>): result<'a, heapError> => {
    switch peek(queue) {
    | Ok(item) => Ok(item.value)
    | Error(e) => Error(e)
    }
  }

  /** Peek at the highest priority */
  let peekPriority = (queue: queue<'a>): result<int, heapError> => {
    switch peek(queue) {
    | Ok(item) => Ok(item.priority)
    | Error(e) => Error(e)
    }
  }

  /** Check if queue is empty */
  let isEmpty = (queue: queue<'a>): bool => {
    isEmpty(queue)
  }

  /** Get queue length */
  let length = (queue: queue<'a>): int => {
    length(queue)
  }
}
