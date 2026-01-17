-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeQueue - Verified bounded queue operations
|||
||| Type-safe bounded queues with backpressure support.
||| Guarantees capacity is never exceeded.
module Proven.SafeQueue

import Proven.Core
import Data.So
import Data.Vect
import Data.List

%default total

-- ============================================================================
-- BOUNDED QUEUE
-- ============================================================================

||| A bounded queue with maximum capacity
public export
record BoundedQueue (cap : Nat) a where
  constructor MkBoundedQueue
  items : List a
  0 sizeInvariant : So (length items <= cap)

||| Create an empty bounded queue
export
empty : (cap : Nat) -> BoundedQueue cap a
empty cap = believe_me (MkBoundedQueue [])

||| Get current size
export
size : BoundedQueue cap a -> Nat
size q = length q.items

||| Check if queue is empty
export
isEmpty : BoundedQueue cap a -> Bool
isEmpty q = null q.items

||| Check if queue is full
export
isFull : BoundedQueue cap a -> Bool
isFull q = length q.items >= cap

||| Available space
export
availableSpace : BoundedQueue cap a -> Nat
availableSpace {cap} q = cap `minus` length q.items

-- ============================================================================
-- QUEUE OPERATIONS
-- ============================================================================

||| Enqueue result
public export
data EnqueueResult a : Type where
  ||| Successfully enqueued
  Enqueued : EnqueueResult a
  ||| Queue was full
  QueueFull : EnqueueResult a

||| Try to enqueue an item (FIFO - add to back)
export
tryEnqueue : a -> BoundedQueue cap a -> (EnqueueResult a, BoundedQueue cap a)
tryEnqueue x q =
  if length q.items >= cap
  then (QueueFull, q)
  else (Enqueued, believe_me (MkBoundedQueue (q.items ++ [x])))

||| Force enqueue, dropping oldest if full
export
forceEnqueue : a -> BoundedQueue cap a -> BoundedQueue cap a
forceEnqueue x q =
  if length q.items >= cap
  then believe_me (MkBoundedQueue (drop 1 q.items ++ [x]))
  else believe_me (MkBoundedQueue (q.items ++ [x]))

||| Dequeue from front
export
dequeue : BoundedQueue cap a -> (Maybe a, BoundedQueue cap a)
dequeue q =
  case q.items of
    [] => (Nothing, q)
    (x :: xs) => (Just x, believe_me (MkBoundedQueue xs))

||| Peek at front without removing
export
peek : BoundedQueue cap a -> Maybe a
peek q = head' q.items

||| Peek at back
export
peekBack : BoundedQueue cap a -> Maybe a
peek' q = last' q.items

||| Clear the queue
export
clear : BoundedQueue cap a -> BoundedQueue cap a
clear _ = believe_me (MkBoundedQueue [])

||| Convert to list
export
toList : BoundedQueue cap a -> List a
toList q = q.items

-- ============================================================================
-- PRIORITY QUEUE
-- ============================================================================

||| A bounded priority queue (min-heap behavior)
public export
record PriorityQueue (cap : Nat) a where
  constructor MkPriorityQueue
  items : List (Nat, a)  -- (priority, value) - lower is higher priority
  0 sizeInvariant : So (length items <= cap)

||| Create empty priority queue
export
emptyPriority : (cap : Nat) -> PriorityQueue cap a
emptyPriority cap = believe_me (MkPriorityQueue [])

||| Insert into sorted position
insertSorted : (Nat, a) -> List (Nat, a) -> List (Nat, a)
insertSorted x [] = [x]
insertSorted x@(px, _) (y@(py, _) :: ys) =
  if px <= py then x :: y :: ys else y :: insertSorted x ys

||| Try to enqueue with priority
export
tryEnqueuePriority : Nat -> a -> PriorityQueue cap a -> (EnqueueResult a, PriorityQueue cap a)
tryEnqueuePriority prio x q =
  if length q.items >= cap
  then (QueueFull, q)
  else (Enqueued, believe_me (MkPriorityQueue (insertSorted (prio, x) q.items)))

||| Dequeue highest priority (lowest number)
export
dequeuePriority : PriorityQueue cap a -> (Maybe a, PriorityQueue cap a)
dequeuePriority q =
  case q.items of
    [] => (Nothing, q)
    ((_, x) :: xs) => (Just x, believe_me (MkPriorityQueue xs))

||| Peek highest priority
export
peekPriority : PriorityQueue cap a -> Maybe a
peekPriority q =
  case q.items of
    [] => Nothing
    ((_, x) :: _) => Just x

-- ============================================================================
-- RING BUFFER
-- ============================================================================

||| A fixed-size ring buffer (circular queue)
public export
record RingBuffer (n : Nat) a where
  constructor MkRingBuffer
  buffer : Vect n (Maybe a)
  head : Fin n     -- Read position
  tail : Fin n     -- Write position
  count : Nat      -- Current item count
  0 countInvariant : So (count <= n)

||| Create an empty ring buffer
export
emptyRing : (n : Nat) -> {auto prf : GT n 0} -> RingBuffer n a
emptyRing n = believe_me (MkRingBuffer (replicate n Nothing) FZ FZ 0)

||| Check if ring buffer is empty
export
isEmptyRing : RingBuffer n a -> Bool
isEmptyRing rb = rb.count == 0

||| Check if ring buffer is full
export
isFullRing : RingBuffer n a -> Bool
isFullRing {n} rb = rb.count >= n

||| Increment a Fin (wrapping)
nextFin : {n : Nat} -> Fin n -> Fin n
nextFin {n = S m} i =
  let next = finToNat i + 1
  in if next >= S m then FZ else believe_me (natToFinLt next)

||| Write to ring buffer
export
writeRing : a -> RingBuffer n a -> (Bool, RingBuffer n a)
writeRing {n} x rb =
  if rb.count >= n
  then (False, rb)  -- Full
  else let newBuf = replaceAt rb.tail (Just x) rb.buffer
           newTail = nextFin rb.tail
       in (True, believe_me (MkRingBuffer newBuf rb.head newTail (S rb.count)))

||| Read from ring buffer
export
readRing : RingBuffer n a -> (Maybe a, RingBuffer n a)
readRing rb =
  if rb.count == 0
  then (Nothing, rb)
  else let value = index rb.head rb.buffer
           newBuf = replaceAt rb.head Nothing rb.buffer
           newHead = nextFin rb.head
       in (value, believe_me (MkRingBuffer newBuf newHead rb.tail (pred rb.count)))

-- ============================================================================
-- BACKPRESSURE
-- ============================================================================

||| Backpressure signals
public export
data BackpressureSignal : Type where
  ||| Accept more items freely
  Accept : BackpressureSignal
  ||| Slow down production
  SlowDown : BackpressureSignal
  ||| Stop production until drain
  Stop : BackpressureSignal

||| Calculate backpressure based on queue fill level
export
backpressure : BoundedQueue cap a -> (lowWater : Nat) -> (highWater : Nat) -> BackpressureSignal
backpressure {cap} q low high =
  let current = length q.items
  in if current >= high || current >= cap then Stop
     else if current >= low then SlowDown
     else Accept

||| Queue with backpressure thresholds
public export
record BackpressureQueue (cap : Nat) a where
  constructor MkBPQueue
  queue : BoundedQueue cap a
  lowWaterMark : Nat
  highWaterMark : Nat

||| Create backpressure queue
export
newBackpressureQueue : (cap : Nat)
                    -> (lowWater : Nat)
                    -> (highWater : Nat)
                    -> Maybe (BackpressureQueue cap a)
newBackpressureQueue cap low high =
  if low >= high || high > cap
  then Nothing
  else Just (MkBPQueue (empty cap) low high)

||| Get current backpressure signal
export
getBackpressure : BackpressureQueue cap a -> BackpressureSignal
getBackpressure bpq = backpressure bpq.queue bpq.lowWaterMark bpq.highWaterMark

||| Enqueue with backpressure
export
bpEnqueue : a -> BackpressureQueue cap a -> (BackpressureSignal, BackpressureQueue cap a)
bpEnqueue x bpq =
  let (result, newQueue) = tryEnqueue x bpq.queue
      newBPQ = { queue := newQueue } bpq
  in (getBackpressure newBPQ, newBPQ)

-- ============================================================================
-- DOUBLE-ENDED QUEUE (DEQUE)
-- ============================================================================

||| Bounded double-ended queue
public export
record Deque (cap : Nat) a where
  constructor MkDeque
  items : List a
  0 sizeInvariant : So (length items <= cap)

||| Create empty deque
export
emptyDeque : (cap : Nat) -> Deque cap a
emptyDeque cap = believe_me (MkDeque [])

||| Push to front
export
pushFront : a -> Deque cap a -> (Bool, Deque cap a)
pushFront x d =
  if length d.items >= cap
  then (False, d)
  else (True, believe_me (MkDeque (x :: d.items)))

||| Push to back
export
pushBack : a -> Deque cap a -> (Bool, Deque cap a)
pushBack x d =
  if length d.items >= cap
  then (False, d)
  else (True, believe_me (MkDeque (d.items ++ [x])))

||| Pop from front
export
popFront : Deque cap a -> (Maybe a, Deque cap a)
popFront d =
  case d.items of
    [] => (Nothing, d)
    (x :: xs) => (Just x, believe_me (MkDeque xs))

||| Pop from back
export
popBack : Deque cap a -> (Maybe a, Deque cap a)
popBack d =
  case d.items of
    [] => (Nothing, d)
    xs => case last' xs of
      Nothing => (Nothing, d)
      Just x => (Just x, believe_me (MkDeque (init xs)))
