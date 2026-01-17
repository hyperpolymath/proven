-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeQueue - Safe queue operations
|||
||| This module provides safe queue (FIFO) operations with
||| proper bounds checking and overflow handling.
module Proven.SafeQueue

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Queue Types
--------------------------------------------------------------------------------

||| A FIFO queue backed by two lists for O(1) amortized operations
public export
record Queue a where
  constructor MkQueue
  front : List a
  back : List a

||| A bounded queue with maximum capacity
public export
record BoundedQueue a where
  constructor MkBoundedQueue
  capacity : Nat
  queue : Queue a

--------------------------------------------------------------------------------
-- Basic Queue Operations
--------------------------------------------------------------------------------

||| Create an empty queue
public export
empty : Queue a
empty = MkQueue [] []

||| Check if queue is empty
public export
isEmpty : Queue a -> Bool
isEmpty (MkQueue [] []) = True
isEmpty _ = False

||| Get queue length
public export
length : Queue a -> Nat
length (MkQueue f b) = length f + length b

||| Enqueue an element (add to back)
public export
enqueue : a -> Queue a -> Queue a
enqueue x (MkQueue f b) = MkQueue f (x :: b)

||| Balance the queue (move back to front when front is empty)
balance : Queue a -> Queue a
balance (MkQueue [] b) = MkQueue (reverse b) []
balance q = q

||| Dequeue an element (remove from front)
public export
dequeue : Queue a -> Maybe (a, Queue a)
dequeue q =
  case balance q of
    MkQueue [] _ => Nothing
    MkQueue (x :: xs) b => Just (x, MkQueue xs b)

||| Peek at the front element without removing
public export
peek : Queue a -> Maybe a
peek q =
  case balance q of
    MkQueue [] _ => Nothing
    MkQueue (x :: _) _ => Just x

||| Convert queue to list (front to back order)
public export
toList : Queue a -> List a
toList (MkQueue f b) = f ++ reverse b

||| Create queue from list
public export
fromList : List a -> Queue a
fromList xs = MkQueue xs []

--------------------------------------------------------------------------------
-- Bounded Queue Operations
--------------------------------------------------------------------------------

||| Create an empty bounded queue
public export
emptyBounded : (capacity : Nat) -> BoundedQueue a
emptyBounded cap = MkBoundedQueue cap empty

||| Check if bounded queue is full
public export
isFull : BoundedQueue a -> Bool
isFull (MkBoundedQueue cap q) = length q >= cap

||| Get remaining capacity
public export
remaining : BoundedQueue a -> Nat
remaining (MkBoundedQueue cap q) = minus cap (length q)

||| Enqueue to bounded queue (fails if full)
public export
enqueueBounded : a -> BoundedQueue a -> Maybe (BoundedQueue a)
enqueueBounded x bq =
  if isFull bq
    then Nothing
    else Just (MkBoundedQueue bq.capacity (enqueue x bq.queue))

||| Enqueue to bounded queue (drops oldest if full)
public export
enqueueDropOldest : a -> BoundedQueue a -> BoundedQueue a
enqueueDropOldest x bq =
  if isFull bq
    then case dequeue bq.queue of
           Nothing => MkBoundedQueue bq.capacity (enqueue x empty)
           Just (_, q') => MkBoundedQueue bq.capacity (enqueue x q')
    else MkBoundedQueue bq.capacity (enqueue x bq.queue)

||| Dequeue from bounded queue
public export
dequeueBounded : BoundedQueue a -> Maybe (a, BoundedQueue a)
dequeueBounded bq =
  case dequeue bq.queue of
    Nothing => Nothing
    Just (x, q') => Just (x, MkBoundedQueue bq.capacity q')

||| Peek at bounded queue
public export
peekBounded : BoundedQueue a -> Maybe a
peekBounded bq = peek bq.queue

||| Length of bounded queue
public export
lengthBounded : BoundedQueue a -> Nat
lengthBounded bq = length bq.queue

||| Check if bounded queue is empty
public export
isEmptyBounded : BoundedQueue a -> Bool
isEmptyBounded bq = isEmpty bq.queue

--------------------------------------------------------------------------------
-- Queue Transformations
--------------------------------------------------------------------------------

||| Map a function over all elements
public export
mapQueue : (a -> b) -> Queue a -> Queue b
mapQueue f (MkQueue front back) = MkQueue (map f front) (map f back)

||| Filter elements from queue
public export
filterQueue : (a -> Bool) -> Queue a -> Queue a
filterQueue p (MkQueue front back) = MkQueue (filter p front) (filter p back)

||| Clear all elements from queue
public export
clear : Queue a -> Queue a
clear _ = empty

||| Clear bounded queue
public export
clearBounded : BoundedQueue a -> BoundedQueue a
clearBounded bq = MkBoundedQueue bq.capacity empty
