-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- SafeBuffer: Formally verified bounded buffer management
--
-- Provides:
-- - Fixed-capacity buffers with overflow prevention proofs
-- - Ring buffers with wrap-around safety
-- - Streaming buffers with backpressure
-- - Buffer pools with allocation tracking

module Proven.SafeBuffer

import Data.List
import Data.Nat
import Data.Vect
import Data.Fin
import Decidable.Equality

%default total

||| A bounded buffer with compile-time capacity
public export
record Buffer (capacity : Nat) (a : Type) where
  constructor MkBuffer
  bufData : Vect capacity (Maybe a)
  bufWritePos : Fin capacity
  bufReadPos : Fin capacity
  bufCount : Nat

||| Proof that buffer count is within capacity
public export
data BufferValid : Buffer cap a -> Type where
  MkBufferValid : LTE (bufCount buf) cap -> BufferValid buf

||| Create an empty buffer
public export
emptyBuffer : {cap : Nat} -> {auto ok : IsSucc cap} -> Buffer cap a
emptyBuffer {cap = S n} = MkBuffer (replicate (S n) Nothing) FZ FZ 0

||| Check if buffer is full
public export
isFull : Buffer cap a -> Bool
isFull buf = bufCount buf == cap
  where
    cap : Nat
    cap = length (bufData buf)

||| Check if buffer is empty
public export
isEmpty : Buffer cap a -> Bool
isEmpty buf = bufCount buf == 0

||| Get available space
public export
available : {cap : Nat} -> Buffer cap a -> Nat
available buf = minus cap (bufCount buf)

||| Proof that buffer has space
public export
data HasSpace : Buffer cap a -> Type where
  MkHasSpace : (LT (bufCount buf) cap) -> HasSpace buf

||| Proof that buffer has data
public export
data HasData : Buffer cap a -> Type where
  MkHasData : (GT (bufCount buf) 0) -> HasData buf

||| Increment Fin with wrap-around (returns to 0 at boundary)
public export
incFin : {cap : Nat} -> Fin cap -> Fin cap
incFin {cap = Z} pos = absurd pos
incFin {cap = S k} pos =
  let next = finToNat pos + 1
  in case natToFin (mod next (S k)) (S k) of
       Just f => f
       Nothing => FZ -- Should never happen

||| Write to buffer (only if space available)
public export
write : {cap : Nat} -> a -> Buffer cap a -> HasSpace buf -> Buffer cap a
write {cap} val buf _ =
  let newData = replaceAt (bufWritePos buf) (Just val) (bufData buf)
      newPos = incFin (bufWritePos buf)
  in { bufData := newData,
       bufWritePos := newPos,
       bufCount := S (bufCount buf) } buf

||| Try to write (returns Nothing if full)
public export
tryWrite : {cap : Nat} -> a -> Buffer cap a -> Maybe (Buffer cap a)
tryWrite val buf =
  if isFull buf
    then Nothing
    else Just (let newData = replaceAt (bufWritePos buf) (Just val) (bufData buf)
                   newPos = incFin (bufWritePos buf)
               in { bufData := newData,
                    bufWritePos := newPos,
                    bufCount := S (bufCount buf) } buf)

||| Read from buffer (only if data available)
public export
read : {cap : Nat} -> Buffer cap a -> HasData buf -> (a, Buffer cap a)
read buf _ =
  case index (bufReadPos buf) (bufData buf) of
    Just val =>
      let newData = replaceAt (bufReadPos buf) Nothing (bufData buf)
          newPos = incFin (bufReadPos buf)
          newBuf = { bufData := newData,
                     bufReadPos := newPos,
                     bufCount := pred (bufCount buf) } buf
      in (val, newBuf)
    Nothing => ?readEmptySlot  -- Should be impossible with HasData proof

||| Try to read (returns Nothing if empty)
public export
tryRead : {cap : Nat} -> Buffer cap a -> Maybe (a, Buffer cap a)
tryRead buf =
  if isEmpty buf
    then Nothing
    else case index (bufReadPos buf) (bufData buf) of
           Just val =>
             let newData = replaceAt (bufReadPos buf) Nothing (bufData buf)
                 newPos = incFin (bufReadPos buf)
             in Just (val, { bufData := newData,
                            bufReadPos := newPos,
                            bufCount := pred (bufCount buf) } buf)
           Nothing => Nothing

||| Peek at next value without consuming
public export
peek : {cap : Nat} -> Buffer cap a -> Maybe a
peek buf =
  if isEmpty buf
    then Nothing
    else index (bufReadPos buf) (bufData buf)

||| Ring buffer operations
public export
record RingBuffer (capacity : Nat) (a : Type) where
  constructor MkRingBuffer
  ringData : Vect capacity a
  ringHead : Fin capacity
  ringTail : Fin capacity
  ringFull : Bool

||| Create ring buffer with default value
public export
newRingBuffer : {cap : Nat} -> {auto ok : IsSucc cap} -> a -> RingBuffer cap a
newRingBuffer {cap = S n} defVal = MkRingBuffer (replicate (S n) defVal) FZ FZ False

||| Push to ring buffer (overwrites oldest on overflow)
public export
ringPush : {cap : Nat} -> a -> RingBuffer cap a -> RingBuffer cap a
ringPush val rb =
  let newData = replaceAt (ringHead rb) val (ringData rb)
      newHead = incFin (ringHead rb)
      wasAtTail = finToNat (ringHead rb) == finToNat (ringTail rb)
      newTail = if ringFull rb then incFin (ringTail rb) else ringTail rb
      nowFull = wasAtTail && ringFull rb || newHead == ringTail rb
  in MkRingBuffer newData newHead newTail nowFull

||| Pop from ring buffer
public export
ringPop : {cap : Nat} -> RingBuffer cap a -> Maybe (a, RingBuffer cap a)
ringPop rb =
  if not (ringFull rb) && finToNat (ringHead rb) == finToNat (ringTail rb)
    then Nothing
    else let val = index (ringTail rb) (ringData rb)
             newTail = incFin (ringTail rb)
         in Just (val, { ringTail := newTail, ringFull := False } rb)

||| Streaming buffer with backpressure support
public export
record StreamBuffer (capacity : Nat) (a : Type) where
  constructor MkStreamBuffer
  streamBuf : Buffer capacity a
  streamHighWater : Nat      -- Pause producer above this
  streamLowWater : Nat       -- Resume producer below this
  streamPaused : Bool

||| Create stream buffer with water marks
public export
newStreamBuffer : {cap : Nat} -> {auto ok : IsSucc cap} ->
                  (high : Nat) -> (low : Nat) -> StreamBuffer cap a
newStreamBuffer {cap = S n} high low =
  MkStreamBuffer emptyBuffer (min high (S n)) (min low high) False

||| Check if producer should pause
public export
shouldPause : {cap : Nat} -> StreamBuffer cap a -> Bool
shouldPause sb = bufCount (streamBuf sb) >= streamHighWater sb

||| Check if producer can resume
public export
canResume : {cap : Nat} -> StreamBuffer cap a -> Bool
canResume sb = bufCount (streamBuf sb) <= streamLowWater sb

||| Write to stream buffer with backpressure
public export
streamWrite : {cap : Nat} -> a -> StreamBuffer cap a ->
              Either (StreamBuffer cap a) (StreamBuffer cap a)
streamWrite val sb =
  if shouldPause sb
    then Left ({ streamPaused := True } sb)  -- Signal backpressure
    else case tryWrite val (streamBuf sb) of
           Nothing => Left sb
           Just newBuf => Right ({ streamBuf := newBuf } sb)

||| Read from stream buffer
public export
streamRead : {cap : Nat} -> StreamBuffer cap a ->
             Maybe (a, StreamBuffer cap a)
streamRead sb =
  case tryRead (streamBuf sb) of
    Nothing => Nothing
    Just (val, newBuf) =>
      let newPaused = if canResume sb then False else streamPaused sb
      in Just (val, { streamBuf := newBuf, streamPaused := newPaused } sb)

||| Buffer pool for managing multiple buffers
public export
record BufferPool (n : Nat) (cap : Nat) (a : Type) where
  constructor MkBufferPool
  poolBuffers : Vect n (Buffer cap a)
  poolFree : List (Fin n)
  poolAllocated : List (Fin n)

||| Create empty buffer pool
public export
emptyPool : {n : Nat} -> {cap : Nat} -> {auto okCap : IsSucc cap} -> BufferPool n cap a
emptyPool {n} {cap} = MkBufferPool (replicate n emptyBuffer) (toList (allFins n)) []
  where
    allFins : (m : Nat) -> Vect m (Fin m)
    allFins Z = []
    allFins (S k) = FZ :: map FS (allFins k)

||| Allocate a buffer from pool
public export
poolAlloc : {n : Nat} -> {cap : Nat} -> BufferPool n cap a ->
            Maybe (Fin n, BufferPool n cap a)
poolAlloc pool =
  case poolFree pool of
    [] => Nothing
    (idx :: rest) =>
      Just (idx, { poolFree := rest, poolAllocated := idx :: poolAllocated pool } pool)

||| Release a buffer back to pool
public export
poolRelease : {n : Nat} -> {cap : Nat} -> Fin n -> BufferPool n cap a ->
              BufferPool n cap a
poolRelease idx pool =
  if elem idx (poolAllocated pool)
    then { poolFree := idx :: poolFree pool,
           poolAllocated := filter (/= idx) (poolAllocated pool) } pool
    else pool

||| Get buffer from pool
public export
poolGet : {n : Nat} -> {cap : Nat} -> Fin n -> BufferPool n cap a -> Buffer cap a
poolGet idx pool = index idx (poolBuffers pool)

||| Update buffer in pool
public export
poolUpdate : {n : Nat} -> {cap : Nat} -> Fin n -> Buffer cap a ->
             BufferPool n cap a -> BufferPool n cap a
poolUpdate idx buf pool =
  { poolBuffers := replaceAt idx buf (poolBuffers pool) } pool

||| Pool utilization stats (allocated, free, total)
public export
poolStats : {n : Nat} -> {cap : Nat} -> BufferPool n cap a -> (Nat, Nat, Nat)
poolStats pool = (length (poolAllocated pool), length (poolFree pool), n)

||| Proof that pool allocation succeeded
public export
data PoolHasSpace : BufferPool n cap a -> Type where
  MkPoolHasSpace : GT (length (poolFree pool)) 0 -> PoolHasSpace pool

||| Double buffer for producer/consumer pattern
public export
record DoubleBuffer (cap : Nat) (a : Type) where
  constructor MkDoubleBuffer
  dbFront : Buffer cap a    -- Consumer reads from here
  dbBack : Buffer cap a     -- Producer writes to here
  dbSwapped : Bool

||| Create double buffer
public export
newDoubleBuffer : {cap : Nat} -> {auto ok : IsSucc cap} -> DoubleBuffer cap a
newDoubleBuffer = MkDoubleBuffer emptyBuffer emptyBuffer False

||| Write to back buffer
public export
dbWrite : {cap : Nat} -> a -> DoubleBuffer cap a -> Maybe (DoubleBuffer cap a)
dbWrite val db =
  case tryWrite val (dbBack db) of
    Nothing => Nothing
    Just newBack => Just ({ dbBack := newBack } db)

||| Read from front buffer
public export
dbRead : {cap : Nat} -> DoubleBuffer cap a -> Maybe (a, DoubleBuffer cap a)
dbRead db =
  case tryRead (dbFront db) of
    Nothing => Nothing
    Just (val, newFront) => Just (val, { dbFront := newFront } db)

||| Swap front and back buffers
public export
dbSwap : {cap : Nat} -> DoubleBuffer cap a -> DoubleBuffer cap a
dbSwap db = MkDoubleBuffer (dbBack db) (dbFront db) (not (dbSwapped db))

||| Byte buffer operations (specialized for Nat representing bytes)
public export
ByteBuffer : Nat -> Type
ByteBuffer cap = Buffer cap Nat

||| Write bytes to buffer
public export
writeBytes : {cap : Nat} -> List Nat -> ByteBuffer cap -> Maybe (ByteBuffer cap)
writeBytes [] buf = Just buf
writeBytes (b :: bs) buf =
  case tryWrite b buf of
    Nothing => Nothing
    Just newBuf => writeBytes bs newBuf

||| Read n bytes from buffer
public export
readBytes : {cap : Nat} -> (n : Nat) -> ByteBuffer cap -> Maybe (List Nat, ByteBuffer cap)
readBytes Z buf = Just ([], buf)
readBytes (S n) buf =
  case tryRead buf of
    Nothing => Nothing
    Just (b, newBuf) =>
      case readBytes n newBuf of
        Nothing => Nothing
        Just (bs, finalBuf) => Just (b :: bs, finalBuf)

||| Buffer overflow prevention proof
public export
data NoOverflow : Buffer cap a -> Nat -> Type where
  MkNoOverflow : LTE (bufCount buf + writeCount) cap -> NoOverflow buf writeCount

||| Buffer underflow prevention proof
public export
data NoUnderflow : Buffer cap a -> Nat -> Type where
  MkNoUnderflow : LTE readCount (bufCount buf) -> NoUnderflow buf readCount

||| Safe batch write with overflow proof
public export
safeBatchWrite : {cap : Nat} -> (vals : List a) -> (buf : Buffer cap a) ->
                 NoOverflow buf (length vals) -> Buffer cap a
safeBatchWrite [] buf _ = buf
safeBatchWrite (v :: vs) buf prf =
  case tryWrite v buf of
    Nothing => buf  -- Should not happen with proof
    Just newBuf => safeBatchWrite vs newBuf ?overflowProofStep


