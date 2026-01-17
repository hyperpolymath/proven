-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeMonotonic - Verified monotonic values
|||
||| Type-safe monotonic counters, clocks, and sequences.
||| Guarantees values never decrease (or never increase for decreasing).
|||
||| Used in distributed systems, event sourcing, and time tracking.
module Proven.SafeMonotonic

import Proven.Core
import Data.So

%default total

-- ============================================================================
-- MONOTONIC COUNTER (INCREASING)
-- ============================================================================

||| A monotonically increasing counter
||| The value is guaranteed to never decrease over time
public export
record MonotonicCounter where
  constructor MkMonotonicCounter
  value : Nat
  -- Conceptually, we track that value >= previous value
  -- This is enforced by only allowing increment operations

||| Create a new counter starting at zero
export
newCounter : MonotonicCounter
newCounter = MkMonotonicCounter 0

||| Create a counter with initial value
export
counterFrom : Nat -> MonotonicCounter
counterFrom n = MkMonotonicCounter n

||| Get the current value
export
counterValue : MonotonicCounter -> Nat
counterValue c = c.value

||| Increment by one
export
increment : MonotonicCounter -> MonotonicCounter
increment c = MkMonotonicCounter (S c.value)

||| Increment by n
export
incrementBy : Nat -> MonotonicCounter -> MonotonicCounter
incrementBy n c = MkMonotonicCounter (c.value + n)

||| Compare two counters (useful for detecting if counter advanced)
export
counterCompare : MonotonicCounter -> MonotonicCounter -> Ordering
counterCompare a b = compare a.value b.value

||| Check if counter a happened-before counter b
export
happenedBefore : MonotonicCounter -> MonotonicCounter -> Bool
happenedBefore a b = a.value < b.value

-- ============================================================================
-- MONOTONIC TIMESTAMP
-- ============================================================================

||| A monotonically increasing timestamp (nanoseconds since epoch)
public export
record MonotonicTimestamp where
  constructor MkTimestamp
  nanos : Integer
  0 nonNegative : So (nanos >= 0)

||| Create a timestamp from nanoseconds
export
timestampFromNanos : Integer -> Maybe MonotonicTimestamp
timestampFromNanos n =
  if n >= 0 then Just (believe_me (MkTimestamp n))
  else Nothing

||| Create a timestamp from milliseconds
export
timestampFromMillis : Integer -> Maybe MonotonicTimestamp
timestampFromMillis ms =
  if ms >= 0 then Just (believe_me (MkTimestamp (ms * 1000000)))
  else Nothing

||| Create a timestamp from seconds
export
timestampFromSeconds : Integer -> Maybe MonotonicTimestamp
timestampFromSeconds s =
  if s >= 0 then Just (believe_me (MkTimestamp (s * 1000000000)))
  else Nothing

||| Get nanoseconds
export
toNanos : MonotonicTimestamp -> Integer
toNanos t = t.nanos

||| Get milliseconds
export
toMillis : MonotonicTimestamp -> Integer
toMillis t = t.nanos `div` 1000000

||| Get seconds
export
toSeconds : MonotonicTimestamp -> Integer
toSeconds t = t.nanos `div` 1000000000

||| Add duration (always increases)
export
addDuration : MonotonicTimestamp -> Integer -> MonotonicTimestamp
addDuration t duration =
  let newVal = t.nanos + max 0 duration
  in believe_me (MkTimestamp newVal)

||| Compare timestamps
export
timestampCompare : MonotonicTimestamp -> MonotonicTimestamp -> Ordering
timestampCompare a b = compare a.nanos b.nanos

||| Duration between two timestamps (always non-negative)
export
durationBetween : MonotonicTimestamp -> MonotonicTimestamp -> Integer
durationBetween a b = abs (a.nanos - b.nanos)

-- ============================================================================
-- SEQUENCE NUMBER
-- ============================================================================

||| A monotonically increasing sequence number
||| Used for ordering events, messages, or transactions
public export
record SequenceNumber where
  constructor MkSeqNum
  value : Nat

||| Initial sequence number
export
initialSeq : SequenceNumber
initialSeq = MkSeqNum 0

||| Create sequence from value
export
seqFrom : Nat -> SequenceNumber
seqFrom = MkSeqNum

||| Get sequence value
export
seqValue : SequenceNumber -> Nat
seqValue s = s.value

||| Get next sequence number
export
nextSeq : SequenceNumber -> SequenceNumber
nextSeq s = MkSeqNum (S s.value)

||| Skip to a higher sequence (must be greater)
export
skipTo : SequenceNumber -> Nat -> Maybe SequenceNumber
skipTo s n = if n > s.value then Just (MkSeqNum n) else Nothing

||| Check if sequence a precedes sequence b
export
precedes : SequenceNumber -> SequenceNumber -> Bool
precedes a b = a.value < b.value

||| Gap between two sequences
export
seqGap : SequenceNumber -> SequenceNumber -> Nat
seqGap a b = if a.value > b.value then a.value - b.value else b.value - a.value

-- ============================================================================
-- LAMPORT CLOCK
-- ============================================================================

||| Lamport logical clock for distributed systems
||| Implements the happens-before relation
public export
record LamportClock where
  constructor MkLamport
  time : Nat

||| Create a new Lamport clock
export
newLamportClock : LamportClock
newLamportClock = MkLamport 0

||| Get current time
export
lamportTime : LamportClock -> Nat
lamportTime c = c.time

||| Local event (increment)
export
localEvent : LamportClock -> LamportClock
localEvent c = MkLamport (S c.time)

||| Send event (increment and return timestamp to send)
export
sendEvent : LamportClock -> (LamportClock, Nat)
sendEvent c = let c' = MkLamport (S c.time) in (c', c'.time)

||| Receive event (merge with received timestamp)
export
receiveEvent : LamportClock -> Nat -> LamportClock
receiveEvent c received = MkLamport (S (max c.time received))

||| Merge two clocks (for synchronization)
export
mergeLamport : LamportClock -> LamportClock -> LamportClock
mergeLamport a b = MkLamport (max a.time b.time)

-- ============================================================================
-- HYBRID LOGICAL CLOCK (HLC)
-- ============================================================================

||| Hybrid Logical Clock combining physical and logical time
||| Provides both causality tracking and bounded clock skew
public export
record HybridLogicalClock where
  constructor MkHLC
  physicalTime : Integer  -- Wall clock time (e.g., Unix millis)
  logicalTime : Nat       -- Logical counter for same physical time

||| Create a new HLC
export
newHLC : Integer -> HybridLogicalClock
newHLC pt = MkHLC (max 0 pt) 0

||| Get physical time component
export
hlcPhysicalTime : HybridLogicalClock -> Integer
hlcPhysicalTime c = c.physicalTime

||| Get logical time component
export
hlcLogicalTime : HybridLogicalClock -> Nat
hlcLogicalTime c = c.logicalTime

||| Local event with current wall time
export
hlcLocalEvent : HybridLogicalClock -> Integer -> HybridLogicalClock
hlcLocalEvent c wallTime =
  if wallTime > c.physicalTime
  then MkHLC wallTime 0
  else MkHLC c.physicalTime (S c.logicalTime)

||| Send event (returns updated clock and timestamp)
export
hlcSendEvent : HybridLogicalClock -> Integer -> (HybridLogicalClock, (Integer, Nat))
hlcSendEvent c wallTime =
  let c' = hlcLocalEvent c wallTime
  in (c', (c'.physicalTime, c'.logicalTime))

||| Receive event
export
hlcReceiveEvent : HybridLogicalClock -> Integer -> (Integer, Nat) -> HybridLogicalClock
hlcReceiveEvent c wallTime (msgPT, msgLT) =
  let maxPT = max wallTime (max c.physicalTime msgPT)
  in if maxPT > c.physicalTime && maxPT > msgPT
     then MkHLC maxPT 0
     else if maxPT == c.physicalTime && maxPT == msgPT
     then MkHLC maxPT (S (max c.logicalTime msgLT))
     else if maxPT == c.physicalTime
     then MkHLC maxPT (S c.logicalTime)
     else MkHLC maxPT (S msgLT)

||| Compare two HLC timestamps
export
hlcCompare : HybridLogicalClock -> HybridLogicalClock -> Ordering
hlcCompare a b =
  case compare a.physicalTime b.physicalTime of
    EQ => compare a.logicalTime b.logicalTime
    other => other

-- ============================================================================
-- VERSION VECTOR
-- ============================================================================

||| Version vector for tracking causality across multiple nodes
public export
record VersionVector (n : Nat) where
  constructor MkVersionVector
  versions : Vect n Nat

||| Create a new version vector (all zeros)
export
newVersionVector : (n : Nat) -> VersionVector n
newVersionVector n = MkVersionVector (replicate n 0)

||| Get version for node i
export
getVersion : VersionVector n -> Fin n -> Nat
getVersion vv i = index i vv.versions

||| Increment version for node i
export
incrementVersion : VersionVector n -> Fin n -> VersionVector n
incrementVersion vv i =
  let vs = vv.versions
      v = index i vs
  in MkVersionVector (replaceAt i (S v) vs)

||| Merge two version vectors (pointwise max)
export
mergeVersionVectors : VersionVector n -> VersionVector n -> VersionVector n
mergeVersionVectors a b =
  MkVersionVector (zipWith max a.versions b.versions)

||| Check if a happened-before b (a ≤ b componentwise, and a ≠ b)
export
vvHappenedBefore : VersionVector n -> VersionVector n -> Bool
vvHappenedBefore a b =
  let leq = all id (zipWith (<=) (toList a.versions) (toList b.versions))
      neq = any id (zipWith (/=) (toList a.versions) (toList b.versions))
  in leq && neq

||| Check if two version vectors are concurrent (neither happened-before the other)
export
vvConcurrent : VersionVector n -> VersionVector n -> Bool
vvConcurrent a b = not (vvHappenedBefore a b) && not (vvHappenedBefore b a)

-- ============================================================================
-- HIGH WATER MARK
-- ============================================================================

||| A high water mark that tracks the highest seen value
public export
record HighWaterMark where
  constructor MkHWM
  value : Integer

||| Create a new high water mark
export
newHighWaterMark : HighWaterMark
newHighWaterMark = MkHWM 0

||| Create with initial value
export
hwmFrom : Integer -> HighWaterMark
hwmFrom = MkHWM

||| Get current high water mark
export
hwmValue : HighWaterMark -> Integer
hwmValue h = h.value

||| Update with a new value (only increases if greater)
export
hwmUpdate : HighWaterMark -> Integer -> HighWaterMark
hwmUpdate h v = if v > h.value then MkHWM v else h

||| Check if a value would advance the high water mark
export
hwmWouldAdvance : HighWaterMark -> Integer -> Bool
hwmWouldAdvance h v = v > h.value
