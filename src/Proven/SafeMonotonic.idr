-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeMonotonic - Safe monotonic counters and timestamps
|||
||| This module provides monotonically increasing values that
||| can never decrease, useful for ordering and causality.
module Proven.SafeMonotonic
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Monotonic Counter
--------------------------------------------------------------------------------

||| A monotonically increasing counter
||| Once incremented, it can never decrease
public export
record MonotonicCounter where
  constructor MkMonotonic
  value : Nat

||| Create a new monotonic counter starting at 0
public export
zero : MonotonicCounter
zero = MkMonotonic 0

||| Create a monotonic counter with initial value
public export
fromNat : Nat -> MonotonicCounter
fromNat n = MkMonotonic n

||| Get the current value
public export
getValue : MonotonicCounter -> Nat
getValue = value

||| Increment the counter by 1
public export
increment : MonotonicCounter -> MonotonicCounter
increment (MkMonotonic n) = MkMonotonic (S n)

||| Increment the counter by a given amount
public export
incrementBy : Nat -> MonotonicCounter -> MonotonicCounter
incrementBy k (MkMonotonic n) = MkMonotonic (n + k)

||| Advance counter to at least the given value
||| If current value is higher, keeps current value
public export
advanceTo : Nat -> MonotonicCounter -> MonotonicCounter
advanceTo target (MkMonotonic n) = MkMonotonic (max target n)

||| Compare two counters
public export
Eq MonotonicCounter where
  (MkMonotonic a) == (MkMonotonic b) = a == b

public export
Ord MonotonicCounter where
  compare (MkMonotonic a) (MkMonotonic b) = compare a b

--------------------------------------------------------------------------------
-- Logical Timestamp (Lamport Clock)
--------------------------------------------------------------------------------

||| Lamport logical timestamp for distributed ordering
public export
record LamportClock where
  constructor MkLamport
  timestamp : Nat
  nodeId : Nat               -- Tie-breaker for concurrent events

||| Create a new Lamport clock
public export
newLamport : (nodeId : Nat) -> LamportClock
newLamport nid = MkLamport 0 nid

||| Local event: increment the clock
public export
tick : LamportClock -> LamportClock
tick (MkLamport ts nid) = MkLamport (S ts) nid

||| Send event: increment and return timestamp
public export
send : LamportClock -> (Nat, LamportClock)
send clock =
  let newClock = tick clock
  in (newClock.timestamp, newClock)

||| Receive event: update clock based on received timestamp
public export
receive : Nat -> LamportClock -> LamportClock
receive received (MkLamport local nid) =
  MkLamport (S (max local received)) nid

||| Compare Lamport clocks (total ordering)
public export
Eq LamportClock where
  a == b = a.timestamp == b.timestamp && a.nodeId == b.nodeId

public export
Ord LamportClock where
  compare a b =
    case compare a.timestamp b.timestamp of
      EQ => compare a.nodeId b.nodeId
      x => x

||| Check if event a happened before event b
public export
happenedBefore : LamportClock -> LamportClock -> Bool
happenedBefore a b = a.timestamp < b.timestamp

--------------------------------------------------------------------------------
-- Vector Clock
--------------------------------------------------------------------------------

||| Vector clock for tracking causality in distributed systems
public export
record VectorClock where
  constructor MkVector
  clocks : List (Nat, Nat)   -- List of (nodeId, timestamp) pairs
  selfId : Nat

||| Create a new vector clock
public export
newVector : (selfId : Nat) -> VectorClock
newVector sid = MkVector [(sid, 0)] sid

||| Get timestamp for a specific node
getNodeTime : Nat -> List (Nat, Nat) -> Nat
getNodeTime _ [] = 0
getNodeTime nid ((n, t) :: rest) = if n == nid then t else getNodeTime nid rest

||| Update timestamp for a specific node
updateNodeTime : Nat -> Nat -> List (Nat, Nat) -> List (Nat, Nat)
updateNodeTime nid newTime [] = [(nid, newTime)]
updateNodeTime nid newTime ((n, t) :: rest) =
  if n == nid then (n, newTime) :: rest
  else (n, t) :: updateNodeTime nid newTime rest

||| Local event: increment own timestamp
public export
tickVector : VectorClock -> VectorClock
tickVector vc =
  let current = getNodeTime vc.selfId vc.clocks
      newClocks = updateNodeTime vc.selfId (S current) vc.clocks
  in MkVector newClocks vc.selfId

||| Merge two vector clocks (taking max of each component)
public export
merge : VectorClock -> List (Nat, Nat) -> VectorClock
merge vc received =
  let merged = mergeClocks vc.clocks received
  in MkVector merged vc.selfId
  where
    mergeClocks : List (Nat, Nat) -> List (Nat, Nat) -> List (Nat, Nat)
    mergeClocks local [] = local
    mergeClocks local ((nid, rt) :: rest) =
      let lt = getNodeTime nid local
          newLocal = updateNodeTime nid (max lt rt) local
      in mergeClocks newLocal rest

||| Receive event with vector clock
public export
receiveVector : List (Nat, Nat) -> VectorClock -> VectorClock
receiveVector received vc = tickVector (merge vc received)

||| Check if vc1 happened before vc2
public export
vcHappenedBefore : VectorClock -> VectorClock -> Bool
vcHappenedBefore vc1 vc2 =
  all (checkComponent vc2.clocks) vc1.clocks && any (checkStrictLess vc2.clocks) vc1.clocks
  where
    checkComponent : List (Nat, Nat) -> (Nat, Nat) -> Bool
    checkComponent vc2clocks (nid, t1) = t1 <= getNodeTime nid vc2clocks

    checkStrictLess : List (Nat, Nat) -> (Nat, Nat) -> Bool
    checkStrictLess vc2clocks (nid, t1) = t1 < getNodeTime nid vc2clocks

||| Check if two vector clocks are concurrent (neither happened before)
public export
areConcurrent : VectorClock -> VectorClock -> Bool
areConcurrent vc1 vc2 = not (vcHappenedBefore vc1 vc2) && not (vcHappenedBefore vc2 vc1)

--------------------------------------------------------------------------------
-- High-Water Mark
--------------------------------------------------------------------------------

||| High-water mark for tracking processed offsets
public export
record HighWaterMark where
  constructor MkHWM
  mark : Nat
  
||| Create a new high-water mark
public export
newHWM : HighWaterMark
newHWM = MkHWM 0

||| Update high-water mark if offset is higher
public export
updateHWM : Nat -> HighWaterMark -> HighWaterMark
updateHWM offset (MkHWM current) = MkHWM (max current offset)

||| Check if an offset has been processed
public export
isProcessed : Nat -> HighWaterMark -> Bool
isProcessed offset (MkHWM mark) = offset <= mark

||| Get the current high-water mark
public export
getHWM : HighWaterMark -> Nat
getHWM = mark

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show MonotonicCounter where
  show (MkMonotonic n) = "Counter(" ++ show n ++ ")"

public export
Show LamportClock where
  show lc = "Lamport(" ++ show lc.timestamp ++ "@" ++ show lc.nodeId ++ ")"

public export
Show VectorClock where
  show vc = "Vector" ++ show vc.clocks

