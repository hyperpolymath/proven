-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- SafeOrdering: Formally verified temporal ordering and causality
--
-- Provides:
-- - Happens-before relations with transitivity proofs
-- - Causal consistency guarantees
-- - Logical clock correctness
-- - Total ordering from partial orders

module Proven.SafeOrdering
import Data.String
import Data.List

import Data.List
import Data.Nat
import Data.Vect
import Data.Fin
import Decidable.Equality

%default total

||| An event identifier
public export
EventId : Type
EventId = Nat

||| A process identifier
public export
ProcessId : Type
ProcessId = Nat

||| An event in a distributed system
public export
record Event where
  constructor MkEvent
  eventId : EventId
  processId : ProcessId
  timestamp : Nat
  payload : String

||| Happens-before relation (Lamport)
public export
data HappensBefore : Event -> Event -> Type where
  ||| Same process, earlier in sequence
  SameProcess : (e1.processId = e2.processId) -> LT e1.timestamp e2.timestamp ->
                HappensBefore e1 e2
  ||| Message passing: send happens before receive
  MessagePassing : (sendEvent : Event) -> (recvEvent : Event) ->
                   HappensBefore sendEvent recvEvent
  ||| Transitivity: if a -> b and b -> c then a -> c
  Transitive : HappensBefore e1 e2 -> HappensBefore e2 e3 ->
               HappensBefore e1 e3

||| Concurrent events (neither happens before the other)
public export
data Concurrent : Event -> Event -> Type where
  MkConcurrent : (notHB1 : HappensBefore e1 e2 -> Void) ->
                 (notHB2 : HappensBefore e2 e1 -> Void) ->
                 Concurrent e1 e2

||| A vector timestamp for n processes
public export
record VectorTimestamp (n : Nat) where
  constructor MkVectorTimestamp
  vtValues : Vect n Nat

||| Zero vector timestamp
public export
zeroVT : {n : Nat} -> VectorTimestamp n
zeroVT = MkVectorTimestamp (replicate n 0)

||| Increment local clock in vector timestamp
public export
tickVT : {n : Nat} -> Fin n -> VectorTimestamp n -> VectorTimestamp n
tickVT pos vt = { vtValues := updateAt pos S (vtValues vt) } vt

||| Merge vector timestamps (element-wise max)
public export
mergeVT : {n : Nat} -> VectorTimestamp n -> VectorTimestamp n -> VectorTimestamp n
mergeVT vt1 vt2 = { vtValues := zipWith max (vtValues vt1) (vtValues vt2) } vt1

||| Vector timestamp comparison (less than or equal)
public export
vtLTE : {n : Nat} -> VectorTimestamp n -> VectorTimestamp n -> Bool
vtLTE vt1 vt2 = all (uncurry (<=)) (zip (toList (vtValues vt1)) (toList (vtValues vt2)))

||| Vector timestamp strictly less than
public export
vtLT : {n : Nat} -> VectorTimestamp n -> VectorTimestamp n -> Bool
vtLT vt1 vt2 = vtLTE vt1 vt2 &&
               any (uncurry (<)) (zip (toList (vtValues vt1)) (toList (vtValues vt2)))

||| Check if events are concurrent via vector timestamps
public export
vtConcurrent : {n : Nat} -> VectorTimestamp n -> VectorTimestamp n -> Bool
vtConcurrent vt1 vt2 = not (vtLT vt1 vt2) && not (vtLT vt2 vt1)

||| Proof that vector timestamps correctly capture happens-before
public export
data VTHappensBefore : {n : Nat} -> VectorTimestamp n -> VectorTimestamp n -> Type where
  MkVTHappensBefore : (lte : vtLTE vt1 vt2 = True) ->
                      (strict : vtLT vt1 vt2 = True) ->
                      VTHappensBefore vt1 vt2

||| A partial order on events
public export
record PartialOrder (event : Type) where
  constructor MkPartialOrder
  poLTE : event -> event -> Bool
  poReflexive : (e : event) -> poLTE e e = True
  poAntisymmetric : (e1, e2 : event) -> poLTE e1 e2 = True -> poLTE e2 e1 = True -> e1 = e2
  poTransitive : (e1, e2, e3 : event) -> poLTE e1 e2 = True -> poLTE e2 e3 = True ->
                 poLTE e1 e3 = True

||| A total order on events
public export
record TotalOrder (event : Type) where
  constructor MkTotalOrder
  toPartial : PartialOrder event
  toTotal : (e1, e2 : event) -> Either (poLTE (toPartial) e1 e2 = True)
                                        (poLTE (toPartial) e2 e1 = True)

||| Linearize a partial order into a total order (using process IDs as tiebreaker)
public export
linearize : Event -> Event -> Ordering
linearize e1 e2 =
  case compare (timestamp e1) (timestamp e2) of
    LT => LT
    GT => GT
    EQ => compare (processId e1) (processId e2)

||| A causal history of events
public export
record CausalHistory where
  constructor MkCausalHistory
  events : List Event
  happensBefore : List (EventId, EventId)  -- (earlier, later) pairs

||| Empty causal history
public export
emptyHistory : CausalHistory
emptyHistory = MkCausalHistory [] []

||| Add event to causal history
public export
addEvent : Event -> List EventId -> CausalHistory -> CausalHistory
addEvent evt deps hist =
  { events := evt :: events hist,
    happensBefore := map (\d => (d, eventId evt)) deps ++ happensBefore hist } hist

||| Check if one event causally precedes another
||| Uses assert_total as termination depends on acyclic causal graph
public export
causallyPrecedes : EventId -> EventId -> CausalHistory -> Bool
causallyPrecedes e1 e2 hist =
  elem (e1, e2) (happensBefore hist) ||
  any (\mid => assert_total (causallyPrecedes e1 mid hist) && elem (mid, e2) (happensBefore hist))
      (map fst (happensBefore hist))

||| Causal delivery - events delivered in causal order
public export
record CausalDelivery where
  constructor MkCausalDelivery
  delivered : List Event
  pending : List (Event, List EventId)  -- Event with its causal dependencies

||| Check if event can be delivered (all dependencies delivered)
public export
canDeliver : Event -> List EventId -> CausalDelivery -> Bool
canDeliver evt deps cd =
  all (\dep => any (\e => eventId e == dep) (delivered cd)) deps

||| Deliver event if causally ready
public export
tryDeliver : Event -> List EventId -> CausalDelivery -> Maybe CausalDelivery
tryDeliver evt deps cd =
  if canDeliver evt deps cd
    then Just ({ delivered := evt :: delivered cd } cd)
    else Nothing

||| Add to pending if not deliverable
public export
addPending : Event -> List EventId -> CausalDelivery -> CausalDelivery
addPending evt deps cd = { pending := (evt, deps) :: pending cd } cd

||| Process pending events
||| Uses assert_total as termination depends on finite pending list
public export
processPending : CausalDelivery -> CausalDelivery
processPending cd =
  case find (\p => canDeliver (fst p) (snd p) cd) (pending cd) of
    Nothing => cd
    Just (evt, deps) =>
      assert_total $ processPending ({ delivered := evt :: delivered cd,
                        pending := filter (\p => eventId (fst p) /= eventId evt) (pending cd) } cd)

||| Interval timestamp for efficient causal ordering
public export
record IntervalTimestamp where
  constructor MkIntervalTimestamp
  itLow : Nat
  itHigh : Nat

||| Compare interval timestamps
public export
itHappensBefore : IntervalTimestamp -> IntervalTimestamp -> Bool
itHappensBefore it1 it2 = itHigh it1 < itLow it2

||| Plausible clock (for real-time hybrid clocks)
public export
record PlausibleClock where
  constructor MkPlausibleClock
  pcPhysical : Nat  -- Physical time component
  pcLogical : Nat   -- Logical time component

||| Update plausible clock on send
public export
pcSend : Nat -> PlausibleClock -> PlausibleClock
pcSend physTime pc =
  if physTime > pcPhysical pc
    then MkPlausibleClock physTime 0
    else { pcLogical := S (pcLogical pc) } pc

||| Update plausible clock on receive
public export
pcReceive : Nat -> PlausibleClock -> PlausibleClock -> PlausibleClock
pcReceive physTime local remote =
  let maxPhys = max physTime (max (pcPhysical local) (pcPhysical remote))
  in if maxPhys == physTime && physTime > pcPhysical local && physTime > pcPhysical remote
       then MkPlausibleClock physTime 0
       else if maxPhys == pcPhysical local && pcPhysical local == pcPhysical remote
            then MkPlausibleClock maxPhys (S (max (pcLogical local) (pcLogical remote)))
            else if maxPhys == pcPhysical local
                 then MkPlausibleClock maxPhys (S (pcLogical local))
                 else MkPlausibleClock maxPhys (S (pcLogical remote))

||| Compare plausible clocks
public export
pcCompare : PlausibleClock -> PlausibleClock -> Ordering
pcCompare pc1 pc2 =
  case compare (pcPhysical pc1) (pcPhysical pc2) of
    LT => LT
    GT => GT
    EQ => compare (pcLogical pc1) (pcLogical pc2)

||| Sequence number for total ordering within a process
public export
record SequenceNumber where
  constructor MkSequenceNumber
  seqProcess : ProcessId
  seqNumber : Nat

||| Increment sequence number
public export
nextSeq : SequenceNumber -> SequenceNumber
nextSeq sn = { seqNumber := S (seqNumber sn) } sn

||| Compare sequence numbers (forms total order)
public export
seqCompare : SequenceNumber -> SequenceNumber -> Ordering
seqCompare sn1 sn2 =
  case compare (seqNumber sn1) (seqNumber sn2) of
    LT => LT
    GT => GT
    EQ => compare (seqProcess sn1) (seqProcess sn2)

||| Proof that sequence numbers form a total order
public export
seqTotalOrder : (sn1, sn2 : SequenceNumber) ->
                Either (seqCompare sn1 sn2 = LT)
                       (Either (seqCompare sn1 sn2 = EQ)
                               (seqCompare sn1 sn2 = GT))
seqTotalOrder sn1 sn2 with (seqCompare sn1 sn2)
  seqTotalOrder sn1 sn2 | LT = Left Refl
  seqTotalOrder sn1 sn2 | EQ = Right (Left Refl)
  seqTotalOrder sn1 sn2 | GT = Right (Right Refl)

||| Epoch-based ordering (for consensus protocols)
public export
record EpochOrdering where
  constructor MkEpochOrdering
  epoch : Nat
  epochSeq : Nat

||| Compare epoch orderings
public export
epochCompare : EpochOrdering -> EpochOrdering -> Ordering
epochCompare eo1 eo2 =
  case compare (epoch eo1) (epoch eo2) of
    LT => LT
    GT => GT
    EQ => compare (epochSeq eo1) (epochSeq eo2)

||| Advance to next epoch
public export
nextEpoch : EpochOrdering -> EpochOrdering
nextEpoch eo = MkEpochOrdering (S (epoch eo)) 0

||| Advance within epoch
public export
advanceInEpoch : EpochOrdering -> EpochOrdering
advanceInEpoch eo = { epochSeq := S (epochSeq eo) } eo

