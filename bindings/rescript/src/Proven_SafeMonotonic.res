// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeMonotonic - Monotonic counters and logical clocks.
 *
 * Provides safe monotonic sequence operations that cannot crash:
 * - Monotonic counter (can never decrease)
 * - Lamport logical clock for distributed ordering
 * - High-water mark for offset tracking
 * - Vector clock for causality tracking
 *
 * All operations maintain monotonicity guarantees.
 */

// ============================================================================
// Types
// ============================================================================

/** Monotonically increasing counter */
type monotonicCounter = {
  mutable value: float,
}

/** Lamport logical clock for distributed ordering */
type lamportClock = {
  mutable timestamp: float,
  nodeId: float,
}

/** High-water mark for tracking processed offsets */
type highWaterMark = {
  mutable mark: float,
}

/** Vector clock entry */
type vectorClockEntry = {
  nodeId: float,
  mutable timestamp: float,
}

/** Vector clock for causality tracking */
type vectorClock = {
  selfId: float,
  mutable entries: array<vectorClockEntry>,
}

/** Clock comparison result */
type clockOrder =
  | Before      // Happened before
  | After       // Happened after
  | Concurrent  // Concurrent (no causal relationship)
  | Equal       // Same clock value

// ============================================================================
// Monotonic Counter
// ============================================================================

/** Create a new monotonic counter starting at 0 */
let makeCounter = (): monotonicCounter => {
  {value: 0.0}
}

/** Create a monotonic counter with initial value */
let makeCounterFrom = (initialValue: float): result<monotonicCounter, string> => {
  if initialValue < 0.0 {
    Error("Initial value must be non-negative")
  } else {
    Ok({value: initialValue})
  }
}

/** Get current counter value */
let getCounterValue = (counter: monotonicCounter): float => {
  counter.value
}

/** Increment counter by 1, returning new value */
let incrementCounter = (counter: monotonicCounter): float => {
  counter.value = counter.value +. 1.0
  counter.value
}

/** Increment counter by specified amount, returning new value */
let incrementCounterBy = (counter: monotonicCounter, amount: float): result<float, string> => {
  if amount < 0.0 {
    Error("Increment amount must be non-negative")
  } else {
    counter.value = counter.value +. amount
    Ok(counter.value)
  }
}

/** Advance counter to at least target value (keeps current if higher) */
let advanceCounterTo = (counter: monotonicCounter, target: float): float => {
  counter.value = Js.Math.max_float(counter.value, target)
  counter.value
}

/** Compare two counters */
let compareCounters = (a: monotonicCounter, b: monotonicCounter): int => {
  if a.value < b.value {
    -1
  } else if a.value > b.value {
    1
  } else {
    0
  }
}

// ============================================================================
// Lamport Logical Clock
// ============================================================================

/** Create a new Lamport clock with node ID */
let makeLamportClock = (nodeId: float): lamportClock => {
  {timestamp: 0.0, nodeId}
}

/** Get current timestamp */
let getLamportTimestamp = (clock: lamportClock): float => {
  clock.timestamp
}

/** Get node ID */
let getLamportNodeId = (clock: lamportClock): float => {
  clock.nodeId
}

/** Local event: increment the clock */
let tick = (clock: lamportClock): float => {
  clock.timestamp = clock.timestamp +. 1.0
  clock.timestamp
}

/** Send event: increment and return timestamp for sending */
let send = (clock: lamportClock): float => {
  tick(clock)
}

/** Receive event: update clock based on received timestamp */
let receive = (clock: lamportClock, receivedTimestamp: float): float => {
  clock.timestamp = Js.Math.max_float(clock.timestamp, receivedTimestamp) +. 1.0
  clock.timestamp
}

/** Compare two Lamport clocks (total ordering using node ID as tiebreaker) */
let compareLamportClocks = (a: lamportClock, b: lamportClock): clockOrder => {
  if a.timestamp < b.timestamp {
    Before
  } else if a.timestamp > b.timestamp {
    After
  } else if a.nodeId < b.nodeId {
    Before
  } else if a.nodeId > b.nodeId {
    After
  } else {
    Equal
  }
}

/** Check if event a happened before event b */
let happenedBefore = (a: lamportClock, b: lamportClock): bool => {
  a.timestamp < b.timestamp
}

/** Check if event a happened after event b */
let happenedAfter = (a: lamportClock, b: lamportClock): bool => {
  a.timestamp > b.timestamp
}

/** Check if two events are concurrent */
let areConcurrent = (a: lamportClock, b: lamportClock): bool => {
  a.timestamp == b.timestamp && a.nodeId != b.nodeId
}

// ============================================================================
// High-Water Mark
// ============================================================================

/** Create a new high-water mark starting at 0 */
let makeHighWaterMark = (): highWaterMark => {
  {mark: 0.0}
}

/** Create a high-water mark with initial value */
let makeHighWaterMarkFrom = (initialValue: float): result<highWaterMark, string> => {
  if initialValue < 0.0 {
    Error("Initial value must be non-negative")
  } else {
    Ok({mark: initialValue})
  }
}

/** Get current high-water mark value */
let getHighWaterMark = (hwm: highWaterMark): float => {
  hwm.mark
}

/** Update high-water mark if offset is higher */
let updateHighWaterMark = (hwm: highWaterMark, offset: float): unit => {
  hwm.mark = Js.Math.max_float(hwm.mark, offset)
}

/** Check if an offset has been processed (is at or below the mark) */
let isProcessed = (hwm: highWaterMark, offset: float): bool => {
  offset <= hwm.mark
}

/** Check if an offset is pending (is above the mark) */
let isPending = (hwm: highWaterMark, offset: float): bool => {
  offset > hwm.mark
}

/** Get the gap between an offset and the high-water mark */
let getGap = (hwm: highWaterMark, offset: float): float => {
  offset -. hwm.mark
}

// ============================================================================
// Vector Clock
// ============================================================================

/** Create a new vector clock with self ID */
let makeVectorClock = (selfId: float): vectorClock => {
  {
    selfId,
    entries: [{nodeId: selfId, timestamp: 0.0}],
  }
}

/** Find entry for a node ID, or create if not exists */
let findOrCreateEntry = (vc: vectorClock, nodeId: float): vectorClockEntry => {
  let maybeEntry = Belt.Array.getBy(vc.entries, entry => entry.nodeId == nodeId)
  switch maybeEntry {
  | Some(entry) => entry
  | None => {
      let newEntry = {nodeId, timestamp: 0.0}
      let _ = Js.Array2.push(vc.entries, newEntry)
      newEntry
    }
  }
}

/** Get timestamp for a specific node */
let getVectorTimestamp = (vc: vectorClock, nodeId: float): float => {
  let entry = findOrCreateEntry(vc, nodeId)
  entry.timestamp
}

/** Get self timestamp */
let getSelfTimestamp = (vc: vectorClock): float => {
  getVectorTimestamp(vc, vc.selfId)
}

/** Tick the local clock */
let tickVectorClock = (vc: vectorClock): unit => {
  let entry = findOrCreateEntry(vc, vc.selfId)
  entry.timestamp = entry.timestamp +. 1.0
}

/** Merge another vector clock into this one */
let mergeVectorClocks = (vc: vectorClock, other: array<vectorClockEntry>): unit => {
  Belt.Array.forEach(other, otherEntry => {
    let entry = findOrCreateEntry(vc, otherEntry.nodeId)
    entry.timestamp = Js.Math.max_float(entry.timestamp, otherEntry.timestamp)
  })
}

/** Send event: tick and return entries for sending */
let sendVectorClock = (vc: vectorClock): array<vectorClockEntry> => {
  tickVectorClock(vc)
  Belt.Array.map(vc.entries, entry => {nodeId: entry.nodeId, timestamp: entry.timestamp})
}

/** Receive event: merge received entries and tick */
let receiveVectorClock = (vc: vectorClock, received: array<vectorClockEntry>): unit => {
  mergeVectorClocks(vc, received)
  tickVectorClock(vc)
}

/** Compare two vector clocks */
let compareVectorClocks = (a: vectorClock, b: vectorClock): clockOrder => {
  let aEntries = a.entries
  let bEntries = b.entries

  // Collect all node IDs
  let allNodeIds = Belt.Set.fromArray(
    Belt.Array.concat(
      Belt.Array.map(aEntries, e => e.nodeId),
      Belt.Array.map(bEntries, e => e.nodeId),
    ),
    ~id=module(Belt.Id.MakeComparable({
      type t = float
      let cmp = (x, y) =>
        if x < y {
          -1
        } else if x > y {
          1
        } else {
          0
        }
    })),
  )

  let aLessOrEqual = ref(true)
  let bLessOrEqual = ref(true)
  let equal = ref(true)

  Belt.Set.forEach(allNodeIds, nodeId => {
    let aTs = switch Belt.Array.getBy(aEntries, e => e.nodeId == nodeId) {
    | Some(e) => e.timestamp
    | None => 0.0
    }
    let bTs = switch Belt.Array.getBy(bEntries, e => e.nodeId == nodeId) {
    | Some(e) => e.timestamp
    | None => 0.0
    }

    if aTs > bTs {
      bLessOrEqual := false
      equal := false
    }
    if bTs > aTs {
      aLessOrEqual := false
      equal := false
    }
  })

  if equal.contents {
    Equal
  } else if aLessOrEqual.contents {
    Before
  } else if bLessOrEqual.contents {
    After
  } else {
    Concurrent
  }
}

/** Get all entries from vector clock */
let getVectorClockEntries = (vc: vectorClock): array<vectorClockEntry> => {
  Belt.Array.map(vc.entries, entry => {nodeId: entry.nodeId, timestamp: entry.timestamp})
}

/** Get number of nodes tracked in vector clock */
let getVectorClockSize = (vc: vectorClock): int => {
  Belt.Array.length(vc.entries)
}

// ============================================================================
// Utility Functions
// ============================================================================

/** Convert clock order to string */
let clockOrderToString = (order: clockOrder): string => {
  switch order {
  | Before => "before"
  | After => "after"
  | Concurrent => "concurrent"
  | Equal => "equal"
  }
}

/** Generate a unique node ID based on timestamp and random value */
let generateNodeId = (): float => {
  let now = Js.Date.now()
  let random = Js.Math.random() *. 1000000.0
  now *. 1000000.0 +. random
}
