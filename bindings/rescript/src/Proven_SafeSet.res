// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeSet - Bounded set operations that cannot crash.
 *
 * Provides set data structures with bounded capacity to prevent
 * unbounded memory growth. All operations are safe and return
 * explicit errors rather than throwing exceptions.
 */

/** Error types for set operations */
type setError =
  | SetFull
  | ElementNotFound
  | InvalidCapacity
  | DuplicateElement

/** Result of an insert operation */
type insertResult =
  | Inserted
  | AlreadyPresent
  | SetFullResult

/** A bounded set with specified capacity */
type t<'a> = {
  mutable data: array<'a>,
  mutable length: int,
  capacity: int,
}

/** Create a new empty set with specified capacity */
let make = (capacity: int): t<'a> => {
  {
    data: [],
    length: 0,
    capacity: capacity,
  }
}

/** Get the number of elements in the set */
let count = (set: t<'a>): int => {
  set.length
}

/** Check if the set is empty */
let isEmpty = (set: t<'a>): bool => {
  set.length == 0
}

/** Check if the set is full */
let isFull = (set: t<'a>): bool => {
  set.length >= set.capacity
}

/** Get remaining capacity */
let remaining = (set: t<'a>): int => {
  set.capacity - set.length
}

/** Check if element exists in the set */
let contains = (set: t<'a>, value: 'a): bool => {
  Belt.Array.some(Belt.Array.slice(set.data, ~offset=0, ~len=set.length), elem => elem == value)
}

/** Insert an element (fails if full) */
let insert = (set: t<'a>, value: 'a): result<unit, setError> => {
  if contains(set, value) {
    Ok() // Already present, no-op
  } else if isFull(set) {
    Error(SetFull)
  } else {
    set.data = Belt.Array.concat(set.data, [value])
    set.length = set.length + 1
    Ok()
  }
}

/** Insert with detailed result */
let insertResult = (set: t<'a>, value: 'a): insertResult => {
  if contains(set, value) {
    AlreadyPresent
  } else if isFull(set) {
    SetFullResult
  } else {
    set.data = Belt.Array.concat(set.data, [value])
    set.length = set.length + 1
    Inserted
  }
}

/** Remove an element */
let remove = (set: t<'a>, value: 'a): result<unit, setError> => {
  let idx = ref(-1)
  for i in 0 to set.length - 1 {
    if idx.contents == -1 {
      switch Belt.Array.get(set.data, i) {
      | Some(elem) if elem == value => idx := i
      | _ => ()
      }
    }
  }
  if idx.contents == -1 {
    Error(ElementNotFound)
  } else {
    // Swap with last element and shrink
    let lastIdx = set.length - 1
    if idx.contents < lastIdx {
      switch Belt.Array.get(set.data, lastIdx) {
      | Some(lastElem) => Belt.Array.setUnsafe(set.data, idx.contents, lastElem)
      | None => ()
      }
    }
    set.length = set.length - 1
    Ok()
  }
}

/** Remove if present (no error if missing) */
let discard = (set: t<'a>, value: 'a): bool => {
  switch remove(set, value) {
  | Ok() => true
  | Error(_) => false
  }
}

/** Clear all elements */
let clear = (set: t<'a>): unit => {
  set.length = 0
  set.data = []
}

/** Get elements as an array */
let items = (set: t<'a>): array<'a> => {
  Belt.Array.slice(set.data, ~offset=0, ~len=set.length)
}

/** Union with another set (modifies self) */
let unionWith = (set: t<'a>, other: t<'a>): int => {
  let otherItems = items(other)
  let added = ref(0)
  Belt.Array.forEach(otherItems, elem => {
    if insertResult(set, elem) == Inserted {
      added := added.contents + 1
    }
  })
  added.contents
}

/** Intersection with another set (modifies self) */
let intersectWith = (set: t<'a>, other: t<'a>): int => {
  let removed = ref(0)
  let i = ref(0)
  while i.contents < set.length {
    switch Belt.Array.get(set.data, i.contents) {
    | Some(elem) =>
      if !contains(other, elem) {
        let _ = discard(set, elem)
        removed := removed.contents + 1
      } else {
        i := i.contents + 1
      }
    | None => i := i.contents + 1
    }
  }
  removed.contents
}

/** Difference with another set (modifies self, removes elements in other) */
let differenceWith = (set: t<'a>, other: t<'a>): int => {
  let removed = ref(0)
  let i = ref(0)
  while i.contents < set.length {
    switch Belt.Array.get(set.data, i.contents) {
    | Some(elem) =>
      if contains(other, elem) {
        let _ = discard(set, elem)
        removed := removed.contents + 1
      } else {
        i := i.contents + 1
      }
    | None => i := i.contents + 1
    }
  }
  removed.contents
}

/** Check if self is subset of other */
let isSubsetOf = (set: t<'a>, other: t<'a>): bool => {
  Belt.Array.every(items(set), elem => contains(other, elem))
}

/** Check if self is proper subset of other */
let isProperSubsetOf = (set: t<'a>, other: t<'a>): bool => {
  set.length < other.length && isSubsetOf(set, other)
}

/** Check if self is superset of other */
let isSupersetOf = (set: t<'a>, other: t<'a>): bool => {
  isSubsetOf(other, set)
}

/** Check if sets are disjoint (no common elements) */
let isDisjointWith = (set: t<'a>, other: t<'a>): bool => {
  !Belt.Array.some(items(set), elem => contains(other, elem))
}

/** Check equality with another set */
let equals = (set: t<'a>, other: t<'a>): bool => {
  set.length == other.length && isSubsetOf(set, other)
}

/** Pop and return an arbitrary element */
let pop = (set: t<'a>): option<'a> => {
  if isEmpty(set) {
    None
  } else {
    set.length = set.length - 1
    Belt.Array.get(set.data, set.length)
  }
}

/** Create a new set from union of two sets */
let union = (a: t<'a>, b: t<'a>): t<'a> => {
  let newSet = make(a.capacity + b.capacity)
  Belt.Array.forEach(items(a), elem => {
    let _ = insert(newSet, elem)
  })
  Belt.Array.forEach(items(b), elem => {
    let _ = insert(newSet, elem)
  })
  newSet
}

/** Create a new set from intersection of two sets */
let intersection = (a: t<'a>, b: t<'a>): t<'a> => {
  let newSet = make(Js.Math.min_int(a.capacity, b.capacity))
  Belt.Array.forEach(items(a), elem => {
    if contains(b, elem) {
      let _ = insert(newSet, elem)
    }
  })
  newSet
}

/** Create a new set from difference of two sets (a - b) */
let difference = (a: t<'a>, b: t<'a>): t<'a> => {
  let newSet = make(a.capacity)
  Belt.Array.forEach(items(a), elem => {
    if !contains(b, elem) {
      let _ = insert(newSet, elem)
    }
  })
  newSet
}

/** Create a new set from symmetric difference of two sets */
let symmetricDifference = (a: t<'a>, b: t<'a>): t<'a> => {
  let newSet = make(a.capacity + b.capacity)
  Belt.Array.forEach(items(a), elem => {
    if !contains(b, elem) {
      let _ = insert(newSet, elem)
    }
  })
  Belt.Array.forEach(items(b), elem => {
    if !contains(a, elem) {
      let _ = insert(newSet, elem)
    }
  })
  newSet
}

/** Map a function over all elements */
let map = (set: t<'a>, f: 'a => 'b): t<'b> => {
  let newSet = make(set.capacity)
  Belt.Array.forEach(items(set), elem => {
    let _ = insert(newSet, f(elem))
  })
  newSet
}

/** Filter elements matching a predicate */
let filter = (set: t<'a>, predicate: 'a => bool): t<'a> => {
  let newSet = make(set.capacity)
  Belt.Array.forEach(items(set), elem => {
    if predicate(elem) {
      let _ = insert(newSet, elem)
    }
  })
  newSet
}

/** Fold over all elements */
let fold = (set: t<'a>, init: 'b, f: ('b, 'a) => 'b): 'b => {
  Belt.Array.reduce(items(set), init, f)
}

/** Check if any element matches a predicate */
let exists = (set: t<'a>, predicate: 'a => bool): bool => {
  Belt.Array.some(items(set), predicate)
}

/** Check if all elements match a predicate */
let forAll = (set: t<'a>, predicate: 'a => bool): bool => {
  Belt.Array.every(items(set), predicate)
}

/** Find an element matching a predicate */
let find = (set: t<'a>, predicate: 'a => bool): option<'a> => {
  Belt.Array.getBy(items(set), predicate)
}

/** Ordered set module that maintains insertion order */
module Ordered = {
  type orderedSet<'a> = {
    mutable data: array<'a>,
    mutable length: int,
    capacity: int,
  }

  let make = (capacity: int): orderedSet<'a> => {
    {
      data: [],
      length: 0,
      capacity: capacity,
    }
  }

  let count = (set: orderedSet<'a>): int => set.length

  let isEmpty = (set: orderedSet<'a>): bool => set.length == 0

  let isFull = (set: orderedSet<'a>): bool => set.length >= set.capacity

  let contains = (set: orderedSet<'a>, value: 'a): bool => {
    Belt.Array.some(Belt.Array.slice(set.data, ~offset=0, ~len=set.length), elem => elem == value)
  }

  let get = (set: orderedSet<'a>, index: int): option<'a> => {
    if index < 0 || index >= set.length {
      None
    } else {
      Belt.Array.get(set.data, index)
    }
  }

  let indexOf = (set: orderedSet<'a>, value: 'a): option<int> => {
    let idx = ref(None)
    for i in 0 to set.length - 1 {
      if idx.contents == None {
        switch Belt.Array.get(set.data, i) {
        | Some(elem) if elem == value => idx := Some(i)
        | _ => ()
        }
      }
    }
    idx.contents
  }

  let insert = (set: orderedSet<'a>, value: 'a): result<unit, setError> => {
    if contains(set, value) {
      Ok()
    } else if isFull(set) {
      Error(SetFull)
    } else {
      set.data = Belt.Array.concat(set.data, [value])
      set.length = set.length + 1
      Ok()
    }
  }

  let remove = (set: orderedSet<'a>, value: 'a): result<unit, setError> => {
    switch indexOf(set, value) {
    | None => Error(ElementNotFound)
    | Some(idx) =>
      // Shift elements left to preserve order
      let newData = Belt.Array.keepWithIndex(set.data, (_, i) => i != idx)
      set.data = newData
      set.length = set.length - 1
      Ok()
    }
  }

  let first = (set: orderedSet<'a>): option<'a> => get(set, 0)

  let last = (set: orderedSet<'a>): option<'a> => {
    if set.length == 0 {
      None
    } else {
      get(set, set.length - 1)
    }
  }

  let clear = (set: orderedSet<'a>): unit => {
    set.length = 0
    set.data = []
  }

  let items = (set: orderedSet<'a>): array<'a> => {
    Belt.Array.slice(set.data, ~offset=0, ~len=set.length)
  }
}

/** Sorted set module that maintains sorted order */
module Sorted = {
  type sortedSet<'a> = {
    mutable data: array<'a>,
    mutable length: int,
    capacity: int,
  }

  let make = (capacity: int): sortedSet<'a> => {
    {
      data: [],
      length: 0,
      capacity: capacity,
    }
  }

  let count = (set: sortedSet<'a>): int => set.length

  let isEmpty = (set: sortedSet<'a>): bool => set.length == 0

  let isFull = (set: sortedSet<'a>): bool => set.length >= set.capacity

  /** Binary search for element position */
  let findPosition = (set: sortedSet<'a>, value: 'a): (bool, int) => {
    if set.length == 0 {
      (false, 0)
    } else {
      let left = ref(0)
      let right = ref(set.length)
      let found = ref(false)
      let result = ref(0)
      while left.contents < right.contents && !found.contents {
        let mid = left.contents + (right.contents - left.contents) / 2
        switch Belt.Array.get(set.data, mid) {
        | Some(elem) =>
          if elem == value {
            found := true
            result := mid
          } else if elem < value {
            left := mid + 1
          } else {
            right := mid
          }
        | None => right := mid
        }
      }
      if found.contents {
        (true, result.contents)
      } else {
        (false, left.contents)
      }
    }
  }

  let contains = (set: sortedSet<'a>, value: 'a): bool => {
    let (found, _) = findPosition(set, value)
    found
  }

  let insert = (set: sortedSet<'a>, value: 'a): result<unit, setError> => {
    let (found, pos) = findPosition(set, value)
    if found {
      Ok() // Already present
    } else if isFull(set) {
      Error(SetFull)
    } else {
      // Insert at position, shifting elements right
      let before = Belt.Array.slice(set.data, ~offset=0, ~len=pos)
      let after = Belt.Array.slice(set.data, ~offset=pos, ~len=set.length - pos)
      set.data = Belt.Array.concatMany([before, [value], after])
      set.length = set.length + 1
      Ok()
    }
  }

  let remove = (set: sortedSet<'a>, value: 'a): result<unit, setError> => {
    let (found, pos) = findPosition(set, value)
    if !found {
      Error(ElementNotFound)
    } else {
      let before = Belt.Array.slice(set.data, ~offset=0, ~len=pos)
      let after = Belt.Array.slice(set.data, ~offset=pos + 1, ~len=set.length - pos - 1)
      set.data = Belt.Array.concat(before, after)
      set.length = set.length - 1
      Ok()
    }
  }

  let get = (set: sortedSet<'a>, index: int): option<'a> => {
    if index < 0 || index >= set.length {
      None
    } else {
      Belt.Array.get(set.data, index)
    }
  }

  let min = (set: sortedSet<'a>): option<'a> => get(set, 0)

  let max = (set: sortedSet<'a>): option<'a> => {
    if set.length == 0 {
      None
    } else {
      get(set, set.length - 1)
    }
  }

  /** Get all items from the set */
  let items = (set: sortedSet<'a>): array<'a> => {
    Belt.Array.slice(set.data, ~offset=0, ~len=set.length)
  }

  /** Get elements in range [low, high] */
  let range = (set: sortedSet<'a>, low: 'a, high: 'a): array<'a> => {
    Belt.Array.keep(items(set), elem => elem >= low && elem <= high)
  }

  let clear = (set: sortedSet<'a>): unit => {
    set.length = 0
    set.data = []
  }
}

/** String set module with efficient string comparison */
module StringSet = {
  type stringSet = {
    mutable data: array<string>,
    mutable length: int,
    capacity: int,
  }

  let make = (capacity: int): stringSet => {
    {
      data: [],
      length: 0,
      capacity: capacity,
    }
  }

  let count = (set: stringSet): int => set.length

  let isEmpty = (set: stringSet): bool => set.length == 0

  let isFull = (set: stringSet): bool => set.length >= set.capacity

  let contains = (set: stringSet, value: string): bool => {
    Belt.Array.some(Belt.Array.slice(set.data, ~offset=0, ~len=set.length), elem => elem == value)
  }

  let insert = (set: stringSet, value: string): result<unit, setError> => {
    if contains(set, value) {
      Ok()
    } else if isFull(set) {
      Error(SetFull)
    } else {
      set.data = Belt.Array.concat(set.data, [value])
      set.length = set.length + 1
      Ok()
    }
  }

  let remove = (set: stringSet, value: string): result<unit, setError> => {
    let idx = ref(-1)
    for i in 0 to set.length - 1 {
      if idx.contents == -1 {
        switch Belt.Array.get(set.data, i) {
        | Some(elem) if elem == value => idx := i
        | _ => ()
        }
      }
    }
    if idx.contents == -1 {
      Error(ElementNotFound)
    } else {
      let lastIdx = set.length - 1
      if idx.contents < lastIdx {
        switch Belt.Array.get(set.data, lastIdx) {
        | Some(lastElem) => Belt.Array.setUnsafe(set.data, idx.contents, lastElem)
        | None => ()
        }
      }
      set.length = set.length - 1
      Ok()
    }
  }

  let clear = (set: stringSet): unit => {
    set.length = 0
    set.data = []
  }

  let items = (set: stringSet): array<string> => {
    Belt.Array.slice(set.data, ~offset=0, ~len=set.length)
  }
}
