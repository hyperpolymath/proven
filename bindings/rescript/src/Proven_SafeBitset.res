// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeBitset - Fixed-size bitset operations with bounds checking that cannot crash.
 *
 * This module provides a safe wrapper around bitset operations with explicit
 * bounds checking. All operations return Result types on invalid indices rather
 * than causing undefined behavior.
 */

/** Error types for bitset operations */
type bitsetError =
  | IndexOutOfBounds
  | SizeMismatch
  | InvalidRange
  | Overflow

/** A fixed-size bitset */
type t = {
  mutable words: array<int>,
  size: int,
  wordCount: int,
}

/** Bits per word (JavaScript safe integer operations use 32 bits) */
let bitsPerWord = 32

/** Calculate word count for a given size */
let wordCountForSize = (size: int): int => {
  (size + bitsPerWord - 1) / bitsPerWord
}

/** Initialize with all bits set to zero */
let initEmpty = (size: int): t => {
  let wordCount = wordCountForSize(size)
  {
    words: Belt.Array.make(wordCount, 0),
    size: size,
    wordCount: wordCount,
  }
}

/** Initialize with all bits set to one */
let initFull = (size: int): t => {
  let wordCount = wordCountForSize(size)
  let words = Belt.Array.make(wordCount, -1) // All bits set (as signed int)
  // Clear unused bits in the last word
  let extraBits = mod(size, bitsPerWord)
  if extraBits != 0 && wordCount > 0 {
    let mask = lsl(1, extraBits) - 1
    Belt.Array.setUnsafe(words, wordCount - 1, mask)
  }
  {
    words: words,
    size: size,
    wordCount: wordCount,
  }
}

/** Get the capacity (number of bits) */
let capacity = (bitset: t): int => {
  bitset.size
}

/** Get word index and bit mask for a given bit index */
let wordAndBit = (index: int): (int, int) => {
  let wordIndex = index / bitsPerWord
  let bitIndex = mod(index, bitsPerWord)
  let mask = lsl(1, bitIndex)
  (wordIndex, mask)
}

/** Safely set a bit at the given index */
let set = (bitset: t, index: int): result<unit, bitsetError> => {
  if index < 0 || index >= bitset.size {
    Error(IndexOutOfBounds)
  } else {
    let (wordIndex, mask) = wordAndBit(index)
    switch Belt.Array.get(bitset.words, wordIndex) {
    | Some(word) =>
      Belt.Array.setUnsafe(bitset.words, wordIndex, lor(word, mask))
      Ok()
    | None => Error(IndexOutOfBounds)
    }
  }
}

/** Safely unset (clear) a bit at the given index */
let unset = (bitset: t, index: int): result<unit, bitsetError> => {
  if index < 0 || index >= bitset.size {
    Error(IndexOutOfBounds)
  } else {
    let (wordIndex, mask) = wordAndBit(index)
    switch Belt.Array.get(bitset.words, wordIndex) {
    | Some(word) =>
      Belt.Array.setUnsafe(bitset.words, wordIndex, land(word, lnot(mask)))
      Ok()
    | None => Error(IndexOutOfBounds)
    }
  }
}

/** Safely toggle a bit at the given index */
let toggle = (bitset: t, index: int): result<unit, bitsetError> => {
  if index < 0 || index >= bitset.size {
    Error(IndexOutOfBounds)
  } else {
    let (wordIndex, mask) = wordAndBit(index)
    switch Belt.Array.get(bitset.words, wordIndex) {
    | Some(word) =>
      Belt.Array.setUnsafe(bitset.words, wordIndex, lxor(word, mask))
      Ok()
    | None => Error(IndexOutOfBounds)
    }
  }
}

/** Safely check if a bit is set at the given index */
let isSet = (bitset: t, index: int): result<bool, bitsetError> => {
  if index < 0 || index >= bitset.size {
    Error(IndexOutOfBounds)
  } else {
    let (wordIndex, mask) = wordAndBit(index)
    switch Belt.Array.get(bitset.words, wordIndex) {
    | Some(word) => Ok(land(word, mask) != 0)
    | None => Error(IndexOutOfBounds)
    }
  }
}

/** Get bit value (returns None if out of bounds) */
let get = (bitset: t, index: int): option<bool> => {
  switch isSet(bitset, index) {
  | Ok(v) => Some(v)
  | Error(_) => None
  }
}

/** Set bit value (returns false if out of bounds) */
let put = (bitset: t, index: int, value: bool): bool => {
  let result = if value {
    set(bitset, index)
  } else {
    unset(bitset, index)
  }
  switch result {
  | Ok() => true
  | Error(_) => false
  }
}

/** Set a range of bits [start, end) */
let setRange = (bitset: t, startIdx: int, endIdx: int): result<unit, bitsetError> => {
  if startIdx > endIdx {
    Error(InvalidRange)
  } else if endIdx > bitset.size {
    Error(IndexOutOfBounds)
  } else {
    for i in startIdx to endIdx - 1 {
      let _ = set(bitset, i)
    }
    Ok()
  }
}

/** Unset a range of bits [start, end) */
let unsetRange = (bitset: t, startIdx: int, endIdx: int): result<unit, bitsetError> => {
  if startIdx > endIdx {
    Error(InvalidRange)
  } else if endIdx > bitset.size {
    Error(IndexOutOfBounds)
  } else {
    for i in startIdx to endIdx - 1 {
      let _ = unset(bitset, i)
    }
    Ok()
  }
}

/** Count the number of set bits (popcount) */
let count = (bitset: t): int => {
  let total = ref(0)
  Belt.Array.forEach(bitset.words, word => {
    // Count bits using Brian Kernighan's algorithm
    let w = ref(word)
    while w.contents != 0 {
      w := land(w.contents, w.contents - 1)
      total := total.contents + 1
    }
  })
  total.contents
}

/** Count the number of unset bits */
let countZeros = (bitset: t): int => {
  bitset.size - count(bitset)
}

/** Check if all bits are zero */
let isEmpty = (bitset: t): bool => {
  Belt.Array.every(bitset.words, word => word == 0)
}

/** Check if all bits are one */
let isFull = (bitset: t): bool => {
  count(bitset) == bitset.size
}

/** Check if any bit is set */
let any = (bitset: t): bool => {
  !isEmpty(bitset)
}

/** Check if no bits are set */
let none = (bitset: t): bool => {
  isEmpty(bitset)
}

/** Find the first set bit, return None if none */
let findFirstSet = (bitset: t): option<int> => {
  let result = ref(None)
  let i = ref(0)
  while result.contents == None && i.contents < bitset.wordCount {
    switch Belt.Array.get(bitset.words, i.contents) {
    | Some(word) if word != 0 =>
      // Find lowest set bit in this word
      for bit in 0 to bitsPerWord - 1 {
        if result.contents == None {
          let bitIdx = i.contents * bitsPerWord + bit
          if bitIdx < bitset.size && land(word, lsl(1, bit)) != 0 {
            result := Some(bitIdx)
          }
        }
      }
    | _ => ()
    }
    i := i.contents + 1
  }
  result.contents
}

/** Find the last set bit, return None if none */
let findLastSet = (bitset: t): option<int> => {
  let result = ref(None)
  let i = ref(bitset.wordCount - 1)
  while result.contents == None && i.contents >= 0 {
    switch Belt.Array.get(bitset.words, i.contents) {
    | Some(word) if word != 0 =>
      // Find highest set bit in this word
      for bit in bitsPerWord - 1 downto 0 {
        if result.contents == None {
          let bitIdx = i.contents * bitsPerWord + bit
          if bitIdx < bitset.size && land(word, lsl(1, bit)) != 0 {
            result := Some(bitIdx)
          }
        }
      }
    | _ => ()
    }
    i := i.contents - 1
  }
  result.contents
}

/** Find the first unset bit, return None if all are set */
let findFirstUnset = (bitset: t): option<int> => {
  let result = ref(None)
  for i in 0 to bitset.size - 1 {
    if result.contents == None {
      switch get(bitset, i) {
      | Some(false) => result := Some(i)
      | _ => ()
      }
    }
  }
  result.contents
}

/** Clear all bits */
let clear = (bitset: t): unit => {
  for i in 0 to bitset.wordCount - 1 {
    Belt.Array.setUnsafe(bitset.words, i, 0)
  }
}

/** Set all bits */
let fill = (bitset: t): unit => {
  for i in 0 to bitset.wordCount - 1 {
    Belt.Array.setUnsafe(bitset.words, i, -1)
  }
  // Clear unused bits in the last word
  let extraBits = mod(bitset.size, bitsPerWord)
  if extraBits != 0 && bitset.wordCount > 0 {
    let mask = lsl(1, extraBits) - 1
    Belt.Array.setUnsafe(bitset.words, bitset.wordCount - 1, mask)
  }
}

/** Complement (invert) all bits */
let complement = (bitset: t): unit => {
  for i in 0 to bitset.wordCount - 1 {
    switch Belt.Array.get(bitset.words, i) {
    | Some(word) => Belt.Array.setUnsafe(bitset.words, i, lnot(word))
    | None => ()
    }
  }
  // Clear unused bits in the last word
  let extraBits = mod(bitset.size, bitsPerWord)
  if extraBits != 0 && bitset.wordCount > 0 {
    let mask = lsl(1, extraBits) - 1
    switch Belt.Array.get(bitset.words, bitset.wordCount - 1) {
    | Some(word) => Belt.Array.setUnsafe(bitset.words, bitset.wordCount - 1, land(word, mask))
    | None => ()
    }
  }
}

/** Bitwise AND with another bitset (modifies self) */
let setIntersection = (bitset: t, other: t): result<unit, bitsetError> => {
  if bitset.size != other.size {
    Error(SizeMismatch)
  } else {
    for i in 0 to bitset.wordCount - 1 {
      switch (Belt.Array.get(bitset.words, i), Belt.Array.get(other.words, i)) {
      | (Some(a), Some(b)) => Belt.Array.setUnsafe(bitset.words, i, land(a, b))
      | _ => ()
      }
    }
    Ok()
  }
}

/** Bitwise OR with another bitset (modifies self) */
let setUnion = (bitset: t, other: t): result<unit, bitsetError> => {
  if bitset.size != other.size {
    Error(SizeMismatch)
  } else {
    for i in 0 to bitset.wordCount - 1 {
      switch (Belt.Array.get(bitset.words, i), Belt.Array.get(other.words, i)) {
      | (Some(a), Some(b)) => Belt.Array.setUnsafe(bitset.words, i, lor(a, b))
      | _ => ()
      }
    }
    Ok()
  }
}

/** Bitwise XOR with another bitset (modifies self) */
let setXor = (bitset: t, other: t): result<unit, bitsetError> => {
  if bitset.size != other.size {
    Error(SizeMismatch)
  } else {
    for i in 0 to bitset.wordCount - 1 {
      switch (Belt.Array.get(bitset.words, i), Belt.Array.get(other.words, i)) {
      | (Some(a), Some(b)) => Belt.Array.setUnsafe(bitset.words, i, lxor(a, b))
      | _ => ()
      }
    }
    Ok()
  }
}

/** Bitwise AND NOT (set difference): self & ~other (modifies self) */
let setDifference = (bitset: t, other: t): result<unit, bitsetError> => {
  if bitset.size != other.size {
    Error(SizeMismatch)
  } else {
    for i in 0 to bitset.wordCount - 1 {
      switch (Belt.Array.get(bitset.words, i), Belt.Array.get(other.words, i)) {
      | (Some(a), Some(b)) => Belt.Array.setUnsafe(bitset.words, i, land(a, lnot(b)))
      | _ => ()
      }
    }
    Ok()
  }
}

/** Check if two bitsets are equal */
let equals = (bitset: t, other: t): bool => {
  if bitset.size != other.size {
    false
  } else {
    Belt.Array.every(
      Belt.Array.zip(bitset.words, other.words),
      ((a, b)) => a == b,
    )
  }
}

/** Check if this bitset is a subset of another */
let isSubsetOf = (bitset: t, other: t): bool => {
  if bitset.size != other.size {
    false
  } else {
    let result = ref(true)
    for i in 0 to bitset.wordCount - 1 {
      if result.contents {
        switch (Belt.Array.get(bitset.words, i), Belt.Array.get(other.words, i)) {
        | (Some(a), Some(b)) =>
          if land(a, lnot(b)) != 0 {
            result := false
          }
        | _ => result := false
        }
      }
    }
    result.contents
  }
}

/** Check if this bitset is a superset of another */
let isSupersetOf = (bitset: t, other: t): bool => {
  isSubsetOf(other, bitset)
}

/** Check if two bitsets are disjoint (no common set bits) */
let isDisjoint = (bitset: t, other: t): bool => {
  if bitset.size != other.size {
    true
  } else {
    let result = ref(true)
    for i in 0 to bitset.wordCount - 1 {
      if result.contents {
        switch (Belt.Array.get(bitset.words, i), Belt.Array.get(other.words, i)) {
        | (Some(a), Some(b)) =>
          if land(a, b) != 0 {
            result := false
          }
        | _ => ()
        }
      }
    }
    result.contents
  }
}

/** Clone a bitset */
let clone = (bitset: t): t => {
  {
    words: Belt.Array.copy(bitset.words),
    size: bitset.size,
    wordCount: bitset.wordCount,
  }
}

/** Return a new bitset that is the intersection */
let intersection = (a: t, b: t): option<t> => {
  if a.size != b.size {
    None
  } else {
    let result = clone(a)
    switch setIntersection(result, b) {
    | Ok() => Some(result)
    | Error(_) => None
    }
  }
}

/** Return a new bitset that is the union */
let union = (a: t, b: t): option<t> => {
  if a.size != b.size {
    None
  } else {
    let result = clone(a)
    switch setUnion(result, b) {
    | Ok() => Some(result)
    | Error(_) => None
    }
  }
}

/** Return a new bitset that is the XOR */
let xorWith = (a: t, b: t): option<t> => {
  if a.size != b.size {
    None
  } else {
    let result = clone(a)
    switch setXor(result, b) {
    | Ok() => Some(result)
    | Error(_) => None
    }
  }
}

/** Return a new bitset that is the difference (a - b) */
let difference = (a: t, b: t): option<t> => {
  if a.size != b.size {
    None
  } else {
    let result = clone(a)
    switch setDifference(result, b) {
    | Ok() => Some(result)
    | Error(_) => None
    }
  }
}

/** Convert to array of set indices */
let toIndexArray = (bitset: t): array<int> => {
  let result = ref([])
  for i in 0 to bitset.size - 1 {
    switch get(bitset, i) {
    | Some(true) => result := Belt.Array.concat(result.contents, [i])
    | _ => ()
    }
  }
  result.contents
}

/** Create a bitset from an array of indices */
let fromIndices = (size: int, indices: array<int>): result<t, bitsetError> => {
  let bitset = initEmpty(size)
  let error = ref(None)
  Belt.Array.forEach(indices, index => {
    if error.contents == None {
      switch set(bitset, index) {
      | Ok() => ()
      | Error(e) => error := Some(e)
      }
    }
  })
  switch error.contents {
  | Some(e) => Error(e)
  | None => Ok(bitset)
  }
}

/** Hamming distance between two bitsets (count of differing bits) */
let hammingDistance = (a: t, b: t): option<int> => {
  switch xorWith(a, b) {
  | Some(xored) => Some(count(xored))
  | None => None
  }
}

/** Jaccard similarity coefficient between two bitsets */
let jaccardSimilarity = (a: t, b: t): option<float> => {
  switch (intersection(a, b), union(a, b)) {
  | (Some(intersectSet), Some(unionSet)) =>
    let intersectCount = count(intersectSet)
    let unionCount = count(unionSet)
    if unionCount == 0 {
      Some(1.0) // Both empty sets are considered identical
    } else {
      Some(Belt.Int.toFloat(intersectCount) /. Belt.Int.toFloat(unionCount))
    }
  | _ => None
  }
}

/** Iterator over set bits */
let forEach = (bitset: t, f: int => unit): unit => {
  for i in 0 to bitset.size - 1 {
    switch get(bitset, i) {
    | Some(true) => f(i)
    | _ => ()
    }
  }
}

/** Map over set bit indices */
let mapIndices = (bitset: t, f: int => 'a): array<'a> => {
  let result = ref([])
  forEach(bitset, i => {
    result := Belt.Array.concat(result.contents, [f(i)])
  })
  result.contents
}

/** Filter set bit indices */
let filterIndices = (bitset: t, predicate: int => bool): array<int> => {
  let result = ref([])
  forEach(bitset, i => {
    if predicate(i) {
      result := Belt.Array.concat(result.contents, [i])
    }
  })
  result.contents
}

/** Fold over set bit indices */
let foldIndices = (bitset: t, init: 'a, f: ('a, int) => 'a): 'a => {
  let acc = ref(init)
  forEach(bitset, i => {
    acc := f(acc.contents, i)
  })
  acc.contents
}
