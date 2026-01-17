// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeOrdering - Total ordering and comparison operations that cannot crash.
 *
 * Provides type-safe comparison utilities with well-defined behavior for
 * edge cases including NaN handling, null comparisons, and custom ordering.
 */

/** Result of a three-way comparison */
type ordering =
  | Less
  | Equal
  | Greater

/** Reverse the ordering */
let reverse = (ord: ordering): ordering => {
  switch ord {
  | Less => Greater
  | Equal => Equal
  | Greater => Less
  }
}

/** Chain two orderings (use second if first is equal) */
let then_ = (first: ordering, second: ordering): ordering => {
  if first == Equal {
    second
  } else {
    first
  }
}

/** Convert ordering to integer (-1, 0, 1) */
let toInt = (ord: ordering): int => {
  switch ord {
  | Less => -1
  | Equal => 0
  | Greater => 1
  }
}

/** Create ordering from integer comparison result */
let fromInt = (value: int): ordering => {
  if value < 0 {
    Less
  } else if value > 0 {
    Greater
  } else {
    Equal
  }
}

/** Compare two integers */
let compareInt = (a: int, b: int): ordering => {
  if a < b {
    Less
  } else if a > b {
    Greater
  } else {
    Equal
  }
}

/** Compare two floats with NaN handling (NaN is greater than all values) */
let compareFloat = (a: float, b: float): ordering => {
  let aIsNan = Js.Float.isNaN(a)
  let bIsNan = Js.Float.isNaN(b)

  if aIsNan && bIsNan {
    Equal
  } else if aIsNan {
    Greater
  } else if bIsNan {
    Less
  } else if a < b {
    Less
  } else if a > b {
    Greater
  } else {
    Equal
  }
}

/** Compare two strings lexicographically */
let compareString = (a: string, b: string): ordering => {
  let result = Js.String2.localeCompare(a, b)
  if result < 0.0 {
    Less
  } else if result > 0.0 {
    Greater
  } else {
    Equal
  }
}

/** Compare two strings case-insensitively */
let compareStringIgnoreCase = (a: string, b: string): ordering => {
  let aLower = Js.String2.toLowerCase(a)
  let bLower = Js.String2.toLowerCase(b)
  compareString(aLower, bLower)
}

/** Compare two optional values (None is considered less than any Some value) */
let compareOption = (a: option<'a>, b: option<'a>, cmp: ('a, 'a) => ordering): ordering => {
  switch (a, b) {
  | (None, None) => Equal
  | (None, Some(_)) => Less
  | (Some(_), None) => Greater
  | (Some(av), Some(bv)) => cmp(av, bv)
  }
}

/** Compare two optional integers */
let compareOptionInt = (a: option<int>, b: option<int>): ordering => {
  compareOption(a, b, compareInt)
}

/** Compare two optional floats */
let compareOptionFloat = (a: option<float>, b: option<float>): ordering => {
  compareOption(a, b, compareFloat)
}

/** Compare two optional strings */
let compareOptionString = (a: option<string>, b: option<string>): ordering => {
  compareOption(a, b, compareString)
}

/** Compare two arrays lexicographically */
let compareArray = (a: array<'a>, b: array<'a>, cmp: ('a, 'a) => ordering): ordering => {
  let aLen = Belt.Array.length(a)
  let bLen = Belt.Array.length(b)
  let minLen = min(aLen, bLen)

  let rec loop = (i: int): ordering => {
    if i >= minLen {
      compareInt(aLen, bLen)
    } else {
      let av = Belt.Array.getUnsafe(a, i)
      let bv = Belt.Array.getUnsafe(b, i)
      let ord = cmp(av, bv)
      if ord != Equal {
        ord
      } else {
        loop(i + 1)
      }
    }
  }
  loop(0)
}

/** Compare two integer arrays */
let compareIntArray = (a: array<int>, b: array<int>): ordering => {
  compareArray(a, b, compareInt)
}

/** Compare two float arrays */
let compareFloatArray = (a: array<float>, b: array<float>): ordering => {
  compareArray(a, b, compareFloat)
}

/** Compare two string arrays */
let compareStringArray = (a: array<string>, b: array<string>): ordering => {
  compareArray(a, b, compareString)
}

/** Minimum of two values */
let minBy = (a: 'a, b: 'a, cmp: ('a, 'a) => ordering): 'a => {
  if cmp(a, b) == Less {
    a
  } else {
    b
  }
}

/** Maximum of two values */
let maxBy = (a: 'a, b: 'a, cmp: ('a, 'a) => ordering): 'a => {
  if cmp(a, b) == Greater {
    a
  } else {
    b
  }
}

/** Minimum of two integers */
let minInt = (a: int, b: int): int => {
  if a < b {
    a
  } else {
    b
  }
}

/** Maximum of two integers */
let maxInt = (a: int, b: int): int => {
  if a > b {
    a
  } else {
    b
  }
}

/** Minimum of two floats (NaN-safe) */
let minFloat = (a: float, b: float): float => {
  if Js.Float.isNaN(a) {
    b
  } else if Js.Float.isNaN(b) {
    a
  } else if a < b {
    a
  } else {
    b
  }
}

/** Maximum of two floats (NaN-safe) */
let maxFloat = (a: float, b: float): float => {
  if Js.Float.isNaN(a) {
    b
  } else if Js.Float.isNaN(b) {
    a
  } else if a > b {
    a
  } else {
    b
  }
}

/** Clamp an integer to a range [lower, upper] */
let clampInt = (value: int, lower: int, upper: int): int => {
  maxInt(lower, minInt(upper, value))
}

/** Clamp a float to a range [lower, upper] */
let clampFloat = (value: float, lower: float, upper: float): float => {
  maxFloat(lower, minFloat(upper, value))
}

/** Check if an integer is within a range [lower, upper] */
let inRangeInt = (value: int, lower: int, upper: int): bool => {
  value >= lower && value <= upper
}

/** Check if a float is within a range [lower, upper] */
let inRangeFloat = (value: float, lower: float, upper: float): bool => {
  value >= lower && value <= upper
}

/** Find minimum value in an array */
let minArray = (arr: array<'a>, cmp: ('a, 'a) => ordering): option<'a> => {
  if Belt.Array.length(arr) == 0 {
    None
  } else {
    let result = ref(Belt.Array.getUnsafe(arr, 0))
    arr->Belt.Array.forEach(item => {
      if cmp(item, result.contents) == Less {
        result := item
      }
    })
    Some(result.contents)
  }
}

/** Find maximum value in an array */
let maxArray = (arr: array<'a>, cmp: ('a, 'a) => ordering): option<'a> => {
  if Belt.Array.length(arr) == 0 {
    None
  } else {
    let result = ref(Belt.Array.getUnsafe(arr, 0))
    arr->Belt.Array.forEach(item => {
      if cmp(item, result.contents) == Greater {
        result := item
      }
    })
    Some(result.contents)
  }
}

/** Find minimum value in an integer array */
let minIntArray = (arr: array<int>): option<int> => {
  minArray(arr, compareInt)
}

/** Find maximum value in an integer array */
let maxIntArray = (arr: array<int>): option<int> => {
  maxArray(arr, compareInt)
}

/** Find minimum value in a float array */
let minFloatArray = (arr: array<float>): option<float> => {
  minArray(arr, compareFloat)
}

/** Find maximum value in a float array */
let maxFloatArray = (arr: array<float>): option<float> => {
  maxArray(arr, compareFloat)
}

/** Check if an array is sorted in ascending order */
let isSorted = (arr: array<'a>, cmp: ('a, 'a) => ordering): bool => {
  let len = Belt.Array.length(arr)
  if len <= 1 {
    true
  } else {
    let rec loop = (i: int): bool => {
      if i >= len - 1 {
        true
      } else {
        let a = Belt.Array.getUnsafe(arr, i)
        let b = Belt.Array.getUnsafe(arr, i + 1)
        if cmp(a, b) == Greater {
          false
        } else {
          loop(i + 1)
        }
      }
    }
    loop(0)
  }
}

/** Check if an integer array is sorted in ascending order */
let isSortedInt = (arr: array<int>): bool => {
  isSorted(arr, compareInt)
}

/** Check if a float array is sorted in ascending order */
let isSortedFloat = (arr: array<float>): bool => {
  isSorted(arr, compareFloat)
}

/** Check if a string array is sorted in ascending order */
let isSortedString = (arr: array<string>): bool => {
  isSorted(arr, compareString)
}

/** Check if an array is sorted in descending order */
let isSortedDescending = (arr: array<'a>, cmp: ('a, 'a) => ordering): bool => {
  let len = Belt.Array.length(arr)
  if len <= 1 {
    true
  } else {
    let rec loop = (i: int): bool => {
      if i >= len - 1 {
        true
      } else {
        let a = Belt.Array.getUnsafe(arr, i)
        let b = Belt.Array.getUnsafe(arr, i + 1)
        if cmp(a, b) == Less {
          false
        } else {
          loop(i + 1)
        }
      }
    }
    loop(0)
  }
}

/** Create a reverse comparator from an existing comparator */
let reverseComparator = (cmp: ('a, 'a) => ordering): (('a, 'a) => ordering) => {
  (a, b) => reverse(cmp(a, b))
}

/** Combine multiple comparators (multi-key sort) */
let combineComparators = (comparators: array<('a, 'a) => ordering>): (('a, 'a) => ordering) => {
  (a, b) => {
    let len = Belt.Array.length(comparators)
    let rec loop = (i: int): ordering => {
      if i >= len {
        Equal
      } else {
        let cmp = Belt.Array.getUnsafe(comparators, i)
        let result = cmp(a, b)
        if result != Equal {
          result
        } else {
          loop(i + 1)
        }
      }
    }
    loop(0)
  }
}
