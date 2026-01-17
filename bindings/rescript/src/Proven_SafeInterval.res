// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeInterval - Numeric interval operations with bounds checking that cannot crash.
 *
 * Provides closed, open, and half-open interval types with set operations.
 * All operations validate bounds and return errors instead of crashing.
 */

/** Error types for interval operations */
type intervalError =
  | InvalidBounds
  | EmptyInterval
  | DisjointIntervals
  | Overflow

/** Bound type for interval endpoints */
type boundType =
  | Closed     // Includes the endpoint [a, b]
  | Open       // Excludes the endpoint (a, b)
  | Unbounded  // No bound (extends to infinity)

/** Generic interval type for numeric values */
type interval = {
  lower: option<float>,
  upper: option<float>,
  lowerBound: boundType,
  upperBound: boundType,
}

/** Create a closed interval [lower, upper] */
let closed = (lower: float, upper: float): result<interval, intervalError> => {
  if lower > upper {
    Error(InvalidBounds)
  } else {
    Ok({
      lower: Some(lower),
      upper: Some(upper),
      lowerBound: Closed,
      upperBound: Closed,
    })
  }
}

/** Create an open interval (lower, upper) */
let open_ = (lower: float, upper: float): result<interval, intervalError> => {
  if lower >= upper {
    Error(InvalidBounds)
  } else {
    Ok({
      lower: Some(lower),
      upper: Some(upper),
      lowerBound: Open,
      upperBound: Open,
    })
  }
}

/** Create a half-open interval [lower, upper) */
let closedOpen = (lower: float, upper: float): result<interval, intervalError> => {
  if lower >= upper {
    Error(InvalidBounds)
  } else {
    Ok({
      lower: Some(lower),
      upper: Some(upper),
      lowerBound: Closed,
      upperBound: Open,
    })
  }
}

/** Create a half-open interval (lower, upper] */
let openClosed = (lower: float, upper: float): result<interval, intervalError> => {
  if lower >= upper {
    Error(InvalidBounds)
  } else {
    Ok({
      lower: Some(lower),
      upper: Some(upper),
      lowerBound: Open,
      upperBound: Closed,
    })
  }
}

/** Create an interval from lower bound to positive infinity [lower, +inf) */
let atLeast = (lower: float): interval => {
  {
    lower: Some(lower),
    upper: None,
    lowerBound: Closed,
    upperBound: Unbounded,
  }
}

/** Create an interval from lower bound to positive infinity (lower, +inf) */
let greaterThan = (lower: float): interval => {
  {
    lower: Some(lower),
    upper: None,
    lowerBound: Open,
    upperBound: Unbounded,
  }
}

/** Create an interval from negative infinity to upper bound (-inf, upper] */
let atMost = (upper: float): interval => {
  {
    lower: None,
    upper: Some(upper),
    lowerBound: Unbounded,
    upperBound: Closed,
  }
}

/** Create an interval from negative infinity to upper bound (-inf, upper) */
let lessThan = (upper: float): interval => {
  {
    lower: None,
    upper: Some(upper),
    lowerBound: Unbounded,
    upperBound: Open,
  }
}

/** Create an interval covering all values (-inf, +inf) */
let all: interval = {
  lower: None,
  upper: None,
  lowerBound: Unbounded,
  upperBound: Unbounded,
}

/** Create a singleton interval [value, value] */
let singleton = (value: float): interval => {
  {
    lower: Some(value),
    upper: Some(value),
    lowerBound: Closed,
    upperBound: Closed,
  }
}

/** Check if this interval is empty */
let isEmpty = (i: interval): bool => {
  switch (i.lower, i.upper) {
  | (None, _) | (_, None) => false
  | (Some(l), Some(u)) =>
    if l > u {
      true
    } else if l == u {
      i.lowerBound == Open || i.upperBound == Open
    } else {
      false
    }
  }
}

/** Check if this interval contains a value */
let contains = (i: interval, value: float): bool => {
  // Check lower bound
  let passesLower = switch i.lower {
  | None => true
  | Some(l) =>
    switch i.lowerBound {
    | Closed => value >= l
    | Open => value > l
    | Unbounded => true
    }
  }

  // Check upper bound
  let passesUpper = switch i.upper {
  | None => true
  | Some(u) =>
    switch i.upperBound {
    | Closed => value <= u
    | Open => value < u
    | Unbounded => true
    }
  }

  passesLower && passesUpper
}

/** Check if this interval fully contains another interval */
let containsInterval = (outer: interval, inner: interval): bool => {
  // Check lower bound
  let passesLower = switch outer.lower {
  | None => true
  | Some(outerL) =>
    switch inner.lower {
    | None => false // inner has no lower bound but outer does
    | Some(innerL) =>
      if innerL < outerL {
        false
      } else if innerL == outerL {
        !(outer.lowerBound == Open && inner.lowerBound == Closed)
      } else {
        true
      }
    }
  }

  // Check upper bound
  let passesUpper = switch outer.upper {
  | None => true
  | Some(outerU) =>
    switch inner.upper {
    | None => false // inner has no upper bound but outer does
    | Some(innerU) =>
      if innerU > outerU {
        false
      } else if innerU == outerU {
        !(outer.upperBound == Open && inner.upperBound == Closed)
      } else {
        true
      }
    }
  }

  passesLower && passesUpper
}

/** Check if this interval overlaps with another */
let overlaps = (a: interval, b: interval): bool => {
  // Check if a's upper is below b's lower
  let aBeforeB = switch (a.upper, b.lower) {
  | (Some(au), Some(bl)) =>
    if au < bl {
      true
    } else if au == bl {
      a.upperBound == Open || b.lowerBound == Open
    } else {
      false
    }
  | _ => false
  }

  // Check if b's upper is below a's lower
  let bBeforeA = switch (b.upper, a.lower) {
  | (Some(bu), Some(al)) =>
    if bu < al {
      true
    } else if bu == al {
      b.upperBound == Open || a.lowerBound == Open
    } else {
      false
    }
  | _ => false
  }

  !aBeforeB && !bBeforeA
}

/** Compute the intersection of two intervals */
let intersection = (a: interval, b: interval): option<interval> => {
  if !overlaps(a, b) {
    None
  } else {
    // Determine new lower bound
    let (newLower, newLowerBound) = switch (a.lower, b.lower) {
    | (None, None) => (None, Unbounded)
    | (Some(al), None) => (Some(al), a.lowerBound)
    | (None, Some(bl)) => (Some(bl), b.lowerBound)
    | (Some(al), Some(bl)) =>
      if al > bl {
        (Some(al), a.lowerBound)
      } else if bl > al {
        (Some(bl), b.lowerBound)
      } else {
        // Equal - use more restrictive bound
        let bound = if a.lowerBound == Open || b.lowerBound == Open {
          Open
        } else {
          Closed
        }
        (Some(al), bound)
      }
    }

    // Determine new upper bound
    let (newUpper, newUpperBound) = switch (a.upper, b.upper) {
    | (None, None) => (None, Unbounded)
    | (Some(au), None) => (Some(au), a.upperBound)
    | (None, Some(bu)) => (Some(bu), b.upperBound)
    | (Some(au), Some(bu)) =>
      if au < bu {
        (Some(au), a.upperBound)
      } else if bu < au {
        (Some(bu), b.upperBound)
      } else {
        // Equal - use more restrictive bound
        let bound = if a.upperBound == Open || b.upperBound == Open {
          Open
        } else {
          Closed
        }
        (Some(au), bound)
      }
    }

    let result = {
      lower: newLower,
      upper: newUpper,
      lowerBound: newLowerBound,
      upperBound: newUpperBound,
    }

    if isEmpty(result) {
      None
    } else {
      Some(result)
    }
  }
}

/** Compute the span (convex hull) of two intervals */
let span = (a: interval, b: interval): interval => {
  // Determine new lower bound
  let (newLower, newLowerBound) = if a.lowerBound == Unbounded || b.lowerBound == Unbounded {
    (None, Unbounded)
  } else {
    switch (a.lower, b.lower) {
    | (None, None) => (None, Unbounded)
    | (Some(al), None) => (Some(al), a.lowerBound)
    | (None, Some(bl)) => (Some(bl), b.lowerBound)
    | (Some(al), Some(bl)) =>
      if al < bl {
        (Some(al), a.lowerBound)
      } else if bl < al {
        (Some(bl), b.lowerBound)
      } else {
        // Equal - use less restrictive bound
        let bound = if a.lowerBound == Closed || b.lowerBound == Closed {
          Closed
        } else {
          Open
        }
        (Some(al), bound)
      }
    }
  }

  // Determine new upper bound
  let (newUpper, newUpperBound) = if a.upperBound == Unbounded || b.upperBound == Unbounded {
    (None, Unbounded)
  } else {
    switch (a.upper, b.upper) {
    | (None, None) => (None, Unbounded)
    | (Some(au), None) => (Some(au), a.upperBound)
    | (None, Some(bu)) => (Some(bu), b.upperBound)
    | (Some(au), Some(bu)) =>
      if au > bu {
        (Some(au), a.upperBound)
      } else if bu > au {
        (Some(bu), b.upperBound)
      } else {
        // Equal - use less restrictive bound
        let bound = if a.upperBound == Closed || b.upperBound == Closed {
          Closed
        } else {
          Open
        }
        (Some(au), bound)
      }
    }
  }

  {
    lower: newLower,
    upper: newUpper,
    lowerBound: newLowerBound,
    upperBound: newUpperBound,
  }
}

/** Get the length/size of the interval (upper - lower)
 * Returns None for unbounded intervals
 */
let length = (i: interval): option<float> => {
  switch (i.lower, i.upper) {
  | (Some(l), Some(u)) => Some(u -. l)
  | _ => None
  }
}

/** Get the midpoint of the interval
 * Returns None for unbounded intervals
 */
let midpoint = (i: interval): option<float> => {
  switch (i.lower, i.upper) {
  | (Some(l), Some(u)) => Some(l +. (u -. l) /. 2.0)
  | _ => None
  }
}

/** Clamp a value to this interval */
let clamp = (i: interval, value: float): float => {
  let result = ref(value)

  switch i.lower {
  | Some(l) =>
    switch i.lowerBound {
    | Closed =>
      if result.contents < l {
        result := l
      }
    | Open =>
      if result.contents <= l {
        // Best effort for open bound - use slightly above
        result := l +. 1.0e-10
      }
    | Unbounded => ()
    }
  | None => ()
  }

  switch i.upper {
  | Some(u) =>
    switch i.upperBound {
    | Closed =>
      if result.contents > u {
        result := u
      }
    | Open =>
      if result.contents >= u {
        // Best effort for open bound - use slightly below
        result := u -. 1.0e-10
      }
    | Unbounded => ()
    }
  | None => ()
  }

  result.contents
}

/** Check if two intervals are equal */
let equal = (a: interval, b: interval): bool => {
  a.lowerBound == b.lowerBound &&
  a.upperBound == b.upperBound &&
  a.lower == b.lower &&
  a.upper == b.upper
}

/** Check if this interval is a proper subset of another */
let isProperSubsetOf = (inner: interval, outer: interval): bool => {
  containsInterval(outer, inner) && !equal(inner, outer)
}

/** Format the interval as a string */
let toString = (i: interval): string => {
  let leftBracket = switch i.lowerBound {
  | Closed => "["
  | Open | Unbounded => "("
  }

  let lowerStr = switch i.lower {
  | Some(l) => Belt.Float.toString(l)
  | None => "-inf"
  }

  let upperStr = switch i.upper {
  | Some(u) => Belt.Float.toString(u)
  | None => "+inf"
  }

  let rightBracket = switch i.upperBound {
  | Closed => "]"
  | Open | Unbounded => ")"
  }

  `${leftBracket}${lowerStr}, ${upperStr}${rightBracket}`
}

/** Integer interval utilities */

/** Create a closed integer interval [lower, upper] */
let closedInt = (lower: int, upper: int): result<interval, intervalError> => {
  closed(Belt.Int.toFloat(lower), Belt.Int.toFloat(upper))
}

/** Create an open integer interval (lower, upper) */
let openInt = (lower: int, upper: int): result<interval, intervalError> => {
  open_(Belt.Int.toFloat(lower), Belt.Int.toFloat(upper))
}

/** Check if a value is in a closed range [lower, upper] */
let inRange = (value: float, lower: float, upper: float): bool => {
  value >= lower && value <= upper
}

/** Check if a value is in an open range (lower, upper) */
let inOpenRange = (value: float, lower: float, upper: float): bool => {
  value > lower && value < upper
}

/** Clamp a value to a closed range [lower, upper] */
let clampToRange = (value: float, lower: float, upper: float): float => {
  if value < lower {
    lower
  } else if value > upper {
    upper
  } else {
    value
  }
}

/** Check if an integer is in a closed range [lower, upper] */
let inRangeInt = (value: int, lower: int, upper: int): bool => {
  value >= lower && value <= upper
}

/** Clamp an integer to a closed range [lower, upper] */
let clampToRangeInt = (value: int, lower: int, upper: int): int => {
  if value < lower {
    lower
  } else if value > upper {
    upper
  } else {
    value
  }
}

/** Normalize a value from one range to another */
let normalize = (value: float, fromLower: float, fromUpper: float, toLower: float, toUpper: float): float => {
  let fromRange = fromUpper -. fromLower
  if fromRange == 0.0 {
    toLower
  } else {
    let normalized = (value -. fromLower) /. fromRange
    toLower +. normalized *. (toUpper -. toLower)
  }
}

/** Lerp (linear interpolation) within an interval */
let lerp = (i: interval, t: float): option<float> => {
  switch (i.lower, i.upper) {
  | (Some(l), Some(u)) => Some(l +. t *. (u -. l))
  | _ => None
  }
}

/** Get the position of a value within an interval as a ratio [0, 1] */
let inverseLerp = (i: interval, value: float): option<float> => {
  switch (i.lower, i.upper) {
  | (Some(l), Some(u)) =>
    let range = u -. l
    if range == 0.0 {
      Some(0.0)
    } else {
      Some((value -. l) /. range)
    }
  | _ => None
  }
}

/** Expand an interval by a delta amount on both sides */
let expand = (i: interval, delta: float): interval => {
  {
    lower: Belt.Option.map(i.lower, l => l -. delta),
    upper: Belt.Option.map(i.upper, u => u +. delta),
    lowerBound: i.lowerBound,
    upperBound: i.upperBound,
  }
}

/** Contract an interval by a delta amount on both sides */
let contract = (i: interval, delta: float): option<interval> => {
  let newLower = Belt.Option.map(i.lower, l => l +. delta)
  let newUpper = Belt.Option.map(i.upper, u => u -. delta)

  // Check if the interval would become invalid
  switch (newLower, newUpper) {
  | (Some(l), Some(u)) =>
    if l > u {
      None
    } else {
      Some({
        lower: newLower,
        upper: newUpper,
        lowerBound: i.lowerBound,
        upperBound: i.upperBound,
      })
    }
  | _ =>
    Some({
      lower: newLower,
      upper: newUpper,
      lowerBound: i.lowerBound,
      upperBound: i.upperBound,
    })
  }
}

/** Check if the interval is bounded (has both finite endpoints) */
let isBounded = (i: interval): bool => {
  Belt.Option.isSome(i.lower) && Belt.Option.isSome(i.upper)
}

/** Check if the interval is unbounded on the lower end */
let isLowerUnbounded = (i: interval): bool => {
  i.lowerBound == Unbounded || Belt.Option.isNone(i.lower)
}

/** Check if the interval is unbounded on the upper end */
let isUpperUnbounded = (i: interval): bool => {
  i.upperBound == Unbounded || Belt.Option.isNone(i.upper)
}
