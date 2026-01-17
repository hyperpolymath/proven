// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeProbability - Probability operations with value clamping that cannot crash.
 *
 * Provides safe probability calculations including combinations, Bayes' theorem,
 * binomial distribution, and expected value calculations.
 */

/** Error types for probability operations */
type probabilityError =
  | OutOfRange
  | InvalidOdds
  | InvalidProbabilities
  | DivisionByZero

/** Probability value constrained to [0, 1] */
type probability = {value: float}

/** Create a probability with validation */
let make = (value: float): result<probability, probabilityError> => {
  if value < 0.0 || value > 1.0 {
    Error(OutOfRange)
  } else {
    Ok({value: value})
  }
}

/** Create a probability, clamping to valid range [0, 1] */
let makeClamped = (value: float): probability => {
  {value: Js.Math.max_float(0.0, Js.Math.min_float(1.0, value))}
}

/** Create a probability without validation (for known-valid data) */
let makeUnsafe = (value: float): probability => {
  {value: value}
}

/** Get the raw probability value */
let getValue = (p: probability): float => {
  p.value
}

/** Probability of 0 (impossible) */
let zero: probability = {value: 0.0}

/** Probability of 1 (certain) */
let one: probability = {value: 1.0}

/** Probability of 0.5 (even odds) */
let half: probability = {value: 0.5}

/** Complement (1 - p) */
let complement = (p: probability): probability => {
  {value: 1.0 -. p.value}
}

/** Convert to percentage (0-100) */
let toPercent = (p: probability): float => {
  p.value *. 100.0
}

/** Create from percentage (0-100) */
let fromPercent = (percent: float): result<probability, probabilityError> => {
  make(percent /. 100.0)
}

/** Create from percentage, clamped */
let fromPercentClamped = (percent: float): probability => {
  makeClamped(percent /. 100.0)
}

/** Convert to odds (p / (1-p)) */
let toOdds = (p: probability): option<float> => {
  if p.value >= 1.0 {
    None
  } else {
    Some(p.value /. (1.0 -. p.value))
  }
}

/** Create from odds */
let fromOdds = (odds: float): result<probability, probabilityError> => {
  if odds < 0.0 {
    Error(InvalidOdds)
  } else {
    make(odds /. (1.0 +. odds))
  }
}

/** AND combination (independent events): P(A and B) = P(A) * P(B) */
let and_ = (p1: probability, p2: probability): probability => {
  {value: p1.value *. p2.value}
}

/** AND combination for multiple independent events */
let andAll = (probs: array<probability>): probability => {
  let result = Belt.Array.reduce(probs, 1.0, (acc, p) => acc *. p.value)
  {value: result}
}

/** OR combination (independent events): P(A or B) = P(A) + P(B) - P(A)P(B) */
let or_ = (p1: probability, p2: probability): probability => {
  {value: p1.value +. p2.value -. p1.value *. p2.value}
}

/** OR combination for multiple independent events */
let orAll = (probs: array<probability>): probability => {
  // P(A or B or C) = 1 - P(not A) * P(not B) * P(not C)
  let complementProduct = Belt.Array.reduce(probs, 1.0, (acc, p) => acc *. (1.0 -. p.value))
  {value: 1.0 -. complementProduct}
}

/** Conditional probability: P(A|B) = P(A and B) / P(B) */
let conditional = (
  pAandB: probability,
  pB: probability,
): result<probability, probabilityError> => {
  if pB.value == 0.0 {
    Error(DivisionByZero)
  } else {
    let result = pAandB.value /. pB.value
    if result > 1.0 {
      // This can happen with invalid input (P(A and B) > P(B))
      Error(InvalidProbabilities)
    } else {
      Ok({value: result})
    }
  }
}

/** Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B) */
let bayes = (
  prior: probability, // P(A)
  likelihood: probability, // P(B|A)
  evidence: probability, // P(B)
): result<probability, probabilityError> => {
  if evidence.value == 0.0 {
    Error(DivisionByZero)
  } else {
    let posterior = likelihood.value *. prior.value /. evidence.value
    if posterior > 1.0 {
      Error(InvalidProbabilities)
    } else {
      Ok({value: posterior})
    }
  }
}

/** Calculate binomial coefficient (n choose k) */
let binomialCoefficient = (n: int, k: int): float => {
  if k > n || k < 0 {
    0.0
  } else if k == 0 || k == n {
    1.0
  } else {
    let kToUse = if k > n - k {
      n - k
    } else {
      k
    } // Use smaller k for efficiency
    let result = ref(1.0)
    for i in 0 to kToUse - 1 {
      result := result.contents *. Belt.Int.toFloat(n - i) /. Belt.Int.toFloat(i + 1)
    }
    result.contents
  }
}

/** Binomial probability: P(exactly k successes in n trials) */
let binomial = (n: int, k: int, p: probability): float => {
  if k > n || k < 0 {
    0.0
  } else {
    let coeff = binomialCoefficient(n, k)
    let success = Js.Math.pow_float(~base=p.value, ~exp=Belt.Int.toFloat(k))
    let failure = Js.Math.pow_float(~base=1.0 -. p.value, ~exp=Belt.Int.toFloat(n - k))
    coeff *. success *. failure
  }
}

/** Cumulative binomial: P(at most k successes in n trials) */
let binomialCumulative = (n: int, k: int, p: probability): float => {
  let sum = ref(0.0)
  for i in 0 to k {
    sum := sum.contents +. binomial(n, i, p)
  }
  sum.contents
}

/** Expected value calculation */
let expectedValue = (
  outcomes: array<float>,
  probabilities: array<probability>,
): option<float> => {
  let numOutcomes = Belt.Array.length(outcomes)
  let numProbs = Belt.Array.length(probabilities)

  if numOutcomes != numProbs || numOutcomes == 0 {
    None
  } else {
    let sum = ref(0.0)
    let probSum = ref(0.0)

    for i in 0 to numOutcomes - 1 {
      let outcome = Belt.Array.getUnsafe(outcomes, i)
      let prob = Belt.Array.getUnsafe(probabilities, i)
      sum := sum.contents +. outcome *. prob.value
      probSum := probSum.contents +. prob.value
    }

    // Check probabilities sum to approximately 1
    if probSum.contents < 0.99 || probSum.contents > 1.01 {
      None
    } else {
      Some(sum.contents)
    }
  }
}

/** Variance calculation given expected value */
let variance = (
  outcomes: array<float>,
  probabilities: array<probability>,
  expectedVal: float,
): option<float> => {
  let numOutcomes = Belt.Array.length(outcomes)
  let numProbs = Belt.Array.length(probabilities)

  if numOutcomes != numProbs || numOutcomes == 0 {
    None
  } else {
    let sum = ref(0.0)

    for i in 0 to numOutcomes - 1 {
      let outcome = Belt.Array.getUnsafe(outcomes, i)
      let prob = Belt.Array.getUnsafe(probabilities, i)
      let diff = outcome -. expectedVal
      sum := sum.contents +. diff *. diff *. prob.value
    }

    Some(sum.contents)
  }
}

/** Standard deviation */
let standardDeviation = (
  outcomes: array<float>,
  probabilities: array<probability>,
): option<float> => {
  switch expectedValue(outcomes, probabilities) {
  | None => None
  | Some(ev) =>
    switch variance(outcomes, probabilities, ev) {
    | None => None
    | Some(v) => Some(Js.Math.sqrt(v))
    }
  }
}

/** Check if probability is certain (1.0) */
let isCertain = (p: probability): bool => {
  p.value >= 1.0
}

/** Check if probability is impossible (0.0) */
let isImpossible = (p: probability): bool => {
  p.value <= 0.0
}

/** Check if probability is likely (> 0.5) */
let isLikely = (p: probability): bool => {
  p.value > 0.5
}

/** Check if probability is unlikely (< 0.5) */
let isUnlikely = (p: probability): bool => {
  p.value < 0.5
}

/** Compare two probabilities */
let compare = (p1: probability, p2: probability): int => {
  if p1.value < p2.value {
    -1
  } else if p1.value > p2.value {
    1
  } else {
    0
  }
}

/** Check if two probabilities are equal (within epsilon) */
let equal = (p1: probability, p2: probability, ~epsilon: float=0.0001): bool => {
  Js.Math.abs_float(p1.value -. p2.value) < epsilon
}

/** Normalize an array of weights to probabilities */
let normalizeWeights = (weights: array<float>): option<array<probability>> => {
  let total = Belt.Array.reduce(weights, 0.0, (acc, w) => acc +. w)
  if total <= 0.0 {
    None
  } else {
    Some(Belt.Array.map(weights, w => {value: w /. total}))
  }
}

/** Convert probability to string representation */
let toString = (p: probability): string => {
  Belt.Float.toString(p.value)
}

/** Convert probability to percentage string */
let toPercentString = (p: probability, ~decimals: int=2): string => {
  let percent = p.value *. 100.0
  let factor = Js.Math.pow_float(~base=10.0, ~exp=Belt.Int.toFloat(decimals))
  let rounded = Js.Math.round(percent *. factor) /. factor
  `${Belt.Float.toString(rounded)}%`
}

/** Convert error to human-readable string */
let errorToString = (error: probabilityError): string => {
  switch error {
  | OutOfRange => "Probability must be between 0 and 1"
  | InvalidOdds => "Odds must be non-negative"
  | InvalidProbabilities => "Invalid probability combination"
  | DivisionByZero => "Division by zero"
  }
}
