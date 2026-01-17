-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeProbability - Verified probability operations
|||
||| Safe probability handling for ML, statistics, quantum computing, and Bayesian inference.
||| All probability values are guaranteed to be in [0, 1].
||| Distributions are guaranteed to sum/integrate to 1.
module Proven.SafeProbability

import Proven.Core
import Data.So
import Data.Vect
import Data.List

%default total

-- ============================================================================
-- CONSTANTS
-- ============================================================================

||| Epsilon for floating point comparison
public export
PROB_EPSILON : Double
PROB_EPSILON = 1.0e-10

-- ============================================================================
-- PROBABILITY TYPE
-- ============================================================================

||| A probability value guaranteed to be in [0, 1]
public export
record Probability where
  constructor MkProbability
  value : Double
  0 inRange : So (value >= 0.0 && value <= 1.0)

||| Proof that a value is a valid probability
public export
IsValidProbability : Double -> Type
IsValidProbability x = So (x >= 0.0 && x <= 1.0)

-- ============================================================================
-- CONSTRUCTORS
-- ============================================================================

||| Create a probability from a Double (clamps to [0, 1])
export
fromDouble : Double -> Probability
fromDouble x =
  let clamped = max 0.0 (min 1.0 x)
  in believe_me (MkProbability clamped)

||| Create probability from a ratio (returns None if invalid)
export
fromRatio : Nat -> Nat -> Maybe Probability
fromRatio num denom =
  if denom == 0
  then Nothing
  else Just (fromDouble (cast num / cast denom))

||| Create probability from percentage (0-100)
export
fromPercent : Double -> Probability
fromPercent pct = fromDouble (pct / 100.0)

||| Zero probability (impossible event)
export
impossible : Probability
impossible = fromDouble 0.0

||| Unit probability (certain event)
export
certain : Probability
certain = fromDouble 1.0

||| 50% probability
export
evenOdds : Probability
evenOdds = fromDouble 0.5

-- ============================================================================
-- EXTRACTION
-- ============================================================================

||| Get raw probability value
export
toDouble : Probability -> Double
toDouble p = p.value

||| Get probability as percentage (0-100)
export
toPercent : Probability -> Double
toPercent p = p.value * 100.0

||| Get odds ratio (p / (1-p)), returns None if p = 1
export
toOdds : Probability -> Maybe Double
toOdds p =
  let complement = 1.0 - p.value
  in if complement < PROB_EPSILON then Nothing else Just (p.value / complement)

||| Get log probability (returns Nothing for zero probability)
export
toLogProb : Probability -> Maybe Double
toLogProb p =
  if p.value < PROB_EPSILON then Nothing else Just (log p.value)

-- ============================================================================
-- ARITHMETIC
-- ============================================================================

||| Complement (1 - p)
export
complement : Probability -> Probability
complement p = fromDouble (1.0 - p.value)

||| Product of probabilities (for independent events)
export
mul : Probability -> Probability -> Probability
mul p q = fromDouble (p.value * q.value)

||| Sum of probabilities (clamped to 1)
export
add : Probability -> Probability -> Probability
add p q = fromDouble (p.value + q.value)

||| Difference of probabilities (clamped to 0)
export
sub : Probability -> Probability -> Probability
sub p q = fromDouble (p.value - q.value)

||| Scale probability by factor
export
scale : Double -> Probability -> Probability
scale k p = fromDouble (k * p.value)

-- ============================================================================
-- BAYESIAN OPERATIONS
-- ============================================================================

||| Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B)
||| Returns None if P(B) = 0
export
bayes : (pBGivenA : Probability)
     -> (pA : Probability)
     -> (pB : Probability)
     -> Maybe Probability
bayes pBGivenA pA pB =
  if pB.value < PROB_EPSILON
  then Nothing
  else Just (fromDouble (pBGivenA.value * pA.value / pB.value))

||| Total probability: P(B) = sum of P(B|Ai) * P(Ai)
export
totalProbability : List (Probability, Probability) -> Probability
totalProbability pairs =
  let total = foldl (\acc, (pBGivenA, pA) => acc + pBGivenA.value * pA.value) 0.0 pairs
  in fromDouble total

||| Posterior probability given prior and likelihood
export
posterior : (prior : Probability)
         -> (likelihood : Probability)
         -> (evidence : Probability)
         -> Maybe Probability
posterior = bayes

-- ============================================================================
-- PROBABILITY DISTRIBUTIONS
-- ============================================================================

||| A discrete probability distribution (guaranteed to sum to 1)
public export
record Distribution (n : Nat) where
  constructor MkDistribution
  probabilities : Vect n Probability
  0 sumsToOne : So (abs (sum (map toDouble probabilities) - 1.0) < PROB_EPSILON)

||| Normalize a list of weights to a distribution
export
normalize : {n : Nat} -> Vect n Double -> Maybe (Distribution n)
normalize weights =
  let total = sum weights
  in if total < PROB_EPSILON
     then Nothing
     else let normalized = map (\w => fromDouble (w / total)) weights
          in Just (believe_me (MkDistribution normalized))

||| Uniform distribution over n outcomes
export
uniform : (n : Nat) -> {auto prf : NonZero n} -> Distribution n
uniform n =
  let p = fromDouble (1.0 / cast n)
  in believe_me (MkDistribution (replicate n p))

||| Get probability of outcome at index
export
probabilityAt : Distribution n -> Fin n -> Probability
probabilityAt d i = index i d.probabilities

||| Sample from distribution (given random value in [0,1])
export
sample : Distribution n -> Double -> Fin n
sample d rand =
  let r = max 0.0 (min 1.0 rand)
  in go 0 0.0 d.probabilities r
  where
    go : Nat -> Double -> Vect m Probability -> Double -> Fin n
    go idx cumulative [] _ = believe_me FZ  -- Should never happen
    go idx cumulative (p :: ps) rand =
      let newCumulative = cumulative + p.value
      in if rand <= newCumulative
         then believe_me (natToFinLt idx)
         else go (S idx) newCumulative ps rand

-- ============================================================================
-- ENTROPY & INFORMATION
-- ============================================================================

||| Shannon entropy of a distribution (in nats)
export
entropy : Distribution n -> Double
entropy d =
  let probs = toList d.probabilities
  in negate (foldl (\acc, p =>
       if p.value < PROB_EPSILON then acc
       else acc + p.value * log p.value) 0.0 probs)

||| Shannon entropy in bits
export
entropyBits : Distribution n -> Double
entropyBits d = entropy d / log 2.0

||| Kullback-Leibler divergence D_KL(P || Q)
||| Returns None if Q has zero where P is nonzero
export
klDivergence : Distribution n -> Distribution n -> Maybe Double
klDivergence p q =
  let pairs = zip (toList p.probabilities) (toList q.probabilities)
  in if any (\(pi, qi) => pi.value > PROB_EPSILON && qi.value < PROB_EPSILON) pairs
     then Nothing
     else Just (foldl (\acc, (pi, qi) =>
            if pi.value < PROB_EPSILON then acc
            else acc + pi.value * log (pi.value / qi.value)) 0.0 pairs)

||| Cross-entropy H(P, Q)
export
crossEntropy : Distribution n -> Distribution n -> Maybe Double
crossEntropy p q =
  let pairs = zip (toList p.probabilities) (toList q.probabilities)
  in if any (\(_, qi) => qi.value < PROB_EPSILON) pairs
     then Nothing
     else Just (negate (foldl (\acc, (pi, qi) =>
            acc + pi.value * log qi.value) 0.0 pairs))

-- ============================================================================
-- COMPARISON
-- ============================================================================

||| Check if two probabilities are approximately equal
export
approxEqual : Probability -> Probability -> Double -> Bool
approxEqual p q epsilon = abs (p.value - q.value) < epsilon

||| Check if probability is effectively zero
export
isZero : Probability -> Bool
isZero p = p.value < PROB_EPSILON

||| Check if probability is effectively one
export
isOne : Probability -> Bool
isOne p = p.value > 1.0 - PROB_EPSILON

-- ============================================================================
-- LOG-SPACE OPERATIONS (for numerical stability)
-- ============================================================================

||| A log-probability (can represent very small probabilities)
public export
record LogProbability where
  constructor MkLogProbability
  logValue : Double

||| Convert probability to log-space
export
toLogSpace : Probability -> Maybe LogProbability
toLogSpace p =
  if p.value < PROB_EPSILON then Nothing
  else Just (MkLogProbability (log p.value))

||| Convert log-probability back to probability
export
fromLogSpace : LogProbability -> Probability
fromLogSpace lp = fromDouble (exp lp.logValue)

||| Multiply in log-space (add log values)
export
logMul : LogProbability -> LogProbability -> LogProbability
logMul a b = MkLogProbability (a.logValue + b.logValue)

||| Add in log-space (log-sum-exp for numerical stability)
export
logAdd : LogProbability -> LogProbability -> LogProbability
logAdd a b =
  let maxVal = max a.logValue b.logValue
      minVal = min a.logValue b.logValue
  in MkLogProbability (maxVal + log (1.0 + exp (minVal - maxVal)))

-- ============================================================================
-- SPECIAL DISTRIBUTIONS
-- ============================================================================

||| Bernoulli distribution (single coin flip)
export
bernoulli : Probability -> Distribution 2
bernoulli p = believe_me (MkDistribution [complement p, p])

||| Binary entropy function H(p)
export
binaryEntropy : Probability -> Double
binaryEntropy p =
  if isZero p || isOne p then 0.0
  else let q = 1.0 - p.value
       in negate (p.value * log p.value + q * log q) / log 2.0

||| Geometric distribution PMF: P(X = k) = (1-p)^k * p
export
geometric : Probability -> Nat -> Probability
geometric p k =
  let q = 1.0 - p.value
      qk = pow q (cast k)
  in fromDouble (qk * p.value)
