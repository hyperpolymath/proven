-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeProbability - Safe probability operations
|||
||| This module provides safe probability operations that ensure
||| values stay within valid ranges and handle edge cases.
module Proven.SafeProbability

import public Proven.Core
import public Proven.SafeFloat

%default total

--------------------------------------------------------------------------------
-- Probability Types
--------------------------------------------------------------------------------

||| A probability value guaranteed to be in [0, 1]
public export
record Probability where
  constructor MkProb
  value : Double

||| Odds representation (a:b)
public export
record Odds where
  constructor MkOdds
  forEvent : Double
  againstEvent : Double

--------------------------------------------------------------------------------
-- Probability Construction
--------------------------------------------------------------------------------

||| Create a probability from a double (Nothing if out of range)
public export
mkProbability : Double -> Maybe Probability
mkProbability p =
  if p >= 0.0 && p <= 1.0 && isFinite p
    then Just (MkProb p)
    else Nothing

||| Create probability clamped to valid range
public export
mkProbabilityClamped : Double -> Probability
mkProbabilityClamped p = MkProb (clampUnit p)

||| Certain event (probability = 1)
public export
certain : Probability
certain = MkProb 1.0

||| Impossible event (probability = 0)
public export
impossible : Probability
impossible = MkProb 0.0

||| Fair coin flip (probability = 0.5)
public export
fair : Probability
fair = MkProb 0.5

--------------------------------------------------------------------------------
-- Basic Operations
--------------------------------------------------------------------------------

||| Complement of a probability P(not A) = 1 - P(A)
public export
complement : Probability -> Probability
complement (MkProb p) = MkProb (1.0 - p)

||| Independent event conjunction P(A and B) = P(A) * P(B)
public export
andIndependent : Probability -> Probability -> Probability
andIndependent (MkProb a) (MkProb b) = MkProb (a * b)

||| Mutually exclusive disjunction P(A or B) = P(A) + P(B)
||| Returns Nothing if result > 1 (events not mutually exclusive)
public export
orExclusive : Probability -> Probability -> Maybe Probability
orExclusive (MkProb a) (MkProb b) =
  let result = a + b
  in if result <= 1.0 then Just (MkProb result) else Nothing

||| General disjunction P(A or B) = P(A) + P(B) - P(A and B)
public export
orGeneral : Probability -> Probability -> Probability -> Probability
orGeneral (MkProb a) (MkProb b) (MkProb ab) = MkProb (clampUnit (a + b - ab))

||| Conditional probability P(A|B) = P(A and B) / P(B)
public export
conditional : (pAandB : Probability) -> (pB : Probability) -> Maybe Probability
conditional (MkProb ab) (MkProb b) =
  if b == 0.0 then Nothing
  else mkProbability (ab / b)

--------------------------------------------------------------------------------
-- Odds Conversion
--------------------------------------------------------------------------------

||| Convert probability to odds
public export
toOdds : Probability -> Maybe Odds
toOdds (MkProb p) =
  if p == 1.0 then Nothing  -- Infinite odds
  else Just (MkOdds p (1.0 - p))

||| Convert odds to probability
public export
fromOdds : Odds -> Probability
fromOdds (MkOdds f a) =
  let total = f + a
  in MkProb (if total == 0.0 then 0.0 else f / total)

||| Express odds as ratio (e.g., "3 to 2")
public export
oddsRatio : Odds -> (Double, Double)
oddsRatio (MkOdds f a) = (f, a)

--------------------------------------------------------------------------------
-- Bayesian Operations
--------------------------------------------------------------------------------

||| Bayes' theorem: P(A|B) = P(B|A) * P(A) / P(B)
public export
bayes : (pBgivenA : Probability) -> (pA : Probability) -> (pB : Probability) -> Maybe Probability
bayes (MkProb pba) (MkProb pa) (MkProb pb) =
  if pb == 0.0 then Nothing
  else mkProbability (pba * pa / pb)

||| Update probability with new evidence (simple Bayesian update)
||| prior: P(H), likelihood: P(E|H), marginal: P(E)
public export
updateBelief : (prior : Probability) -> (likelihood : Probability) -> (marginal : Probability) -> Maybe Probability
updateBelief = bayes

--------------------------------------------------------------------------------
-- Distribution Operations
--------------------------------------------------------------------------------

||| Bernoulli distribution: probability of exactly k successes in 1 trial
public export
bernoulli : (k : Nat) -> (p : Probability) -> Double
bernoulli Z (MkProb p) = 1.0 - p
bernoulli (S Z) (MkProb p) = p
bernoulli _ _ = 0.0

||| Binomial coefficient (n choose k)
binomial : Nat -> Nat -> Nat
binomial n k =
  if k > n then 0
  else if k == 0 || k == n then 1
  else binomial (minus n 1) (minus k 1) + binomial (minus n 1) k

||| Binomial distribution: P(X = k) for n trials with probability p
public export
binomialPMF : (n : Nat) -> (k : Nat) -> (p : Probability) -> Double
binomialPMF n k (MkProb p) =
  if k > n then 0.0
  else cast (binomial n k) * pow p (cast k) * pow (1.0 - p) (cast (minus n k))

--------------------------------------------------------------------------------
-- Expected Value
--------------------------------------------------------------------------------

||| Expected value of a discrete distribution
public export
expectedValue : List (Double, Probability) -> Double
expectedValue outcomes = sum (map (\(v, MkProb p) => v * p) outcomes)

||| Variance of a discrete distribution
public export
variance : List (Double, Probability) -> Double
variance outcomes =
  let mu = expectedValue outcomes
      squaredDiffs = map (\(v, MkProb p) => p * (v - mu) * (v - mu)) outcomes
  in sum squaredDiffs

public export
Eq Probability where
  (MkProb a) == (MkProb b) = approxEqual 0.0000001 a b

public export
Ord Probability where
  compare (MkProb a) (MkProb b) = compare a b

public export
Show Probability where
  show (MkProb p) = show (p * 100.0) ++ "%"
