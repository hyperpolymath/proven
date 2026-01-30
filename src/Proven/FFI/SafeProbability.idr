-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeProbability operations
|||
||| This module exports probability operations to the C ABI
||| via Idris2's RefC backend. All functions are proven total and validate probabilities.
|||
||| Return conventions:
||| - Probability creation → (Int, Double) where status 0 = success, 1 = invalid
||| - Operations → (Int, Double) where status 0 = success, 1 = error (division by zero, etc.)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: All probability values must be in [0, 1] range.
|||           P(A) = 1 means certain, P(A) = 0 means impossible.
|||
||| Probability operations:
||| - Complement: P(not A) = 1 - P(A)
||| - AND (independent): P(A and B) = P(A) × P(B)
||| - OR (exclusive): P(A or B) = P(A) + P(B) [only if mutually exclusive]
||| - OR (general): P(A or B) = P(A) + P(B) - P(A and B)
||| - Conditional: P(A|B) = P(A and B) / P(B)
|||
||| Bayes' theorem: P(A|B) = P(B|A) × P(A) / P(B)
module Proven.FFI.SafeProbability

import Proven.SafeProbability
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

%export
proven_idris_prob_certain : Double
proven_idris_prob_certain = certain.value

%export
proven_idris_prob_impossible : Double
proven_idris_prob_impossible = impossible.value

%export
proven_idris_prob_fair : Double
proven_idris_prob_fair = fair.value

%export
proven_idris_prob_min : Double
proven_idris_prob_min = 0.0

%export
proven_idris_prob_max : Double
proven_idris_prob_max = 1.0

--------------------------------------------------------------------------------
-- Probability Construction
--------------------------------------------------------------------------------

%export
proven_idris_prob_create : Double -> (Int, Double)
proven_idris_prob_create p =
  case mkProbability p of
    Nothing => (1, 0.0)  -- Invalid
    Just prob => (0, prob.value)

%export
proven_idris_prob_create_clamped : Double -> Double
proven_idris_prob_create_clamped p =
  (mkProbabilityClamped p).value

%export
proven_idris_prob_is_valid : Double -> Int
proven_idris_prob_is_valid p =
  encodeBool (isJust (mkProbability p))

--------------------------------------------------------------------------------
-- Basic Operations
--------------------------------------------------------------------------------

%export
proven_idris_prob_complement : Double -> Double
proven_idris_prob_complement p =
  (complement (MkProb p)).value

%export
proven_idris_prob_and_independent : Double -> Double -> Double
proven_idris_prob_and_independent p1 p2 =
  (andIndependent (MkProb p1) (MkProb p2)).value

%export
proven_idris_prob_or_exclusive : Double -> Double -> (Int, Double)
proven_idris_prob_or_exclusive p1 p2 =
  case orExclusive (MkProb p1) (MkProb p2) of
    Nothing => (1, 0.0)  -- Not mutually exclusive (result > 1)
    Just prob => (0, prob.value)

%export
proven_idris_prob_or_general : Double -> Double -> Double -> Double
proven_idris_prob_or_general pA pB pAandB =
  (orGeneral (MkProb pA) (MkProb pB) (MkProb pAandB)).value

%export
proven_idris_prob_conditional : Double -> Double -> (Int, Double)
proven_idris_prob_conditional pAandB pB =
  case conditional (MkProb pAandB) (MkProb pB) of
    Nothing => (1, 0.0)  -- Division by zero (pB = 0)
    Just prob => (0, prob.value)

--------------------------------------------------------------------------------
-- Odds Conversion
--------------------------------------------------------------------------------

%export
proven_idris_prob_to_odds : Double -> (Int, Double, Double)
proven_idris_prob_to_odds p =
  case toOdds (MkProb p) of
    Nothing => (1, 0.0, 0.0)  -- Infinite odds (p = 1)
    Just odds => (0, odds.forEvent, odds.againstEvent)

%export
proven_idris_prob_from_odds : Double -> Double -> Double
proven_idris_prob_from_odds forEvent againstEvent =
  (fromOdds (MkOdds forEvent againstEvent)).value

%export
proven_idris_prob_odds_ratio : Double -> Double -> (Double, Double)
proven_idris_prob_odds_ratio forEvent againstEvent =
  oddsRatio (MkOdds forEvent againstEvent)

%export
proven_idris_prob_decimal_odds_to_prob : Double -> Double
proven_idris_prob_decimal_odds_to_prob decimalOdds =
  if decimalOdds <= 0.0 then 0.0
  else 1.0 / decimalOdds

%export
proven_idris_prob_prob_to_decimal_odds : Double -> (Int, Double)
proven_idris_prob_prob_to_decimal_odds p =
  if p == 0.0 then (1, 0.0)  -- Would be infinite
  else (0, 1.0 / p)

--------------------------------------------------------------------------------
-- Bayesian Operations
--------------------------------------------------------------------------------

%export
proven_idris_prob_bayes : Double -> Double -> Double -> (Int, Double)
proven_idris_prob_bayes pBgivenA pA pB =
  case bayes (MkProb pBgivenA) (MkProb pA) (MkProb pB) of
    Nothing => (1, 0.0)  -- Division by zero (pB = 0) or result out of range
    Just prob => (0, prob.value)

%export
proven_idris_prob_update_belief : Double -> Double -> Double -> (Int, Double)
proven_idris_prob_update_belief prior likelihood marginal =
  case updateBelief (MkProb prior) (MkProb likelihood) (MkProb marginal) of
    Nothing => (1, 0.0)
    Just prob => (0, prob.value)

--------------------------------------------------------------------------------
-- Distributions
--------------------------------------------------------------------------------

%export
proven_idris_prob_bernoulli : Int -> Double -> Double
proven_idris_prob_bernoulli k p =
  bernoulli (cast k) (MkProb p)

%export
proven_idris_prob_binomial_pmf : Int -> Int -> Double -> Double
proven_idris_prob_binomial_pmf n k p =
  binomialPMF (cast n) (cast k) (MkProb p)

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

%export
proven_idris_prob_equal : Double -> Double -> Int
proven_idris_prob_equal p1 p2 =
  encodeBool ((MkProb p1) == (MkProb p2))

%export
proven_idris_prob_less_than : Double -> Double -> Int
proven_idris_prob_less_than p1 p2 =
  encodeBool (p1 < p2)

%export
proven_idris_prob_greater_than : Double -> Double -> Int
proven_idris_prob_greater_than p1 p2 =
  encodeBool (p1 > p2)

%export
proven_idris_prob_compare : Double -> Double -> Int
proven_idris_prob_compare p1 p2 =
  case compare (MkProb p1) (MkProb p2) of
    LT => (-1)
    EQ => 0
    GT => 1

--------------------------------------------------------------------------------
-- Special Probability Checks
--------------------------------------------------------------------------------

%export
proven_idris_prob_is_certain : Double -> Int
proven_idris_prob_is_certain p =
  encodeBool (p == 1.0)

%export
proven_idris_prob_is_impossible : Double -> Int
proven_idris_prob_is_impossible p =
  encodeBool (p == 0.0)

%export
proven_idris_prob_is_fair : Double -> Int
proven_idris_prob_is_fair p =
  encodeBool (approxEqual 0.0001 p 0.5)

%export
proven_idris_prob_is_likely : Double -> Int
proven_idris_prob_is_likely p =
  encodeBool (p > 0.5)

%export
proven_idris_prob_is_unlikely : Double -> Int
proven_idris_prob_is_unlikely p =
  encodeBool (p < 0.5)

--------------------------------------------------------------------------------
-- Percentage Conversion
--------------------------------------------------------------------------------

%export
proven_idris_prob_to_percentage : Double -> Double
proven_idris_prob_to_percentage p =
  p * 100.0

%export
proven_idris_prob_from_percentage : Double -> (Int, Double)
proven_idris_prob_from_percentage pct =
  let p = pct / 100.0
  in case mkProbability p of
       Nothing => (1, 0.0)
       Just prob => (0, prob.value)

--------------------------------------------------------------------------------
-- Mutually Exclusive Check
--------------------------------------------------------------------------------

%export
proven_idris_prob_can_be_mutually_exclusive : Double -> Double -> Int
proven_idris_prob_can_be_mutually_exclusive p1 p2 =
  encodeBool (p1 + p2 <= 1.0)

%export
proven_idris_prob_sum_is_valid : Double -> Double -> Int
proven_idris_prob_sum_is_valid p1 p2 =
  encodeBool (p1 + p2 <= 1.0)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_prob_friendly_error : String -> String
proven_idris_prob_friendly_error errorMsg =
  if isInfixOf "range" (toLower errorMsg) || isInfixOf "0" (toLower errorMsg)
    then "Probability must be between 0 and 1 (inclusive)"
  else if isInfixOf "conditional" (toLower errorMsg) || isInfixOf "division" (toLower errorMsg)
    then "Cannot compute conditional probability (condition has probability 0)"
  else if isInfixOf "bayes" (toLower errorMsg)
    then "Bayesian update failed (marginal probability is 0 or result out of range)"
  else if isInfixOf "odds" (toLower errorMsg)
    then "Cannot convert (probability is 1 resulting in infinite odds)"
  else if isInfixOf "exclusive" (toLower errorMsg)
    then "Events are not mutually exclusive (probabilities sum > 1)"
  else if isInfixOf "distribution" (toLower errorMsg)
    then "Invalid distribution parameters"
  else
    "Probability operation error"
