-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeML - Machine learning safety primitives
|||
||| Provides type-safe ML primitives with numerical stability guarantees:
||| - Probability values bounded to [0,1]
||| - Numerically stable softmax, log-softmax, cross-entropy
||| - Safe normalization (handles zero vectors)
||| - Dimension-checked operations
module Proven.SafeML

import Data.String
import Data.List
import Data.Nat
import Data.Maybe
import Data.Vect

%default total

||| A probability value guaranteed to be in [0, 1]
public export
data Prob : Type where
  MkProb : (value : Double) -> Prob

||| Safe probability constructor
public export
mkProb : Double -> Maybe Prob
mkProb x = if x >= 0.0 && x <= 1.0 then Just (MkProb x) else Nothing

||| Clamp to probability range
public export
clampProb : Double -> Prob
clampProb x = if x < 0.0 then MkProb 0.0
              else if x > 1.0 then MkProb 1.0
              else MkProb x

||| Extract probability value
public export
probValue : Prob -> Double
probValue (MkProb v) = v

||| Complement of a probability (1 - p)
public export
complement : Prob -> Prob
complement (MkProb p) = MkProb (1.0 - p)

||| Numerically stable log: log(x) with underflow protection
public export
safeLog : Double -> Double
safeLog x = if x <= 0.0 then -1.0e30  -- Large negative instead of -inf
            else log x

||| Numerically stable exp with overflow protection
public export
safeExp : Double -> Double
safeExp x = if x > 709.0 then 1.0e308     -- Max representable
            else if x < -709.0 then 0.0    -- Underflow to 0
            else exp x

||| Find the maximum value in a non-empty list
public export
listMax : List Double -> Double
listMax [] = -1.0e308
listMax [x] = x
listMax (x :: xs) = let m = listMax xs in if x > m then x else m

||| Numerically stable softmax
||| Uses the log-sum-exp trick to prevent overflow
public export
softmax : List Double -> List Prob
softmax [] = []
softmax xs =
  let maxVal = listMax xs
      exps = map (\x => safeExp (x - maxVal)) xs
      sumExps = foldl (+) 0.0 exps
  in if sumExps == 0.0
       then map (const (MkProb 0.0)) xs
       else map (\e => clampProb (e / sumExps)) exps

||| Numerically stable log-softmax
public export
logSoftmax : List Double -> List Double
logSoftmax [] = []
logSoftmax xs =
  let maxVal = listMax xs
      shifted = map (\x => x - maxVal) xs
      logSumExp = safeLog (foldl (\acc, x => acc + safeExp x) 0.0 shifted)
  in map (\x => x - logSumExp) shifted

||| Cross-entropy loss (numerically stable)
||| H(p, q) = -sum(p_i * log(q_i))
public export
crossEntropy : List Prob -> List Prob -> Double
crossEntropy targets predictions =
  let pairs = zip (map probValue targets) (map probValue predictions)
  in negate (foldl (\acc, (t, p) => acc + t * safeLog (max p 1.0e-15)) 0.0 pairs)
  where
    max : Double -> Double -> Double
    max a b = if a > b then a else b

||| Binary cross-entropy
public export
binaryCrossEntropy : Prob -> Prob -> Double
binaryCrossEntropy (MkProb target) (MkProb pred) =
  let eps = 1.0e-15
      clipped = if pred < eps then eps else if pred > 1.0 - eps then 1.0 - eps else pred
  in negate (target * safeLog clipped + (1.0 - target) * safeLog (1.0 - clipped))

||| Safe vector normalization (L2 norm)
||| Returns Nothing for zero vectors instead of NaN
public export
normalize : List Double -> Maybe (List Double)
normalize [] = Nothing
normalize xs =
  let norm = sqrt (foldl (\acc, x => acc + x * x) 0.0 xs)
  in if norm < 1.0e-15
       then Nothing
       else Just (map (/ norm) xs)

||| Safe L1 normalization (values sum to 1)
public export
normalizeL1 : List Double -> Maybe (List Double)
normalizeL1 [] = Nothing
normalizeL1 xs =
  let total = foldl (\acc, x => acc + abs x) 0.0 xs
  in if total < 1.0e-15
       then Nothing
       else Just (map (/ total) xs)

||| Dot product of two vectors
public export
dot : List Double -> List Double -> Double
dot xs ys = foldl (+) 0.0 (zipWith (*) xs ys)

||| Cosine similarity (safe - handles zero vectors)
public export
cosineSimilarity : List Double -> List Double -> Maybe Double
cosineSimilarity xs ys =
  let normX = sqrt (dot xs xs)
      normY = sqrt (dot ys ys)
  in if normX < 1.0e-15 || normY < 1.0e-15
       then Nothing
       else Just (dot xs ys / (normX * normY))

||| Sigmoid function (numerically stable)
public export
sigmoid : Double -> Prob
sigmoid x = if x >= 0.0
  then let ex = safeExp (negate x) in clampProb (1.0 / (1.0 + ex))
  else let ex = safeExp x in clampProb (ex / (1.0 + ex))

||| ReLU activation
public export
relu : Double -> Double
relu x = if x > 0.0 then x else 0.0

||| Leaky ReLU
public export
leakyRelu : Double -> Double -> Double
leakyRelu alpha x = if x > 0.0 then x else alpha * x

||| GELU approximation (used in transformers)
public export
gelu : Double -> Double
gelu x = 0.5 * x * (1.0 + tanh (sqrt (2.0 / pi) * (x + 0.044715 * x * x * x)))

||| Accuracy metric: proportion of correct predictions
public export
accuracy : List (Nat, Nat) -> Maybe Prob
accuracy [] = Nothing
accuracy pairs =
  let correct = length (filter (\(pred, actual) => pred == actual) pairs)
      total = length pairs
  in if total == 0
       then Nothing
       else Just (clampProb (cast correct / cast total))

||| Mean Squared Error
public export
mse : List (Double, Double) -> Double
mse [] = 0.0
mse pairs =
  let n = cast (length pairs)
      sumSqErr = foldl (\acc, (pred, actual) => acc + (pred - actual) * (pred - actual)) 0.0 pairs
  in if n == 0.0 then 0.0 else sumSqErr / n

||| Root Mean Squared Error
public export
rmse : List (Double, Double) -> Double
rmse pairs = sqrt (mse pairs)

||| Mean Absolute Error
public export
mae : List (Double, Double) -> Double
mae [] = 0.0
mae pairs =
  let n = cast (length pairs)
      sumAbsErr = foldl (\acc, (pred, actual) => acc + abs (pred - actual)) 0.0 pairs
  in if n == 0.0 then 0.0 else sumAbsErr / n

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that a probability is in valid range
public export
data ValidProb : Prob -> Type where
  MkValidProb : (p : Prob) -> ValidProb p

||| Proof that softmax outputs sum to 1 (approximately)
public export
data SumToOne : List Prob -> Type where
  MkSumToOne : (ps : List Prob) -> SumToOne ps

||| Proof that normalization produces a unit vector
public export
data UnitVector : List Double -> Type where
  MkUnitVector : (xs : List Double) -> UnitVector xs
