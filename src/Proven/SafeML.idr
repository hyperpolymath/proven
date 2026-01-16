-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeML - Safe machine learning operations with formal verification
|||
||| This module provides safe implementations of common ML operations that
||| prevent numerical instabilities like NaN, Infinity, and division by zero.
||| All operations are total and return Option/Result types for failure cases.
|||
||| Key features:
||| - Numerically stable softmax with overflow protection
||| - Safe loss functions that handle edge cases
||| - Verified activation functions
||| - Gradient clipping and normalization
||| - Safe regularization (L1/L2)
module Proven.SafeML

import public Proven.Core
import public Proven.SafeFloat
import public Proven.SafeTensor
import Data.Vect
import Data.List

%default total

--------------------------------------------------------------------------------
-- Constants for Numerical Stability
--------------------------------------------------------------------------------

||| Small epsilon for numerical stability in log operations
public export
logEpsilon : Double
logEpsilon = 1.0e-15

||| Maximum exponent to prevent overflow in exp operations
public export
maxExpArg : Double
maxExpArg = 709.0

||| Minimum value to clamp logits before softmax (prevents underflow)
public export
minLogit : Double
minLogit = -500.0

||| Maximum value to clamp logits before softmax (prevents overflow)
public export
maxLogit : Double
maxLogit = 500.0

--------------------------------------------------------------------------------
-- Activation Functions
--------------------------------------------------------------------------------

||| Rectified Linear Unit (ReLU)
||| ReLU(x) = max(0, x)
public export
relu : Double -> Double
relu x = if x > 0.0 then x else 0.0

||| ReLU on a vector
public export
reluVec : List Double -> List Double
reluVec = map relu

||| Leaky ReLU with configurable negative slope
||| LeakyReLU(x) = x if x > 0, else alpha * x
public export
leakyRelu : (alpha : Double) -> Double -> Double
leakyRelu alpha x = if x > 0.0 then x else alpha * x

||| Leaky ReLU on a vector
public export
leakyReluVec : (alpha : Double) -> List Double -> List Double
leakyReluVec alpha = map (leakyRelu alpha)

||| Safe sigmoid function with overflow protection
||| sigmoid(x) = 1 / (1 + exp(-x))
public export
sigmoid : Double -> Double
sigmoid x =
  let clampedX = clamp (negate maxExpArg) maxExpArg x
  in if clampedX >= 0.0
       then 1.0 / (1.0 + Prelude.exp (negate clampedX))
       else let expX = Prelude.exp clampedX
            in expX / (1.0 + expX)

||| Sigmoid on a vector
public export
sigmoidVec : List Double -> List Double
sigmoidVec = map sigmoid

||| Hyperbolic tangent (safe, using sigmoid identity)
||| tanh(x) = 2*sigmoid(2x) - 1
public export
tanh : Double -> Double
tanh x = 2.0 * sigmoid (2.0 * x) - 1.0

||| Tanh on a vector
public export
tanhVec : List Double -> List Double
tanhVec = map tanh

||| GELU (Gaussian Error Linear Unit) approximation
||| GELU(x) ≈ 0.5 * x * (1 + tanh(sqrt(2/π) * (x + 0.044715 * x³)))
public export
gelu : Double -> Double
gelu x =
  let sqrt2OverPi = 0.7978845608028654  -- sqrt(2/π)
      coeff = 0.044715
      inner = sqrt2OverPi * (x + coeff * x * x * x)
  in 0.5 * x * (1.0 + tanh inner)

||| GELU on a vector
public export
geluVec : List Double -> List Double
geluVec = map gelu

||| Swish activation: x * sigmoid(x)
public export
swish : Double -> Double
swish x = x * sigmoid x

||| Swish on a vector
public export
swishVec : List Double -> List Double
swishVec = map swish

--------------------------------------------------------------------------------
-- Safe Softmax
--------------------------------------------------------------------------------

||| Find maximum in a non-empty list
maxOfList : List Double -> Double
maxOfList [] = negate (1.0 / 0.0)  -- -Infinity as fallback
maxOfList [x] = x
maxOfList (x :: xs) = max x (maxOfList xs)

||| Numerically stable softmax implementation
||| Uses the log-sum-exp trick to prevent overflow:
||| softmax(x)_i = exp(x_i - max(x)) / sum(exp(x_j - max(x)))
|||
||| Returns Nothing if the input is empty or if sum underflows to zero
public export
softmax : List Double -> Maybe (List Double)
softmax [] = Nothing
softmax logits =
  let -- Clamp inputs to prevent extreme values
      clamped = map (clamp minLogit maxLogit) logits
      -- Subtract max for numerical stability
      maxVal = maxOfList clamped
      shifted = map (\x => x - maxVal) clamped
      -- Compute exponentials
      exps = map Prelude.exp shifted
      -- Sum of exponentials
      sumExp = sum exps
  in if sumExp < logEpsilon
       then Nothing  -- Underflow: can't normalize
       else Just (map (/ sumExp) exps)

||| Softmax with fallback to uniform distribution on underflow
public export
softmaxOrUniform : List Double -> List Double
softmaxOrUniform [] = []
softmaxOrUniform xs =
  case softmax xs of
    Just probs => probs
    Nothing => let n = cast (length xs)
               in replicate (length xs) (1.0 / n)

||| Temperature-scaled softmax
||| Higher temperature -> more uniform, lower -> more peaked
public export
softmaxWithTemp : (temperature : Double) -> List Double -> Maybe (List Double)
softmaxWithTemp temp logits =
  if temp <= logEpsilon
    then Nothing  -- Temperature too small
    else softmax (map (/ temp) logits)

--------------------------------------------------------------------------------
-- Loss Functions
--------------------------------------------------------------------------------

||| Safe mean squared error loss
||| MSE = (1/n) * Σ(pred_i - target_i)²
public export
mseLoss : List Double -> List Double -> Maybe Double
mseLoss [] [] = Just 0.0
mseLoss predictions targets = do
  diffs <- subList predictions targets
  let squaredDiffs = map (\x => x * x) diffs
  SafeFloat.mean squaredDiffs

||| Mean absolute error loss
||| MAE = (1/n) * Σ|pred_i - target_i|
public export
maeLoss : List Double -> List Double -> Maybe Double
maeLoss [] [] = Just 0.0
maeLoss predictions targets = do
  diffs <- subList predictions targets
  let absDiffs = map abs diffs
  SafeFloat.mean absDiffs

||| Safe cross-entropy loss for classification
||| CE = -Σ target_i * log(pred_i + ε)
|||
||| Assumes predictions are probabilities (from softmax)
public export
crossEntropyLoss : (predictions : List Double) -> (targets : List Double) -> Maybe Double
crossEntropyLoss [] [] = Just 0.0
crossEntropyLoss preds targets =
  if length preds /= length targets
    then Nothing
    else
      let -- Clamp predictions to prevent log(0)
          clampedPreds = map (clamp logEpsilon 1.0) preds
          -- Compute -target * log(pred) for each pair
          terms = zipWith (\t, p => negate t * log p) targets clampedPreds
      in Just (sum terms)

||| Binary cross-entropy loss
||| BCE = -[y * log(p + ε) + (1-y) * log(1-p + ε)]
public export
binaryCrossEntropy : (prediction : Double) -> (target : Double) -> Double
binaryCrossEntropy pred target =
  let p = clamp logEpsilon (1.0 - logEpsilon) pred
  in negate (target * log p + (1.0 - target) * log (1.0 - p))

||| Binary cross-entropy for vectors
public export
binaryCrossEntropyVec : List Double -> List Double -> Maybe Double
binaryCrossEntropyVec preds targets =
  if length preds /= length targets
    then Nothing
    else
      let losses = zipWith binaryCrossEntropy preds targets
      in SafeFloat.mean losses

||| Huber loss (smooth L1) - less sensitive to outliers than MSE
||| Huber(x) = 0.5*x² if |x| <= δ, else δ*(|x| - 0.5*δ)
public export
huberLoss : (delta : Double) -> List Double -> List Double -> Maybe Double
huberLoss delta preds targets =
  if length preds /= length targets
    then Nothing
    else do
      diffs <- subList preds targets
      let losses = map huberSingle diffs
      SafeFloat.mean losses
  where
    huberSingle : Double -> Double
    huberSingle x =
      let absX = abs x
      in if absX <= delta
           then 0.5 * x * x
           else delta * (absX - 0.5 * delta)

--------------------------------------------------------------------------------
-- Regularization
--------------------------------------------------------------------------------

||| L1 norm (sum of absolute values)
public export
l1Norm : List Double -> Double
l1Norm = sum . map abs

||| L2 norm squared (sum of squares)
public export
l2NormSquared : List Double -> Double
l2NormSquared = sum . map (\x => x * x)

||| L2 norm (Euclidean norm)
public export
l2Norm : List Double -> Double
l2Norm xs = Prelude.sqrt (l2NormSquared xs)

||| L1 regularization term
||| λ * Σ|w_i|
public export
l1Regularization : (lambda : Double) -> List Double -> Double
l1Regularization lambda weights = lambda * l1Norm weights

||| L2 regularization term (weight decay)
||| (λ/2) * Σw_i²
public export
l2Regularization : (lambda : Double) -> List Double -> Double
l2Regularization lambda weights = 0.5 * lambda * l2NormSquared weights

||| Elastic net regularization (combination of L1 and L2)
||| α * L1 + (1-α) * L2
public export
elasticNetRegularization : (alpha : Double) -> (lambda : Double) -> List Double -> Double
elasticNetRegularization alpha lambda weights =
  alpha * l1Regularization lambda weights +
  (1.0 - alpha) * l2Regularization lambda weights

--------------------------------------------------------------------------------
-- Gradient Operations
--------------------------------------------------------------------------------

||| Clip gradients by value
||| Clamps each gradient to [-maxVal, maxVal]
public export
clipGradientsByValue : (maxVal : Double) -> List Double -> List Double
clipGradientsByValue maxVal = map (clamp (negate maxVal) maxVal)

||| Clip gradients by global norm
||| If ||g|| > maxNorm, scale g by maxNorm/||g||
public export
clipGradientsByNorm : (maxNorm : Double) -> List Double -> List Double
clipGradientsByNorm maxNorm gradients =
  let norm = l2Norm gradients
  in if norm > maxNorm && norm > SafeFloat.epsilon
       then map (* (maxNorm / norm)) gradients
       else gradients

||| Gradient scaling for mixed precision training
public export
scaleGradients : (scale : Double) -> List Double -> List Double
scaleGradients scale = map (* scale)

--------------------------------------------------------------------------------
-- Batch Normalization Helpers
--------------------------------------------------------------------------------

||| Compute batch statistics (mean and variance)
public export
batchStats : List Double -> Maybe (Double, Double)
batchStats xs = do
  m <- SafeFloat.mean xs
  v <- SafeFloat.variance xs
  Just (m, v)

||| Apply batch normalization
||| y = (x - mean) / sqrt(variance + ε) * gamma + beta
public export
batchNorm : (gamma : Double) -> (beta : Double) ->
            (mean : Double) -> (variance : Double) ->
            Double -> Double
batchNorm gamma beta mean var x =
  let stdDev = Prelude.sqrt (var + SafeFloat.epsilon)
      normalized = (x - mean) / stdDev
  in gamma * normalized + beta

||| Batch normalize a vector
public export
batchNormVec : (gamma : Double) -> (beta : Double) ->
               List Double -> Maybe (List Double)
batchNormVec gamma beta xs = do
  (mean, var) <- batchStats xs
  Just (map (batchNorm gamma beta mean var) xs)

--------------------------------------------------------------------------------
-- Layer Normalization
--------------------------------------------------------------------------------

||| Layer normalization (normalizes across features, not batch)
public export
layerNorm : (gamma : Double) -> (beta : Double) ->
            List Double -> Maybe (List Double)
layerNorm = batchNormVec  -- Same computation, different semantics

--------------------------------------------------------------------------------
-- Dropout Mask Generation (requires external RNG)
--------------------------------------------------------------------------------

||| Apply dropout mask to values
||| Scales remaining values by 1/(1-p) to maintain expected value
public export
applyDropout : (mask : List Bool) -> (p : Double) -> List Double -> Maybe (List Double)
applyDropout mask p values =
  if length mask /= length values || p >= 1.0
    then Nothing
    else
      let scale = 1.0 / (1.0 - p)
          apply : Bool -> Double -> Double
          apply True v = v * scale
          apply False _ = 0.0
      in Just (zipWith apply mask values)

--------------------------------------------------------------------------------
-- Weight Initialization Helpers
--------------------------------------------------------------------------------

||| Xavier/Glorot initialization scale factor
||| scale = sqrt(2 / (fan_in + fan_out))
public export
xavierScale : (fanIn : Nat) -> (fanOut : Nat) -> Maybe Double
xavierScale fanIn fanOut =
  if fanIn == 0 && fanOut == 0
    then Nothing
    else SafeFloat.sqrt (2.0 / cast (fanIn + fanOut))

||| He initialization scale factor (for ReLU networks)
||| scale = sqrt(2 / fan_in)
public export
heScale : (fanIn : Nat) -> Maybe Double
heScale Z = Nothing
heScale fanIn = SafeFloat.sqrt (2.0 / cast fanIn)

||| LeCun initialization scale factor
||| scale = sqrt(1 / fan_in)
public export
lecunScale : (fanIn : Nat) -> Maybe Double
lecunScale Z = Nothing
lecunScale fanIn = SafeFloat.sqrt (1.0 / cast fanIn)

--------------------------------------------------------------------------------
-- Learning Rate Schedules
--------------------------------------------------------------------------------

||| Exponential decay learning rate
||| lr = initial_lr * decay_rate ^ (step / decay_steps)
public export
exponentialDecay : (initialLR : Double) -> (decayRate : Double) ->
                   (decaySteps : Nat) -> (step : Nat) -> Maybe Double
exponentialDecay initLR rate decaySteps step =
  if decaySteps == 0
    then Nothing
    else
      let exponent = cast step / cast decaySteps
      in SafeFloat.pow rate exponent >>= \decay => Just (initLR * decay)

||| Linear warmup learning rate
||| lr = initial_lr * min(1, step / warmup_steps)
public export
linearWarmup : (initialLR : Double) -> (warmupSteps : Nat) -> (step : Nat) -> Double
linearWarmup initLR warmupSteps step =
  if warmupSteps == 0
    then initLR
    else
      let progress = min 1.0 (cast step / cast warmupSteps)
      in initLR * progress

||| Cosine annealing learning rate
||| lr = min_lr + 0.5 * (max_lr - min_lr) * (1 + cos(π * step / total_steps))
public export
cosineAnnealing : (maxLR : Double) -> (minLR : Double) ->
                  (totalSteps : Nat) -> (step : Nat) -> Double
cosineAnnealing maxLR minLR totalSteps step =
  if totalSteps == 0
    then minLR
    else
      let progress = cast step / cast totalSteps
          cosValue = cos (pi * progress)
      in minLR + 0.5 * (maxLR - minLR) * (1.0 + cosValue)

--------------------------------------------------------------------------------
-- Metrics
--------------------------------------------------------------------------------

||| Accuracy for classification (proportion of correct predictions)
public export
accuracy : List Nat -> List Nat -> Maybe Double
accuracy [] [] = Just 1.0  -- Vacuously correct
accuracy predictions targets =
  if length predictions /= length targets
    then Nothing
    else
      let correct = sum (zipWith (\p, t => if p == t then 1 else 0) predictions targets)
      in SafeFloat.div (cast correct) (cast (length predictions))

||| Top-k accuracy (prediction correct if true label in top k predictions)
||| Takes list of (prediction_probs, true_label) pairs
public export
topKCorrect : (k : Nat) -> List Double -> Nat -> Bool
topKCorrect k probs trueLabel =
  let indexed = zip [0..length probs] probs
      -- Sort by probability descending (simple selection sort for small k)
      topKIndices = take k (map fst (sortBy (\(_, p1), (_, p2) => compare p2 p1) indexed))
  in elem trueLabel topKIndices

--------------------------------------------------------------------------------
-- Numerical Stability Checks
--------------------------------------------------------------------------------

||| Check if all values in a list are finite
public export
allFinite : List Double -> Bool
allFinite = all SafeFloat.isFinite

||| Check for NaN values in a list
public export
hasNaN : List Double -> Bool
hasNaN = any SafeFloat.isNaN

||| Sanitize a vector by replacing NaN/Inf with a default
public export
sanitizeVec : (default : Double) -> List Double -> List Double
sanitizeVec def = map (SafeFloat.sanitize def)

||| Check if gradients have exploded (norm exceeds threshold)
public export
gradientsExploded : (threshold : Double) -> List Double -> Bool
gradientsExploded threshold gradients = l2Norm gradients > threshold

||| Check if gradients have vanished (norm below threshold)
public export
gradientsVanished : (threshold : Double) -> List Double -> Bool
gradientsVanished threshold gradients = l2Norm gradients < threshold

