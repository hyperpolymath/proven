-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Bounded value types

module Proven.Bounded
  ( Bounded
  , BoundedInt(..)
  , BoundedNumber(..)
  , mkBoundedInt
  , mkBoundedNumber
  , getValue
  , getBounds
  , inRange
  , requireInRange
  , increment
  , decrement
  , isAtMin
  , isAtMax
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))
import Proven.SafeMath (clamp, safeAdd, safeSub)
import Data.Int (toNumber, floor)

-- | Bounded type class
class Bounded a where
  minBound :: a -> a
  maxBound :: a -> a

-- | Bounded integer with min/max constraints
newtype BoundedInt = BoundedInt
  { value :: Int
  , min :: Int
  , max :: Int
  }

derive instance eqBoundedInt :: Eq BoundedInt

instance showBoundedInt :: Show BoundedInt where
  show (BoundedInt r) = "BoundedInt { value: " <> show r.value
    <> ", min: " <> show r.min
    <> ", max: " <> show r.max <> " }"

-- | Create a bounded integer (clamped to range)
mkBoundedInt :: Int -> Int -> Int -> BoundedInt
mkBoundedInt value minVal maxVal = BoundedInt
  { value: clampInt minVal maxVal value
  , min: minVal
  , max: maxVal
  }
  where
    clampInt :: Int -> Int -> Int -> Int
    clampInt mi ma v
      | v < mi = mi
      | v > ma = ma
      | otherwise = v

-- | Bounded number with min/max constraints
newtype BoundedNumber = BoundedNumber
  { value :: Number
  , min :: Number
  , max :: Number
  }

derive instance eqBoundedNumber :: Eq BoundedNumber

instance showBoundedNumber :: Show BoundedNumber where
  show (BoundedNumber r) = "BoundedNumber { value: " <> show r.value
    <> ", min: " <> show r.min
    <> ", max: " <> show r.max <> " }"

-- | Create a bounded number (clamped to range)
mkBoundedNumber :: Number -> Number -> Number -> BoundedNumber
mkBoundedNumber value minVal maxVal = BoundedNumber
  { value: clamp minVal maxVal value
  , min: minVal
  , max: maxVal
  }

-- | Get the value from a bounded integer
getValue :: BoundedInt -> Int
getValue (BoundedInt r) = r.value

-- | Get the bounds from a bounded integer
getBounds :: BoundedInt -> { min :: Int, max :: Int }
getBounds (BoundedInt r) = { min: r.min, max: r.max }

-- | Check if value is in range
inRange :: forall a. Ord a => a -> a -> a -> Boolean
inRange minVal maxVal value = value >= minVal && value <= maxVal

-- | Require value in range or return error
requireInRange :: forall a. Ord a => a -> a -> a -> Result a ProvenError
requireInRange minVal maxVal value
  | inRange minVal maxVal value = Ok value
  | otherwise = Err OutOfBounds

-- | Increment bounded integer with clamping
increment :: Int -> BoundedInt -> Result BoundedInt ProvenError
increment delta (BoundedInt r) =
  case safeAdd (toNumber r.value) (toNumber delta) of
    Ok result ->
      let newVal = clampInt r.min r.max (floor result)
      in Ok (BoundedInt r { value = newVal })
    Err e -> Err e
  where
    clampInt :: Int -> Int -> Int -> Int
    clampInt mi ma v
      | v < mi = mi
      | v > ma = ma
      | otherwise = v

-- | Decrement bounded integer with clamping
decrement :: Int -> BoundedInt -> Result BoundedInt ProvenError
decrement delta (BoundedInt r) =
  case safeSub (toNumber r.value) (toNumber delta) of
    Ok result ->
      let newVal = clampInt r.min r.max (floor result)
      in Ok (BoundedInt r { value = newVal })
    Err e -> Err e
  where
    clampInt :: Int -> Int -> Int -> Int
    clampInt mi ma v
      | v < mi = mi
      | v > ma = ma
      | otherwise = v

-- | Check if bounded integer is at minimum
isAtMin :: BoundedInt -> Boolean
isAtMin (BoundedInt r) = r.value <= r.min

-- | Check if bounded integer is at maximum
isAtMax :: BoundedInt -> Boolean
isAtMax (BoundedInt r) = r.value >= r.max
