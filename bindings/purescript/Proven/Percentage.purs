-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Percentage calculation utilities

module Proven.Percentage
  ( Percentage(..)
  , BasisPoints(..)
  , mkPercentage
  , mkBasisPoints
  , percentOf
  , basisPointsOf
  , toPercentage
  , toBasisPoints
  , toNormalized
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))
import Proven.SafeMath (safeMul, safeDiv)

-- | Percentage value (0-100)
newtype Percentage = Percentage Number

derive instance eqPercentage :: Eq Percentage
derive instance ordPercentage :: Ord Percentage

instance showPercentage :: Show Percentage where
  show (Percentage p) = show p <> "%"

instance semigroupPercentage :: Semigroup Percentage where
  append (Percentage a) (Percentage b) = Percentage (min 100.0 (a + b))

instance monoidPercentage :: Monoid Percentage where
  mempty = Percentage 0.0

-- | Create a percentage (clamped to 0-100)
mkPercentage :: Number -> Percentage
mkPercentage n
  | n < 0.0 = Percentage 0.0
  | n > 100.0 = Percentage 100.0
  | otherwise = Percentage n

-- | Basis points (100 bps = 1%)
newtype BasisPoints = BasisPoints Int

derive instance eqBasisPoints :: Eq BasisPoints
derive instance ordBasisPoints :: Ord BasisPoints

instance showBasisPoints :: Show BasisPoints where
  show (BasisPoints bps) = show bps <> " bps"

instance semigroupBasisPoints :: Semigroup BasisPoints where
  append (BasisPoints a) (BasisPoints b) = BasisPoints (min 10000 (a + b))

instance monoidBasisPoints :: Monoid BasisPoints where
  mempty = BasisPoints 0

-- | Create basis points (clamped to 0-10000)
mkBasisPoints :: Int -> BasisPoints
mkBasisPoints n
  | n < 0 = BasisPoints 0
  | n > 10000 = BasisPoints 10000
  | otherwise = BasisPoints n

-- | Calculate percentage of an amount (0-100 scale)
percentOf :: Number -> Percentage -> Result Number ProvenError
percentOf amount (Percentage pct)
  | pct < 0.0 || pct > 100.0 = Err InvalidPercentage
  | otherwise = do
      product <- safeMul amount pct
      safeDiv product 100.0

-- | Calculate basis points of an amount (100 bps = 1%)
basisPointsOf :: Number -> BasisPoints -> Result Number ProvenError
basisPointsOf amount (BasisPoints bps) = do
  product <- safeMul amount (toNumber bps)
  safeDiv product 10000.0
  where
    toNumber :: Int -> Number
    toNumber = intToNumber

foreign import intToNumber :: Int -> Number

-- | Convert percentage to basis points
toBasisPoints :: Percentage -> BasisPoints
toBasisPoints (Percentage p) = BasisPoints (floor (p * 100.0))
  where
    floor :: Number -> Int
    floor = floorImpl

foreign import floorImpl :: Number -> Int

-- | Convert basis points to percentage
toPercentage :: BasisPoints -> Percentage
toPercentage (BasisPoints bps) = Percentage (toNumber bps / 100.0)
  where
    toNumber :: Int -> Number
    toNumber = intToNumber

-- | Convert percentage to normalized value (0-1)
toNormalized :: Percentage -> Number
toNormalized (Percentage p) = p / 100.0
