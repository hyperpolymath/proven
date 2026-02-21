{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe mathematical operations with overflow detection.
module Proven.SafeMath
  ( safeDiv
  , safeMod
  , safeAdd
  , safeSub
  , safeMul
  ) where

import Data.Int (Int64)

-- | Safely divide two integers, returning Nothing on division by zero.
safeDiv :: Int64 -> Int64 -> Maybe Int64
safeDiv _ 0 = Nothing
safeDiv a b = Just (a `div` b)

-- | Safely compute modulo, returning Nothing on division by zero.
safeMod :: Int64 -> Int64 -> Maybe Int64
safeMod _ 0 = Nothing
safeMod a b = Just (a `mod` b)

-- | Safely add two integers with overflow detection.
safeAdd :: Int64 -> Int64 -> Maybe Int64
safeAdd a b
  | b > 0 && a > maxBound - b = Nothing
  | b < 0 && a < minBound - b = Nothing
  | otherwise = Just (a + b)

-- | Safely subtract two integers with overflow detection.
safeSub :: Int64 -> Int64 -> Maybe Int64
safeSub a b
  | b < 0 && a > maxBound + b = Nothing
  | b > 0 && a < minBound + b = Nothing
  | otherwise = Just (a - b)

-- | Safely multiply two integers with overflow detection.
safeMul :: Int64 -> Int64 -> Maybe Int64
safeMul 0 _ = Just 0
safeMul _ 0 = Just 0
safeMul a b
  | a > 0 && b > 0 && a > maxBound `div` b = Nothing
  | a > 0 && b < 0 && b < minBound `div` a = Nothing
  | a < 0 && b > 0 && a < minBound `div` b = Nothing
  | a < 0 && b < 0 && b < maxBound `div` a = Nothing
  | otherwise = Just (a * b)
