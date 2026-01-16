-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Proven Safety Library for PureScript
-- |
-- | Formally verified safety primitives for functional web development.
-- | Provides type-safe, pure functional patterns for PureScript applications.
-- |
-- | Version: 0.9.0

module Proven
  ( module Proven.Result
  , module Proven.SafeMath
  , module Proven.Bounded
  , module Proven.Validation
  , module Proven.Percentage
  , version
  ) where

import Prelude

import Proven.Result (Result(..), ok, err, isOk, isErr, unwrap, unwrapOr, mapResult, flatMapResult)
import Proven.SafeMath (SafeMath, safeAdd, safeSub, safeMul, safeDiv, safeMod, clamp)
import Proven.Bounded (Bounded, BoundedInt, mkBoundedInt, getValue, getBounds, inRange, requireInRange)
import Proven.Validation (Validation, isValidPort, isValidPercentage, isValidEmail, requireValidPort, requireValidPercentage)
import Proven.Percentage (percentOf, basisPointsOf)

-- | Library version
version :: String
version = "0.9.0"
