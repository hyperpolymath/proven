-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | Validation - Validation utilities using libproven FFI
-- |
-- | Delegates all validation to the FFI-backed Safe* modules which
-- | delegate to Idris 2 via the Zig FFI layer.

module Proven.Validation
  ( isValidEmail
  , isValidPath
  , requireValidEmail
  , requireSafePath
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))
import Proven.SafeEmail (isValidEmail) as SafeEmail
import Proven.SafePath (hasTraversal) as SafePath

-- | Validate email format (delegates to Idris 2 via SafeEmail FFI).
isValidEmail :: String -> Boolean
isValidEmail = SafeEmail.isValidEmail

-- | Check if path is safe (delegates to Idris 2 via SafePath FFI).
isValidPath :: String -> Boolean
isValidPath path = not (SafePath.hasTraversal path)

-- | Require valid email or return error (delegates to Idris 2 via SafeEmail FFI).
requireValidEmail :: String -> Result String ProvenError
requireValidEmail email
  | isValidEmail email = Ok email
  | otherwise = Err InvalidEmail

-- | Require safe path or return error (delegates to Idris 2 via SafePath FFI).
requireSafePath :: String -> Result String ProvenError
requireSafePath path
  | isValidPath path = Ok path
  | otherwise = Err PathTraversal
