-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeCrypto - FFI bindings to libproven cryptographic operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeCrypto
  ( constantTimeCompare
  , randomBytes
  ) where

import Prelude

-- | Constant-time comparison to prevent timing attacks (delegates to Idris 2).
-- | Returns true only if both strings are equal and have the same length.
foreign import constantTimeCompareImpl :: String -> String -> Boolean

constantTimeCompare :: String -> String -> Boolean
constantTimeCompare = constantTimeCompareImpl

-- | Generate cryptographically secure random bytes (delegates to Idris 2).
-- | Returns an array of random values 0-255.
foreign import randomBytesImpl :: Int -> Array Int

randomBytes :: Int -> Array Int
randomBytes = randomBytesImpl
