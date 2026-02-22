-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeVersion - FFI bindings to libproven semantic version operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeVersion
  ( versionParse
  , versionCompare
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Parse a semantic version string (delegates to Idris 2).
-- | Returns a version handle that can be used for comparison.
foreign import versionParseImpl :: String -> { status :: Int, version :: Foreign }

foreign import data Foreign :: Type

versionParse :: String -> Result Foreign ProvenError
versionParse s =
  let r = versionParseImpl s
  in if r.status == 0 then Ok r.version else Err InvalidVersion

-- | Compare two semantic versions (delegates to Idris 2).
-- | Returns -1, 0, or 1.
foreign import versionCompareImpl :: Foreign -> Foreign -> Int

versionCompare :: Foreign -> Foreign -> Int
versionCompare = versionCompareImpl
