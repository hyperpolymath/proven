-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafePath - FFI bindings to libproven path validation
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafePath
  ( hasTraversal
  , sanitizeFilename
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Check if path contains traversal patterns (delegates to Idris 2).
-- | Detects "..", null bytes, and other attack patterns.
foreign import hasTraversalImpl :: String -> Boolean

hasTraversal :: String -> Boolean
hasTraversal = hasTraversalImpl

-- | Sanitize a filename by removing dangerous characters (delegates to Idris 2).
foreign import sanitizeFilenameImpl :: String -> String

sanitizeFilename :: String -> String
sanitizeFilename = sanitizeFilenameImpl
