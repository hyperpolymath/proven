{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe filesystem path operations via libproven FFI.
--
-- Prevents directory traversal attacks. All checking is performed
-- by the Idris 2 verified core.
module Proven.SafePath
  ( hasTraversal
  , sanitizeFilename
  ) where

import Proven.FFI (c_proven_path_has_traversal, c_proven_path_sanitize_filename,
                   c_proven_free_string)
import Proven.Core (withCStringLen', boolResultToBool, stringResultToMaybe)

-- | Check if a path contains directory traversal sequences.
-- Delegates to @proven_path_has_traversal@ in libproven.
hasTraversal :: String -> IO (Maybe Bool)
hasTraversal path = withCStringLen' path $ \ptr len ->
  boolResultToBool <$> c_proven_path_has_traversal ptr len

-- | Sanitize a filename by removing dangerous characters.
-- Delegates to @proven_path_sanitize_filename@ in libproven.
sanitizeFilename :: String -> IO (Maybe String)
sanitizeFilename name = withCStringLen' name $ \ptr len -> do
  sr <- c_proven_path_sanitize_filename ptr len
  stringResultToMaybe c_proven_free_string sr
