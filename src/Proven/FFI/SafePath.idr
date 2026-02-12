-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafePath operations (Idris-only logic)
module Proven.FFI.SafePath

import Proven.SafePath.Operations
import Data.List
import Data.String

%default total

export
proven_idris_path_has_traversal : String -> Bool
proven_idris_path_has_traversal s =
  elem ".." (splitPath s)

export
proven_idris_path_sanitize_filename : String -> String
proven_idris_path_sanitize_filename s =
  let cleaned = filter isSafe (unpack s)
  in if null cleaned then "" else pack cleaned
  where
    isSafe : Char -> Bool
    isSafe c = isAlphaNum c || c == '.' || c == '-' || c == '_'
