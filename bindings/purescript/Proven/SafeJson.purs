-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeJson - FFI bindings to libproven JSON validation
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeJson
  ( isValidJson
  , jsonTypeOf
  ) where

import Prelude

-- | Check if a string is valid JSON (delegates to Idris 2).
foreign import isValidJsonImpl :: String -> Boolean

isValidJson :: String -> Boolean
isValidJson = isValidJsonImpl

-- | Get the JSON type of a valid JSON string (delegates to Idris 2).
-- | Returns an integer code: 0=null, 1=boolean, 2=number, 3=string, 4=array, 5=object, -1=invalid.
foreign import jsonTypeOfImpl :: String -> Int

jsonTypeOf :: String -> Int
jsonTypeOf = jsonTypeOfImpl
