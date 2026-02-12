-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeSchema operations (56/75)
module Proven.FFI.SafeSchema

import Proven.SafeSchema
import Proven.Core
import Data.String

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

-- Version comparison
export
proven_idris_schema_version_compare : Int -> Int -> Int -> Int -> Int -> Int -> Int
proven_idris_schema_version_compare maj1 min1 pat1 maj2 min2 pat2 =
  if maj1 < maj2 then (-1)
  else if maj1 > maj2 then 1
  else if min1 < min2 then (-1)
  else if min1 > min2 then 1
  else if pat1 < pat2 then (-1)
  else if pat1 > pat2 then 1
  else 0

-- Compatibility checks
export
proven_idris_schema_backward_compatible : Int -> Int
proven_idris_schema_backward_compatible = id

export
proven_idris_schema_forward_compatible : Int -> Int
proven_idris_schema_forward_compatible = id

export
proven_idris_schema_type_widening_safe : Int -> Int -> Int
proven_idris_schema_type_widening_safe from to =
  encodeBool ((from == 0 && to == 3) ||  -- Int -> Float
              (from == 0 && to == 1) ||  -- Int -> String
              (from == 3 && to == 1) ||  -- Float -> String
              (from == 2 && to == 1) ||  -- Bool -> String
              (from == 2 && to == 0) ||  -- Bool -> Int
              from == to)

export
proven_idris_schema_friendly_error : String -> String
proven_idris_schema_friendly_error msg =
  if isInfixOf "compat" (toLower msg)
    then "Schema compatibility violation"
  else "Schema error"
