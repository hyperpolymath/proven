-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeJson operations (Idris-only logic)
module Proven.FFI.SafeJson

import Proven.SafeJson
import Proven.SafeJson.Parser
import Data.String

%default total

jsonTypeToInt : JsonValue -> Int
jsonTypeToInt JsonNull = 0
jsonTypeToInt (JsonBool _) = 1
jsonTypeToInt (JsonNumber _) = 2
jsonTypeToInt (JsonString _) = 3
jsonTypeToInt (JsonArray _) = 4
jsonTypeToInt (JsonObject _) = 5

export
proven_idris_json_is_valid : String -> Bool
proven_idris_json_is_valid s = isValidJson s

export
proven_idris_json_get_type : String -> Int
proven_idris_json_get_type s = case parseJson s of
  Just v => jsonTypeToInt v
  Nothing => -1
