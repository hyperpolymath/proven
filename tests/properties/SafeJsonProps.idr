-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeJsonProps

import Proven.Core
import Proven.SafeJson

%default total

||| Property: Parsing valid JSON returns Ok
prop_validJsonParses : parseJson "{}" = Ok (JsonObject [])
prop_validJsonParses = Refl

||| Property: Parsing empty string fails
prop_emptyStringFails : isErr (parseJson "") = True
prop_emptyStringFails = Refl

||| Property: JSON null is a valid value
prop_nullIsValid : parseJson "null" = Ok JsonNull
prop_nullIsValid = Refl

||| Property: JSON boolean true parses correctly
prop_trueParses : parseJson "true" = Ok (JsonBool True)
prop_trueParses = Refl

||| Property: JSON boolean false parses correctly
prop_falseParses : parseJson "false" = Ok (JsonBool False)
prop_falseParses = Refl

||| Property: Getting from empty object returns Nothing
prop_emptyObjectGet : get "key" (JsonObject []) = Nothing
prop_emptyObjectGet = Refl

||| Property: Array index out of bounds returns Nothing
prop_arrayOutOfBounds : getIndex 10 (JsonArray []) = Nothing
prop_arrayOutOfBounds = Refl

||| Property: Stringify then parse roundtrip for simple values
prop_roundTripNull : parseJson (stringify JsonNull) = Ok JsonNull
prop_roundTripNull = ?prop_roundTripNull_rhs

||| Test runner for JSON properties
export
runJsonProperties : IO ()
runJsonProperties = do
  putStrLn "SafeJson Property Tests"
  putStrLn "======================="
  putStrLn "prop_validJsonParses: PASS (proven by type)"
  putStrLn "prop_emptyStringFails: PASS (proven by type)"
  putStrLn "prop_nullIsValid: PASS (proven by type)"
  putStrLn "prop_trueParses: PASS (proven by type)"
  putStrLn "prop_falseParses: PASS (proven by type)"
  putStrLn "prop_emptyObjectGet: PASS (proven by type)"
  putStrLn "prop_arrayOutOfBounds: PASS (proven by type)"
  putStrLn ""
