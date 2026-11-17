-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeJsonProps

import Proven.Core
import Proven.SafeJson
import System

%default total

-- The real API: `parseJson : String -> Maybe JsonValue` (literal fast paths
-- for null/true/false/""/[]/{}, otherwise the full `parse` state machine),
-- `parse : String -> Either ParseError JsonValue`, `get` for object lookup,
-- `at` for array indexing. There is no `stringify`; `show` renders JSON.

||| Property: parsing "{}" yields the empty object (holds via parseJson's
||| literal dispatch; the full parser is exercised at runtime below).
prop_validJsonParses : parseJson "{}" = Just (JsonObject [])
prop_validJsonParses = Refl

||| Property: parsing the empty string fails
prop_emptyStringFails : parseJson "" = Nothing
prop_emptyStringFails = Refl

||| Property: JSON null parses
prop_nullIsValid : parseJson "null" = Just JsonNull
prop_nullIsValid = Refl

||| Property: JSON true parses
prop_trueParses : parseJson "true" = Just (JsonBool True)
prop_trueParses = Refl

||| Property: JSON false parses
prop_falseParses : parseJson "false" = Just (JsonBool False)
prop_falseParses = Refl

||| Property: getting any key from the empty object returns Nothing
prop_emptyObjectGet : (k : String) -> get k (JsonObject []) = Nothing
prop_emptyObjectGet k = Refl

||| Property: any index into the empty array returns Nothing
prop_arrayOutOfBounds : (n : Nat) -> at n (JsonArray []) = Nothing
prop_arrayOutOfBounds n = Refl

||| OWED: render/parse round trip for JsonNull, stated against the full
||| parser (`parse`, not `parseJson`'s literal fast path) on "null", the
||| rendering both `Show JsonValue` instances produce for JsonNull.
||| Blocked by: `parse` runs the module-private, fuel-indexed `parseValue`
||| over `unpack s`, which does not reduce at the type level. A general
||| `parse (render v) = Right v` cannot even be stated against the library:
||| there is no `stringify`, and `show` is ambiguous (two `Show JsonValue`
||| instances, in Proven.SafeJson.Parser and Proven.SafeJson). This names the
||| proposition; it is not a witness.
public export
NullRoundTripObligation : Type
NullRoundTripObligation = parse "null" = Right JsonNull

||| OWED: the `parseJson` literal fast paths agree with the full parser.
||| Blocked by: same non-reduction of `parse` as above. This names the
||| proposition; it is not a witness.
public export
FastPathAgreesObligation : Type
FastPathAgreesObligation =
  (s : String) -> (v : JsonValue) -> parseJson s = Just v -> parse s = Right v

-- Total structural equality (the library's `Eq JsonValue` is only `covering`).
mutual
  private
  jsonEq : JsonValue -> JsonValue -> Bool
  jsonEq JsonNull JsonNull = True
  jsonEq (JsonBool a) (JsonBool b) = a == b
  jsonEq (JsonNumber a) (JsonNumber b) = a == b
  jsonEq (JsonString a) (JsonString b) = a == b
  jsonEq (JsonArray a) (JsonArray b) = listEq a b
  jsonEq (JsonObject a) (JsonObject b) = pairsEq a b
  jsonEq _ _ = False

  private
  listEq : List JsonValue -> List JsonValue -> Bool
  listEq [] [] = True
  listEq (x :: xs) (y :: ys) = jsonEq x y && listEq xs ys
  listEq _ _ = False

  private
  pairsEq : List (String, JsonValue) -> List (String, JsonValue) -> Bool
  pairsEq [] [] = True
  pairsEq ((k, x) :: xs) ((l, y) :: ys) = k == l && jsonEq x y && pairsEq xs ys
  pairsEq _ _ = False

private
maybeEq : Maybe JsonValue -> Maybe JsonValue -> Bool
maybeEq Nothing Nothing = True
maybeEq (Just a) (Just b) = jsonEq a b
maybeEq _ _ = False

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
parsesTo : String -> JsonValue -> Bool
parsesTo s v = case parse s of
  Right v' => jsonEq v' v
  Left _ => False

private
rejects : String -> Bool
rejects s = case parse s of
  Right _ => False
  Left _ => True

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
  putStrLn "prop_emptyObjectGet: PASS (proven by type, all keys)"
  putStrLn "prop_arrayOutOfBounds: PASS (proven by type, all indices)"
  -- The literal fast paths above bypass the real parser; check it directly.
  check "full parser: {} / [] / null / true / false"
    (parsesTo "{}" (JsonObject []) && parsesTo "[]" (JsonArray []) &&
     parsesTo "null" JsonNull && parsesTo "true" (JsonBool True) &&
     parsesTo "false" (JsonBool False))
  check "full parser: empty and whitespace-only input rejected"
    (rejects "" && rejects "   ")
  check "full parser: whitespace-padded and nested values"
    (parsesTo " { \"a\" : [1, true, null] } "
       (JsonObject [("a", JsonArray [JsonNumber 1.0, JsonBool True, JsonNull])]))
  check "full parser: trailing content and malformed input rejected"
    (rejects "{} x" && rejects "{\"a\" 1}" && rejects "tru" && rejects "[1 2]")
  check "get finds a present key and misses an absent one"
    (maybeEq (get "k" (JsonObject [("k", JsonNull)])) (Just JsonNull) &&
     maybeEq (get "z" (JsonObject [("k", JsonNull)])) Nothing)
  check "at: in-bounds index hits, out-of-bounds misses"
    (maybeEq (at 0 (JsonArray [JsonBool True])) (Just (JsonBool True)) &&
     maybeEq (at 10 (JsonArray [JsonBool True])) Nothing)
  check "full parser on \"null\" (instance of NullRoundTripObligation)"
    (parsesTo "null" JsonNull)
  check "fast paths agree with full parser (instances of FastPathAgreesObligation)"
    (all (\t => case parseJson t of
                  Just v => parsesTo t v
                  Nothing => rejects t)
       ["null", "true", "false", "", "[]", "{}"])
  -- RFC 8259 grammar has no trailing commas. Kept last so the other checks
  -- above still report; a FAIL here is a suspected parser defect.
  check "full parser: trailing commas rejected (RFC 8259)"
    (rejects "[1,]" && rejects "{\"a\":1,}")
  putStrLn ""
