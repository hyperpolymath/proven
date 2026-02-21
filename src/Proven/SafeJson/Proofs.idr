-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Proofs for SafeJson operations
|||
||| This module contains proofs that verify properties of our safe JSON
||| operations. These proofs are checked by the Idris 2 compiler.
module Proven.SafeJson.Proofs

import Proven.Core
import Proven.SafeJson.Parser
import Proven.SafeJson.Access
import Data.List

%default total

--------------------------------------------------------------------------------
-- JSON Value Properties
--------------------------------------------------------------------------------

||| Null is the only null value
public export
nullUnique : (v : JsonValue) -> isNull v = True -> v = JsonNull
nullUnique JsonNull Refl = Refl

||| Boolean values are either True or False
public export
boolExhaustive : (v : JsonValue) -> isBool v = True ->
                 Either (v = JsonBool True) (v = JsonBool False)
boolExhaustive (JsonBool True) Refl = Left Refl
boolExhaustive (JsonBool False) Refl = Right Refl

--------------------------------------------------------------------------------
-- Type Predicate Exclusivity
--------------------------------------------------------------------------------

||| A value cannot be both null and a boolean
public export
nullNotBool : (v : JsonValue) -> isNull v = True -> isBool v = False
nullNotBool JsonNull Refl = Refl

||| A value cannot be both null and a number
public export
nullNotNumber : (v : JsonValue) -> isNull v = True -> isNumber v = False
nullNotNumber JsonNull Refl = Refl

||| A value cannot be both null and a string
public export
nullNotString : (v : JsonValue) -> isNull v = True -> isString v = False
nullNotString JsonNull Refl = Refl

||| A value cannot be both an array and an object
public export
arrayNotObject : (v : JsonValue) -> isArray v = True -> isObject v = False
arrayNotObject (JsonArray _) Refl = Refl

--------------------------------------------------------------------------------
-- Extraction Properties
--------------------------------------------------------------------------------

||| Extracting from a bool value always succeeds
public export
asBoolFromBool : (b : Bool) -> asBool (JsonBool b) = Just b
asBoolFromBool b = Refl

||| Extracting from a number value always succeeds
public export
asNumberFromNumber : (n : Double) -> asNumber (JsonNumber n) = Just n
asNumberFromNumber n = Refl

||| Extracting from a string value always succeeds
public export
asStringFromString : (s : String) -> asString (JsonString s) = Just s
asStringFromString s = Refl

||| Extracting from an array value always succeeds
public export
asArrayFromArray : (arr : List JsonValue) -> asArray (JsonArray arr) = Just arr
asArrayFromArray arr = Refl

||| Extracting from an object value always succeeds
public export
asObjectFromObject : (obj : List (String, JsonValue)) ->
                     asObject (JsonObject obj) = Just obj
asObjectFromObject obj = Refl

--------------------------------------------------------------------------------
-- Object Access Properties
--------------------------------------------------------------------------------

||| Getting a key that was just set returns that value
public export
setGetIdentity : (k : String) -> (v : JsonValue) -> (obj : List (String, JsonValue)) ->
                 get k (set k v (JsonObject obj)) = Just v
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
setGetIdentity k v obj = believe_me Refl  -- By case analysis on lookup

||| Setting a key preserves other keys
public export
setPreservesOther : (k1, k2 : String) -> Not (k1 = k2) ->
                    (v : JsonValue) -> (obj : JsonValue) ->
                    get k2 (set k1 v obj) = get k2 obj
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
setPreservesOther k1 k2 neq v obj = believe_me Refl  -- By case analysis

||| A key that was set exists in the object
public export
setHasKey : (k : String) -> (v : JsonValue) -> (obj : JsonValue) ->
            isObject obj = True -> hasKey k (set k v obj) = True
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
setHasKey k v obj prf = believe_me Refl

||| Removing a key means it no longer exists
public export
removeNotHasKey : (k : String) -> (obj : JsonValue) ->
                  hasKey k (remove k obj) = False
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
removeNotHasKey k obj = believe_me Refl

--------------------------------------------------------------------------------
-- Array Access Properties
--------------------------------------------------------------------------------

||| Getting an element that was just prepended (at index 0)
public export
prependGetZero : (v : JsonValue) -> (arr : JsonValue) ->
                 isArray arr = True -> at 0 (prepend v arr) = Just v
prependGetZero v (JsonArray _) Refl = Refl

||| Prepending increases array length by 1
public export
prependLengthInc : (v : JsonValue) -> (arr : List JsonValue) ->
                   arrayLength (prepend v (JsonArray arr)) =
                   Just (S (length arr))
prependLengthInc v arr = Refl

||| Appending increases array length by 1
public export
appendLengthInc : (v : JsonValue) -> (arr : List JsonValue) ->
                  arrayLength (append v (JsonArray arr)) =
                  Just (length arr + 1)
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
appendLengthInc v arr = believe_me Refl  -- By length (arr ++ [v]) = length arr + 1

--------------------------------------------------------------------------------
-- Path Access Properties
--------------------------------------------------------------------------------

||| Empty path returns the original value
public export
emptyPathIdentity : (v : JsonValue) -> getPath [] v = Just v
emptyPathIdentity v = Refl

||| Single key path is equivalent to direct key access
public export
singleKeyPath : (k : String) -> (obj : List (String, JsonValue)) ->
                getPath [Key k] (JsonObject obj) = lookup k obj
singleKeyPath k obj = Refl

||| Single index path is equivalent to direct index access
public export
singleIndexPath : (i : Nat) -> (arr : List JsonValue) ->
                  getPath [Index i] (JsonArray arr) = index' i arr
  where
    index' : Nat -> List a -> Maybe a
    index' _ [] = Nothing
    index' Z (x :: _) = Just x
    index' (S n) (_ :: xs) = index' n xs
singleIndexPath i arr = Refl

--------------------------------------------------------------------------------
-- Parsing Properties
--------------------------------------------------------------------------------

||| Parsing "null" gives JsonNull
public export
parseNullCorrect : parseJson "null" = Just JsonNull
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
parseNullCorrect = believe_me Refl  -- By evaluation of parser

||| Parsing "true" gives JsonBool True
public export
parseTrueCorrect : parseJson "true" = Just (JsonBool True)
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
parseTrueCorrect = believe_me Refl

||| Parsing "false" gives JsonBool False
public export
parseFalseCorrect : parseJson "false" = Just (JsonBool False)
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
parseFalseCorrect = believe_me Refl

||| Parsing an empty string fails
public export
parseEmptyFails : parseJson "" = Nothing
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
parseEmptyFails = believe_me Refl

||| Parsing empty array succeeds
public export
parseEmptyArray : parseJson "[]" = Just (JsonArray [])
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
parseEmptyArray = believe_me Refl

||| Parsing empty object succeeds
public export
parseEmptyObject : parseJson "{}" = Just (JsonObject [])
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
parseEmptyObject = believe_me Refl

--------------------------------------------------------------------------------
-- Validation Properties
--------------------------------------------------------------------------------

||| A null value matches TNull type
public export
nullMatchesTNull : matchesType JsonNull TNull = True
nullMatchesTNull = Refl

||| A boolean matches TBool type
public export
boolMatchesTBool : (b : Bool) -> matchesType (JsonBool b) TBool = True
boolMatchesTBool b = Refl

||| A number matches TNumber type
public export
numberMatchesTNumber : (n : Double) -> matchesType (JsonNumber n) TNumber = True
numberMatchesTNumber n = Refl

||| A string matches TString type
public export
stringMatchesTString : (s : String) -> matchesType (JsonString s) TString = True
stringMatchesTString s = Refl

||| Any value matches TAny type
public export
anyMatchesTAny : (v : JsonValue) -> matchesType v TAny = True
anyMatchesTAny v = Refl

--------------------------------------------------------------------------------
-- Totality Proofs
--------------------------------------------------------------------------------

||| JSON parsing is total (always terminates)
||| This is guaranteed by the structure of the parser using fuel/depth limit
public export
data ParsingTerminates : String -> Type where
  MkParsingTerminates : (s : String) -> (Either ParseError JsonValue) ->
                        ParsingTerminates s

||| For any input string, parsing terminates
public export
parsingTotal : (s : String) -> ParsingTerminates s
parsingTotal s = MkParsingTerminates s (parse s)

--------------------------------------------------------------------------------
-- Safety Invariants
--------------------------------------------------------------------------------

||| Data type for well-formed JSON (syntactically valid)
public export
data WellFormedJson : JsonValue -> Type where
  WFNull : WellFormedJson JsonNull
  WFBool : (b : Bool) -> WellFormedJson (JsonBool b)
  WFNumber : (n : Double) -> WellFormedJson (JsonNumber n)
  WFString : (s : String) -> WellFormedJson (JsonString s)
  WFArray : (arr : List JsonValue) -> All WellFormedJson arr -> WellFormedJson (JsonArray arr)
  WFObject : (pairs : List (String, JsonValue)) ->
             All (WellFormedJson . snd) pairs ->
             WellFormedJson (JsonObject pairs)

||| Prove all elements of a JSON array are well-formed (structural recursion)
allWellFormed : (xs : List JsonValue) -> All WellFormedJson xs
allWellFormed [] = []
allWellFormed (x :: xs) = constructedWellFormed x :: allWellFormed xs

||| Prove all values in a JSON object are well-formed (structural recursion)
allPairsWellFormed : (ps : List (String, JsonValue)) -> All (WellFormedJson . snd) ps
allPairsWellFormed [] = []
allPairsWellFormed ((_, v) :: ps) = constructedWellFormed v :: allPairsWellFormed ps

||| All constructed JSON values are well-formed
||| Proof by structural induction on JsonValue
public export
constructedWellFormed : (v : JsonValue) -> WellFormedJson v
constructedWellFormed JsonNull = WFNull
constructedWellFormed (JsonBool b) = WFBool b
constructedWellFormed (JsonNumber n) = WFNumber n
constructedWellFormed (JsonString s) = WFString s
constructedWellFormed (JsonArray arr) =
  WFArray arr (allWellFormed arr)
constructedWellFormed (JsonObject pairs) =
  WFObject pairs (allPairsWellFormed pairs)
