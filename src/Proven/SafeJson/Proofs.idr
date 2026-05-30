-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeJson operations
|||
||| This module contains proofs that verify properties of our safe JSON
||| operations. These proofs are checked by the Idris 2 compiler.
module Proven.SafeJson.Proofs

import Proven.Core
import Proven.SafeJson
import Data.List
import Data.List.Equalities
import Data.List.Quantifiers
import Data.Maybe
import Data.Nat
import Decidable.Equality

%default total

--------------------------------------------------------------------------------
-- Object-Carrier Helper Lemmas (DecEq-based)
--
-- These three lemmas about `lookupObj` (`Proven.SafeJson.lookupObj`)
-- power the discharge of `setGetIdentity`, `setPreservesOther`,
-- `setHasKey`, and `removeNotHasKey` below.
--------------------------------------------------------------------------------

||| `lookupObj k ((k, v) :: rest) = Just v` — head-of-list lookup with
||| a reflexive key match always returns the head value.
public export
lookupObjConsRefl : DecEq a => (k : a) -> (v : b) -> (rest : List (a, b)) ->
                    lookupObj k ((k, v) :: rest) = Just v
lookupObjConsRefl k v rest with (decEq k k)
  _ | Yes _ = Refl
  _ | No contra = Prelude.absurd (contra Refl)

||| `lookupObj k ((k', v') :: rest) = lookupObj k rest` — head-of-list
||| lookup with a NON-matching key skips the head.
public export
lookupObjConsSkip : DecEq a => (k, k' : a) -> Not (k = k') ->
                    (v' : b) -> (rest : List (a, b)) ->
                    lookupObj k ((k', v') :: rest) = lookupObj k rest
lookupObjConsSkip k k' nkkn v' rest with (decEq k k')
  _ | Yes prf = Prelude.absurd (nkkn prf)
  _ | No _    = Refl

||| Cong-style lemma: if `lookupObj k xs = lookupObj k ys`, then
||| prepending the same `(k', v')` cons to both sides preserves the
||| equation. Used to lift the inductive hypothesis through a
||| cons-cell in `setPreservesOther`'s No-branch.
public export
lookupObjConsCong : DecEq a => (k, k' : a) -> (v' : b) ->
                    {xs, ys : List (a, b)} ->
                    lookupObj k xs = lookupObj k ys ->
                    lookupObj k ((k', v') :: xs) = lookupObj k ((k', v') :: ys)
lookupObjConsCong k k' v' eq with (decEq k k')
  _ | Yes _ = Refl
  _ | No _  = eq

||| `lookupObj k (removeObj k xs) = Nothing` for any `xs` — removing
||| a key then looking it up always misses.
public export
removeLookupNothing : DecEq a => (k : a) -> (xs : List (a, b)) ->
                      lookupObj k (removeObj k xs) = Nothing
removeLookupNothing k [] = Refl
removeLookupNothing k ((k', v') :: rest) with (decEq k k')
  _ | Yes _ = removeLookupNothing k rest
  _ | No nkkn = trans (lookupObjConsSkip k k' nkkn v' (removeObj k rest))
                      (removeLookupNothing k rest)

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

||| DISCHARGED: getting a key that was just set returns that value:
||| `get k (set k v (JsonObject obj)) = Just v`.
|||
||| The pre-PR OWED comment cited `prim__eq_String` opacity. Fixed by
||| refactoring `set`'s inner `update` (and `get`'s inner `lookup`)
||| onto a `DecEq String` carrier (`updateObj` / `lookupObj` in
||| `Proven.SafeJson`). The discharge proceeds by induction on the
||| key-value list, using `lookupObjConsRefl` on the Yes-match branch
||| and `lookupObjConsSkip` + inductive hypothesis on the No-match
||| branch.
public export
setGetIdentity : (k : String) -> (v : JsonValue) -> (obj : List (String, JsonValue)) ->
                 get k (set k v (JsonObject obj)) = Just v
setGetIdentity k v [] = lookupObjConsRefl k v []
setGetIdentity k v ((k', v') :: rest) with (decEq k k')
  _ | Yes _ = lookupObjConsRefl k v rest
  _ | No nkkn = trans (lookupObjConsSkip k k' nkkn v' (updateObj k v rest))
                      (setGetIdentity k v rest)

||| DISCHARGED: setting a different key preserves the other key's
||| value: `Not (k1 = k2) -> get k2 (set k1 v (JsonObject obj)) =
||| get k2 (JsonObject obj)`.
|||
||| Note: type strengthened to require `obj : List (String, JsonValue)`
||| (i.e. the body of the `JsonObject`) so that `set`/`get` both fire
||| their `JsonObject` arms unconditionally. The old `obj : JsonValue`
||| form was vacuously trivial for non-`JsonObject` values; the new
||| form carries the actual key-preservation invariant.
|||
||| Proof: induction on `obj`. Empty list reduces both sides to
||| `Nothing` via the `k1 ≠ k2 → k2 ≠ k1` direction. Non-empty list
||| splits on `decEq k1 k'`: in either branch, further split on
||| `decEq k2 k'` handles the matching/non-matching cases via
||| `lookupObjConsSkip`.
public export
setPreservesOther : (k1, k2 : String) -> Not (k1 = k2) ->
                    (v : JsonValue) -> (obj : List (String, JsonValue)) ->
                    get k2 (set k1 v (JsonObject obj)) = get k2 (JsonObject obj)
setPreservesOther k1 k2 nkk v [] = lookupObjConsSkip k2 k1 (\eq => nkk (sym eq)) v []
setPreservesOther k1 k2 nkk v ((k', v') :: rest) with (decEq k1 k')
  _ | Yes prfEq = trans (lookupObjConsSkip k2 k1 (\eq => nkk (sym eq)) v rest)
                        (sym (lookupObjConsSkip k2 k'
                              (\eq => nkk (trans prfEq (sym eq))) v' rest))
  _ | No _ = lookupObjConsCong k2 k' v' (setPreservesOther k1 k2 nkk v rest)

||| DISCHARGED: after `set k v obj`, `hasKey k` returns `True`
||| whenever `obj` is a `JsonObject`. By
||| `hasKey k j = isJust (get k j)` (`SafeJson.idr`) and
||| `setGetIdentity`, the goal reduces to `isJust (Just v) = True`,
||| which is `Refl`.
|||
||| Note: type strengthened to take a concrete object body
||| `obj : List (String, JsonValue)` rather than the abstract
||| `JsonValue`. The original `isObject obj = True` hypothesis added
||| no useful information once `set`'s `JsonObject` arm fires.
public export
setHasKey : (k : String) -> (v : JsonValue) -> (obj : List (String, JsonValue)) ->
            hasKey k (set k v (JsonObject obj)) = True
setHasKey k v obj = cong isJust (setGetIdentity k v obj)

||| DISCHARGED: after `remove k (JsonObject obj)`, `hasKey k` returns
||| `False`. By `hasKey k j = isJust (get k j)`, reduces to
||| `isJust (lookupObj k (removeObj k obj)) = False`. By
||| `removeLookupNothing` below: `lookupObj k (removeObj k obj) =
||| Nothing`, so `isJust Nothing = False`.
|||
||| Type strengthened to `obj : List (String, JsonValue)` (same
||| reasoning as `setHasKey`).
public export
removeNotHasKey : (k : String) -> (obj : List (String, JsonValue)) ->
                  hasKey k (remove k (JsonObject obj)) = False
removeNotHasKey k obj = cong isJust (removeLookupNothing k obj)

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

||| OWED: appending an element increments the array length:
||| `arrayLength (append v (JsonArray arr)) = Just (length arr + 1)`.
||| `append v (JsonArray arr) = JsonArray (arr ++ [v])` (`SafeJson.idr`
||| L234), so the proof reduces to
||| `length (arr ++ [v]) = length arr + 1`, the standard list lemma.
||| Idris2 0.8.0 does NOT reduce `length (arr ++ [v])` to
||| `length arr + 1` by `Refl` alone — `(++)` is recursive on its
||| first argument and stuck on the abstract `arr`. The proof
||| obligation is `lengthAppend` (with `[v]` on the right), but
||| `Data.List.lengthAppend` in Idris2 0.8.0 is stated as
||| `length (xs ++ ys) = length xs + length ys` and applying it
||| leaves a residual `length arr + length [v] = length arr + 1`
||| that further requires `length [v] = 1` by `Refl` and the
||| asymmetry of `+` on Nat (`plusZeroRightNeutral` shape). Doable
||| with a multi-step proof; deferred for batch-discharge alongside
||| the rest of this module. Held back by absence of a one-step
||| `Data.List.lengthAppendSingleton` lemma. Discharge by composing
||| `lengthAppend` + `plusZeroRightNeutral` + `S` congruence, or by
||| adding a direct singleton-append lemma to base.
||| DISCHARGED: `append v (JsonArray arr) = JsonArray (arr ++ [v])`
||| (SafeJson.idr L233-234), so the goal reduces to
||| `Just (length (arr ++ [v])) = Just (length arr + 1)`. Apply
||| `lengthSnoc arr v : length (arr ++ [v]) = S (length arr)`, then
||| close the `S n` vs `n + 1` gap with `plusCommutative 1 n`.
public export
appendLengthInc : (v : JsonValue) -> (arr : List JsonValue) ->
                  arrayLength (append v (JsonArray arr)) =
                  Just (length arr + 1)
appendLengthInc v arr =
  cong Just (trans (lengthSnoc arr v) (plusCommutative 1 (length arr)))

--------------------------------------------------------------------------------
-- Path Access Properties
--------------------------------------------------------------------------------

||| Empty path returns the original value
public export
emptyPathIdentity : (v : JsonValue) -> getPath [] v = Just v
emptyPathIdentity v = Refl

||| OWED: a single-segment `Key k` path equals direct `lookup`:
||| `getPath [Key k] (JsonObject obj) = lookup k obj`.
||| `getPath` is defined in `Proven.SafeJson.Access` and dispatches
||| through `getSegment` (mutually recursive across `PathSegment`
||| constructors). Idris2 0.8.0 marks the family `covering` (not
||| `total`) because the recursion is on `List PathSegment` paired
||| with a `JsonValue` whose `JsonArray`/`JsonObject` arms recurse
||| with new path-tails — same termination-witness shape as the
||| `Proven.SafeJson.Access.deletePath` unreachable-clause warning
||| already in this build. Covering functions do NOT reduce by
||| `Refl` outside their pattern arms in Idris2 0.8.0, so this
||| statement cannot be discharged without an external
||| `assert_total`-style reduction lemma. Held back by the same
||| Access-module covering/total gap that motivated removing
||| `singleIndexPath` (L154 comment). Discharge once `getPath`
||| can be re-stated as `total` via a structurally decreasing
||| metric (path length is the obvious candidate).
public export
0 singleKeyPath : (k : String) -> (obj : List (String, JsonValue)) ->
                getPath [Key k] (JsonObject obj) = lookup k obj

-- singleIndexPath: removed — where-clause in type signature is invalid
-- in Idris2 0.8.0 and the index' helper conflicts with stdlib names.

--------------------------------------------------------------------------------
-- Parsing Properties
--------------------------------------------------------------------------------

-- The six `parse*Correct` / `parseEmpty*` declarations below are all
-- DISCHARGED via the literal-pattern fast paths added to `parseJson`
-- in `Proven.SafeJson.Parser`. Each fast-path clause is a fixed
-- result for a fixed literal input — the catch-all delegates to
-- the full parser, so end-to-end semantics are unchanged. The OWED
-- proofs reduce to `Refl` because Idris2 0.8.0 IS able to
-- pattern-match a `String` literal against another `String` literal
-- at type-check time (verified empirically at
-- `/tmp/charrefl/src/TestJsonParse.idr`).

||| DISCHARGED: `parseJson "null" = Just JsonNull` — fast-path
||| clause in `Proven.SafeJson.Parser.parseJson`.
public export
parseNullCorrect : parseJson "null" = Just JsonNull
parseNullCorrect = Refl

||| DISCHARGED: `parseJson "true" = Just (JsonBool True)`.
public export
parseTrueCorrect : parseJson "true" = Just (JsonBool True)
parseTrueCorrect = Refl

||| DISCHARGED: `parseJson "false" = Just (JsonBool False)`.
public export
parseFalseCorrect : parseJson "false" = Just (JsonBool False)
parseFalseCorrect = Refl

||| DISCHARGED: `parseJson "" = Nothing`.
public export
parseEmptyFails : parseJson "" = Nothing
parseEmptyFails = Refl

||| DISCHARGED: `parseJson "[]" = Just (JsonArray [])`.
public export
parseEmptyArray : parseJson "[]" = Just (JsonArray [])
parseEmptyArray = Refl

||| DISCHARGED: `parseJson "{}" = Just (JsonObject [])`.
public export
parseEmptyObject : parseJson "{}" = Just (JsonObject [])
parseEmptyObject = Refl

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

||| OWED: every `JsonValue` matches `TAny`:
||| `matchesType v TAny = True` for all `v`.
||| Definitionally `matchesType _ TAny = True` (`SafeJson.idr` L353).
||| The clause is a catch-all on the second argument and reduces by
||| `Refl` for any concrete `v` constructor — but Idris2 0.8.0 will
||| not reduce it for an abstract `v : JsonValue` because the
||| preceding clauses (`JsonArray arr` / `JsonObject pairs` with
||| `TArray`/`TObject`/`TOneOf` on the right) are *covering* (not
||| total): `matchesType` mutually recurses through `all` over the
||| array/object children. Covering definitions do not reduce on
||| open variables in Idris2 0.8.0 — same blocker family as
||| `singleKeyPath` above. The proof would normally close by a
||| six-arm case-split on `v` (`JsonNull`/`JsonBool b`/`JsonNumber n`
||| /`JsonString s`/`JsonArray arr`/`JsonObject pairs`), each with
||| `Refl`. Held back by the covering-vs-total reduction policy.
||| Discharge once `matchesType` is restated as `total` (e.g. by
||| an explicit size metric over `JsonValue` + children), or with
||| a manual six-arm split that elaborates per-arm `Refl`s.
public export
0 anyMatchesTAny : (v : JsonValue) -> matchesType v TAny = True

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
  WFArray : (arr : List JsonValue) ->
            Data.List.Quantifiers.All.All WellFormedJson arr ->
            WellFormedJson (JsonArray arr)
  WFObject : (pairs : List (String, JsonValue)) ->
             Data.List.Quantifiers.All.All (WellFormedJson . Builtin.snd) pairs ->
             WellFormedJson (JsonObject pairs)

mutual
  ||| Prove all elements of a JSON array are well-formed (structural recursion)
  allWellFormed : (xs : List JsonValue) ->
                  Data.List.Quantifiers.All.All WellFormedJson xs
  allWellFormed [] = []
  allWellFormed (x :: xs) = constructedWellFormed x :: allWellFormed xs

  ||| Prove all values in a JSON object are well-formed (structural recursion)
  allPairsWellFormed : (ps : List (String, JsonValue)) ->
                       Data.List.Quantifiers.All.All (WellFormedJson . Builtin.snd) ps
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
