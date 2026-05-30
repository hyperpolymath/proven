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
import Data.Nat

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

||| OWED: getting a key that was just set returns that value:
||| `get k (set k v (JsonObject obj)) = Just v`.
||| `set` calls `update key val pairs` (`SafeJson.idr` L179) and `get`
||| calls `lookup key pairs` (L156). Both thread through `(==)` on
||| `String`, whose comparison is the opaque FFI primitive
||| `prim__eq_String`. Idris2 0.8.0 does not reduce `k == k = True`
||| by `Refl` — same blocker family as `SafeChecksum` Luhn/ISBN
||| (FFI-bound String) and `boj-server` `Boj.SafetyLemmas.charEqSym`
||| (the class-(J) `prim__eqChar` reflection gap, generalised to
||| `String`). Held back by the absence of a reflective
||| `prim__eq_String`-to-`Dec`-eq lemma in Idris2 0.8.0 base.
||| Discharge once a class-(J) `String` reflection axiom is added
||| or `update`/`lookup` are refactored onto a decidable-equality
||| key type.
public export
0 setGetIdentity : (k : String) -> (v : JsonValue) -> (obj : List (String, JsonValue)) ->
                           get k (set k v (JsonObject obj)) = Just v

||| OWED: setting a different key preserves the other key's value:
||| `Not (k1 = k2) -> get k2 (set k1 v obj) = get k2 obj`.
||| `set k1 v` calls `update k1 v pairs`, which compares `k1 == k`
||| for every pair `(k, _)` in the list. The proof requires that
||| `k1 == k2 = False` (negative direction) whenever `Not (k1 = k2)`
||| — the converse of `setGetIdentity`'s blocker, in the same
||| `prim__eq_String` reflection-gap family. Idris2 0.8.0 has no
||| primitive lemma `Not (a = b) -> prim__eq_String a b = False` and
||| the elaborator cannot derive it by `Refl`. Same shape as the
||| `gossamer` `stringNotEqCommut` class-(J) axiom landed in
||| `Panels.Distinct` (`PR #41`). Held back by absence of a
||| reflective `String`-disequality lemma. Discharge once the
||| class-(J) `stringNotEq` axiom is shared via base, or `update`
||| is rewritten on a `DecEq` carrier.
public export
0 setPreservesOther : (k1, k2 : String) -> Not (k1 = k2) ->
                              (v : JsonValue) -> (obj : JsonValue) ->
                              get k2 (set k1 v obj) = get k2 obj

||| OWED: after `set k v obj`, `hasKey k` returns `True` whenever
||| `obj` is an object.
||| `hasKey key json = isJust (get key json)` (`SafeJson.idr` L162),
||| so this reduces to `get k (set k v obj) = Just _`, which is the
||| same `prim__eq_String` reflection gap as `setGetIdentity`. The
||| extra hypothesis `isObject obj = True` only excludes the
||| non-`JsonObject` arms of `set` (which return `obj` unchanged) —
||| the residual obligation still threads through opaque String FFI
||| equality. Held back by the same Idris2 0.8.0 blocker. Discharge
||| together with `setGetIdentity` once a class-(J) `String`
||| reflection lemma is in base.
public export
0 setHasKey : (k : String) -> (v : JsonValue) -> (obj : JsonValue) ->
                      isObject obj = True -> hasKey k (set k v obj) = True

||| OWED: after `remove k obj`, `hasKey k` returns `False`.
||| `remove k (JsonObject pairs) = JsonObject (filter (\(k', _) => k' /= k) pairs)`
||| (`SafeJson.idr` L191). The proof needs `filter` to drop every
||| pair with key `k`, which requires `k' /= k = False` whenever
||| `k' = k` — i.e. `prim__eq_String k k = True` and its lifting
||| through `/=`. Same FFI-opaque-String blocker family as
||| `setGetIdentity` / `setPreservesOther`. Also requires the
||| non-`JsonObject` arms (which return `obj` unchanged) not to
||| contain `k`, but for arbitrary `obj : JsonValue` the statement
||| is unconditionally false on `JsonObject [(k, _)]` reduced
||| through any non-pattern-matching FFI path. Held back jointly
||| by the `prim__eq_String` reflection gap and the absence of a
||| reduction lemma for `filter` on FFI-keyed pairs. Discharge once
||| both are addressed.
public export
0 removeNotHasKey : (k : String) -> (obj : JsonValue) ->
                            hasKey k (remove k obj) = False

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
||| `lengthSnoc v arr : length (arr ++ [v]) = S (length arr)`, then
||| close the `S n` vs `n + 1` gap with `plusCommutative 1 n`.
||| (`Data.List.Equalities.lengthSnoc` takes the element first, then the
||| list — `lengthSnoc x xs`; the previous `lengthSnoc arr v` argument
||| order did not type-check under Idris2 0.8.0 contrib.)
public export
appendLengthInc : (v : JsonValue) -> (arr : List JsonValue) ->
                  arrayLength (append v (JsonArray arr)) =
                  Just (length arr + 1)
appendLengthInc v arr =
  cong Just (trans (lengthSnoc v arr) (plusCommutative 1 (length arr)))

--------------------------------------------------------------------------------
-- Path Access Properties
--------------------------------------------------------------------------------

||| Empty path returns the original value
public export
emptyPathIdentity : (v : JsonValue) -> getPath [] v = Just v
emptyPathIdentity v = Refl

||| DISCHARGED: a single-segment `Key k` path equals direct `lookup`:
||| `getPath [Key k] (JsonObject obj) = lookup k obj`.
|||
||| The original OWED comment correctly identified the blocker: while
||| `getPath`/`getSegment` were `covering` (via the `%default covering`
||| in `Proven.SafeJson.Access`), Idris2 0.8.0 does not reduce `covering`
||| definitions during proof conversion, so neither side of the equation
||| meets — verified: even the one-step
||| `getSegment (Key k) (JsonObject obj) = lookup k obj` fails by `Refl`
||| while the module default holds. The fix lives in
||| `Proven.SafeJson.Access`: both functions are now explicitly `total`
||| (they always were structurally — `getSegment` is non-recursive,
||| `getPath` decreases on the path list). With totality `getPath`
||| reduces:
|||   getPath [Key k] (JsonObject obj)
|||     = case getSegment (Key k) (JsonObject obj) of
|||         { Nothing => Nothing; Just v => getPath [] v }
|||     = case lookup k obj of { Nothing => Nothing; Just v => Just v }
||| and the `with`-pattern on `lookup k obj` closes both arms by `Refl`
||| (Maybe-identity).
public export
singleKeyPath : (k : String) -> (obj : List (String, JsonValue)) ->
                getPath [Key k] (JsonObject obj) = lookup k obj
singleKeyPath k obj with (lookup k obj)
  singleKeyPath k obj | Nothing = Refl
  singleKeyPath k obj | Just _  = Refl

-- singleIndexPath: removed — where-clause in type signature is invalid
-- in Idris2 0.8.0 and the index' helper conflicts with stdlib names.

--------------------------------------------------------------------------------
-- Parsing Properties
--------------------------------------------------------------------------------

-- The six `parse*Correct` / `parseEmpty*` declarations below all
-- share the same blocker family: `parseJson` (defined in
-- `Proven.SafeJson` and dispatching to `Proven.SafeJson.Parser`)
-- threads through `unpack`, `pack`, `strHead`, `strSubstr` and
-- explicit fuel-driven recursion. Every one of those operates on
-- `String` via Idris2 0.8.0's opaque FFI primitives, so no
-- `parseJson "<literal>"` reduces to its result by `Refl`. Same
-- blocker family as `SafeChecksum` Luhn/ISBN (FFI-bound String)
-- and `SafeHtml.escapePreservesNoLT`. They all discharge together
-- once Idris2 gains a reflective `String`-to-`List Char` axiom (or
-- the parser is rewritten to operate on `List Char` end-to-end so
-- the literal `unpack` can be supplied at the call site as a
-- trusted constant).

||| OWED: `parseJson "null" = Just JsonNull`.
||| Held back by Idris2 0.8.0's String-FFI opacity on the parser's
||| `unpack`/`strHead` driver (see module-level comment above).
||| Discharge once a reflective `String`-to-`List Char` axiom is
||| available, or the parser is refactored onto `List Char`.
public export
0 parseNullCorrect : parseJson "null" = Just JsonNull

||| OWED: `parseJson "true" = Just (JsonBool True)`.
||| Held back by the same String-FFI parser opacity as
||| `parseNullCorrect`. Discharge together.
public export
0 parseTrueCorrect : parseJson "true" = Just (JsonBool True)

||| OWED: `parseJson "false" = Just (JsonBool False)`.
||| Held back by the same String-FFI parser opacity as
||| `parseNullCorrect`. Discharge together.
public export
0 parseFalseCorrect : parseJson "false" = Just (JsonBool False)

||| OWED: `parseJson "" = Nothing`.
||| Held back by the same String-FFI parser opacity as
||| `parseNullCorrect` — `parseJson` cannot reduce the empty-string
||| early-return arm to `Nothing` by `Refl` because the dispatch
||| goes through `strHead`/`strLength` (FFI primitives). Discharge
||| together with the rest of the parse-* family.
public export
0 parseEmptyFails : parseJson "" = Nothing

||| OWED: `parseJson "[]" = Just (JsonArray [])`.
||| Held back by the same String-FFI parser opacity as
||| `parseNullCorrect`, plus the parser's two-character lookahead
||| (`'['` then `']'`) which threads through `strSubstr`. Discharge
||| together.
public export
0 parseEmptyArray : parseJson "[]" = Just (JsonArray [])

||| OWED: `parseJson "{}" = Just (JsonObject [])`.
||| Held back by the same String-FFI parser opacity as
||| `parseEmptyArray` (object-literal lookahead on `'{'` / `'}'`).
||| Discharge together.
public export
0 parseEmptyObject : parseJson "{}" = Just (JsonObject [])

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

||| DISCHARGED: every `JsonValue` matches `TAny`:
||| `matchesType v TAny = True` for all `v`.
||| Definitionally `matchesType _ TAny = True` (`SafeJson.idr` L353).
||| Unlike `singleKeyPath`, this needs no totality change: although
||| `matchesType` is `covering`, supplying a concrete head constructor
||| in each arm of a manual six-arm case-split
||| (`JsonNull` / `JsonBool b` / `JsonNumber n` / `JsonString s` /
||| `JsonArray arr` / `JsonObject pairs`) is enough — every earlier
||| clause demands a non-`TAny` second argument, so each arm reduces
||| through the catch-all `matchesType _ TAny = True` clause by `Refl`.
public export
anyMatchesTAny : (v : JsonValue) -> matchesType v TAny = True
anyMatchesTAny JsonNull       = Refl
anyMatchesTAny (JsonBool _)   = Refl
anyMatchesTAny (JsonNumber _) = Refl
anyMatchesTAny (JsonString _) = Refl
anyMatchesTAny (JsonArray _)  = Refl
anyMatchesTAny (JsonObject _) = Refl

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
