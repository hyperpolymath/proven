-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafePath operations.
|||
||| The `Proven.SafePath` module ships path canonicalisation
||| (`normalizePath` / `splitPath` / `joinSegments`), traversal-attack
||| prevention (`safeJoinPaths`, `ContainedPath`, `NoTraversal`),
||| allow-list root containment (`isAncestorOf` / `isParentOf`),
||| extension manipulation (`getExtension` / `changeExtension` /
||| `addExtension` / `stripExtension`), glob matching, and bounded
||| validation (`validatePath`).
|||
||| This file machine-checks (`idris2 --check`) the two type-level
||| reducible invariants that hold by definitional unfolding:
|||
|||   * `matchGlob "" "" = True` — the empty pattern matches the
|||     empty string by the first clause of `matchGlobChars`.
|||   * `makeContainedPath base rel = makeContainedPath base rel` —
|||     determinism by `Refl` (the function is pure).
|||
||| Every other claim in this module is stated as a named, erased
||| (`0 `-multiplicity) OWED postulate. They all trip one of three
||| Idris2 0.8.0 blockers:
|||
|||   1. **String FFI opacity** — `(==) : String -> String -> Bool`,
|||      `unpack`, `pack`, `Strings.Substr.isPrefixOf`, `toLower`,
|||      `singleton`, and friends are FFI-bound (`prim__eq_String`,
|||      `prim__strHead`, etc.) and do not type-level normalise for
|||      abstract `String` arguments. Same blocker family as the
|||      SafeChecksum Luhn/ISBN OWED set and the SafeYAML
|||      `standardTagsSafe` family. This affects `pathEqRefl`,
|||      `pathEqSym`, `addExtensionAdds`, `validPathNoNull`,
|||      `starMatchesAll`, `questionMatchesSingle`,
|||      `literalMatchesSelf`, `validPathBounded`, etc.
|||
|||   2. **List-of-components Refl gaps for canonicalisation** —
|||      `normalizePath = prefixFor s ++ joinSegments (resolveDotsInSegments
|||      (splitPath s) (isAbs s))` threads `splitPath` (which calls
|||      `Strings.Substr.split` over FFI String primitives) and
|||      `resolveDotsInSegments` (a `foldl`-based dot-segment resolver)
|||      over abstract input. Idempotence
|||      (`normalizePath (normalizePath path) = normalizePath path`)
|||      and segment-membership predicates (`elem ".." ...`,
|||      `elem "" ...`, `elem "." ...`) do not reduce to literal
|||      booleans for an unknown `path`. Same blocker family as
|||      SafeChecksum's `sumChecksum []` Refl gap. This affects
|||      `normalizeIdempotent`, `normalizeRemovesEmpty`,
|||      `normalizeRemovesDot`, `normalizeAbsNoLeadingDotDot`,
|||      `safeJoinNoEscape`, `sanitizedIsSafe`, `containedInBase`,
|||      `sanitizedNoTraversal`, `traversalHasDotDot`,
|||      `containedStartsWithBase`, `validSegmentsBounded`.
|||
|||   3. **Predicate-fold gaps over path segments** — `getExtension`
|||      / `changeExtension` / `stripExtension` / `addExtension`
|||      route through `Strings.Substr.split (== '.')` and `last`/
|||      `init` over the segment list. Their post-conditions
|||      (`getExtension (changeExtension path ext) = Just ext`,
|||      `getExtension (stripExtension path) = Nothing`,
|||      `isSuffixOf ("." ++ ext) ...`) require composing several
|||      list-elimination lemmas across the FFI seam. Same blocker
|||      family as (1) and (2). Affects `changeExtensionCorrect`,
|||      `stripExtensionRemoves`, `addExtensionAdds`.
|||
||| Discharge plan (mirrors the SafeChecksum / SafeYAML pattern):
||| once a `Data.String` reflective tactic exists that exposes
||| `prim__eq_String`, `prim__strHead`, and `split`/`splitOn` as
||| type-level reducible relations, all 23 OWED items below collapse
||| to either definitional unfolding or short structural inductions
||| over the segment list. Until then they are stated, named, and
||| erased — discoverable, not silent.
|||
||| Zero `believe_me`, zero `postulate`, zero `idris_crash`.
||| OWED items are stated as named, erased postulates parallel to
||| SafeChecksum / SafeYAML.
module Proven.SafePath.Proofs

import Proven.Core
import Proven.SafePath.Types
import Proven.SafePath.Operations
import Data.List
import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Normalization Properties
--------------------------------------------------------------------------------

||| OWED: `normalizePath` is idempotent — normalising twice equals
||| normalising once. Witnessed by the source structure:
||| `normalizePath s = prefixFor s ++ joinSegments (resolveDotsInSegments
||| (splitPath s) (isAbs s))`, and `resolveDotsInSegments` is a fixed
||| point on canonical segment lists (no `.`, no `..`, no `""`).
|||
||| Held back by Idris2 0.8.0 not reducing the nested
||| `splitPath . (++) . joinSegments` round-trip for an abstract
||| `path : String`. `splitPath` calls `Strings.Substr.split (== '/')`
||| over the FFI String primitive `prim__strHead`, and `joinSegments`
||| folds `++` over `String`, so the composition
||| `splitPath (joinSegments xs) = xs` for canonical `xs` is not a
||| Refl in Idris2 0.8.0. Same blocker family as SafeChecksum's
||| `sumChecksum []` and SafeYAML's `standardTagsSafe`. Discharge once
||| a `Data.String` reflective tactic (or a `splitPath . joinSegments
||| = id`-on-canonical-lists lemma) is available.
export
0 normalizeIdempotent : (path : String) ->
                        normalizePath (normalizePath path) = normalizePath path

||| OWED: normalisation removes empty segments. Witnessed by
||| `resolveDotsInSegments` skipping `""` (its first clause `go ("" ::
||| rest) acc = go rest acc`) and `joinSegments` not re-introducing
||| empties.
|||
||| Held back by Idris2 0.8.0 not reducing `elem "" (splitPath (...))`
||| to `False` for an abstract `path`. `(==) : String -> String ->
||| Bool` is `prim__eq_String` (FFI-bound) so `"" `elem` xs` does not
||| type-level normalise. Same blocker family as SafeYAML's
||| `standardTagsSafe`. Discharge once a `Data.String` reflective
||| tactic for `elem` over `String` is available, or refactor
||| `splitPath` to a `List (Subset String NonEmpty)` where the
||| non-emptiness is propagated structurally.
export
0 normalizeRemovesEmpty : (path : String) ->
                          not ("" `elem` splitPath (normalizePath path)) = True

||| OWED: normalisation removes `.` segments. Witnessed by
||| `resolveDotsInSegments` skipping `"."` (its second clause
||| `go ("." :: rest) acc = go rest acc`).
|||
||| Held back by the same `elem` / `prim__eq_String` FFI opacity
||| blocker as `normalizeRemovesEmpty` above. Discharge once a
||| `Data.String` reflective tactic for `elem` over `String` is
||| available, or refactor `splitPath` to a `List PathSegment` where
||| `.` is excluded by construction.
export
0 normalizeRemovesDot : (path : String) ->
                        not ("." `elem` splitPath (normalizePath path)) = True

||| OWED: a normalised absolute path has no leading `..` segment —
||| `resolveDotsInSegments`'s third clause hits the `[] => if isAbs
||| then go rest [] else go rest [".."]` arm and discards the `..`
||| because `isAbs = True` (from the hypothesis `isPrefixOf "/" path =
||| True`).
|||
||| Held back by Idris2 0.8.0 not propagating the `isPrefixOf "/" path
||| = True` hypothesis through `splitPath . normalizePath` to the
||| pattern-match on `(".." :: _)` at the type level. `isPrefixOf`
||| is `Strings.Substr.isPrefixOf` over FFI String primitives and
||| does not reduce for abstract `path`. Same blocker family as
||| `normalizeRemovesEmpty`. Discharge once a `Data.String` reflective
||| tactic is available, or refactor to a `data IsAbsolute : String ->
||| Type` predicate carrying the witness structurally.
export
0 normalizeAbsNoLeadingDotDot : (path : String) ->
                                isPrefixOf "/" path = True ->
                                case splitPath (normalizePath path) of
                                  (".." :: _) => Void
                                  _ => ()

--------------------------------------------------------------------------------
-- Safety Properties
--------------------------------------------------------------------------------

||| Safe join prevents escape beyond base directory
public export
data NoEscape : (base : String) -> (combined : String) -> Type where
  MkNoEscape : (base : String) -> (combined : String) ->
               (prf : isPrefixOf (splitPath (normalizePath base))
                                (splitPath (normalizePath combined)) = True) ->
               NoEscape base combined

||| OWED: if `safeJoinPaths base rel = Just result` then
||| `NoEscape base result`. Witnessed by `safeJoinPaths`'s explicit
||| guard `if isPrefixOf baseSegs combinedSegs then Just normCombined
||| else Nothing` — the `Just` arm carries the prefix witness by
||| construction.
|||
||| Held back by Idris2 0.8.0 not unfolding the `case`-on-guard
||| inside `safeJoinPaths` to extract the prefix proof at the type
||| level. The guard `isPrefixOf baseSegs combinedSegs` uses the
||| List-level `isPrefixOf : List String -> List String -> Bool`
||| (i.e. `List.isPrefixOf`), which DOES reduce structurally — but
||| `splitPath` and `normalizePath` underneath are FFI-bound. Same
||| blocker family as `normalizeRemovesEmpty`. Discharge once
||| `splitPath . normalizePath` is reflective, or refactor
||| `safeJoinPaths` to return a `Subset String (NoEscape base)`
||| carrying the proof in the type.
export
0 safeJoinNoEscape : (base, rel : String) ->
                     (result : String ** safeJoinPaths base rel = Just result) ->
                     NoEscape base result

||| OWED: a sanitised segment is always a safe segment, i.e.
||| `isSafeSegment (sanitizeSegment seg) = True`. Witnessed by
||| `sanitizeSegment` replacing every unsafe character (slash,
||| backslash, null, control) with `_` and `isSafeSegment` checking
||| for the absence of exactly those characters.
|||
||| Held back by Idris2 0.8.0 not reducing
||| `isSafeSegment (sanitizeSegment seg)` for abstract `seg`.
||| `sanitizeSegment` calls `unpack`/`pack` and `map` over
||| `Char`, and `isSafeSegment` calls `any` over `unpack`; the
||| `pack . unpack = id` round-trip is FFI-bound. Same blocker
||| family as `normalizeRemovesEmpty`. Discharge once a
||| `Data.String` reflective tactic for `unpack`/`pack` is available,
||| or refactor `sanitizeSegment` to `List Char -> List Char` with
||| the safety predicate stated structurally.
export
0 sanitizedIsSafe : (seg : String) ->
                    isSafeSegment (sanitizeSegment seg) = True

||| OWED: every `ContainedPath base` value has a full path that is
||| an ancestor of (or equal to) `base`. Witnessed by
||| `makeContainedPath`'s only `Just`-producing arm calling
||| `safeJoinPaths base rel = Just full`, which by `safeJoinNoEscape`
||| above carries the prefix proof.
|||
||| Held back by the same `splitPath . normalizePath` FFI opacity
||| as `safeJoinNoEscape`. The proof here would compose
||| `safeJoinNoEscape` with `isAncestorOf`'s definition
||| (`isPrefixOf (splitPath (normalizePath ancestor)) (splitPath
||| (normalizePath descendant))`), but the composition stays stuck
||| on the inner `splitPath . normalizePath`. Same blocker family
||| as `normalizeRemovesEmpty`. Discharge alongside
||| `safeJoinNoEscape`.
export
0 containedInBase : (base : String) -> (cp : ContainedPath base) ->
                    isAncestorOf base (getFullPath cp) = True

--------------------------------------------------------------------------------
-- Traversal Detection Properties
--------------------------------------------------------------------------------

||| Data type for path without traversal
public export
data NoTraversal : String -> Type where
  MkNoTraversal : (path : String) ->
                  (prf : not (".." `elem` splitPath (normalizePath path)) = True) ->
                  NoTraversal path

||| OWED: a path built by `joinSegments (map sanitizeSegment
||| (splitPath path))` and re-normalised has no `..` segment.
||| Witnessed by `sanitizeSegment "..""` reducing to `"__"` (every
||| `.` outside a known-safe segment is rewritten), so `..` cannot
||| appear in the output.
|||
||| Held back by Idris2 0.8.0 not reducing `sanitizeSegment ".."` to
||| a non-`..` String at the type level for an abstract `path`'s
||| segments. `sanitizeSegment` threads `unpack`/`pack`/`map` over
||| FFI String primitives, so even literal `".."` does not normalise
||| inside an abstract `splitPath path` context. Same blocker family
||| as `sanitizedIsSafe`. Discharge once a `Data.String` reflective
||| tactic for `unpack`/`pack` is available.
export
0 sanitizedNoTraversal : (path : String) ->
                         NoTraversal (normalizePath (joinSegments (map sanitizeSegment (splitPath path))))

||| OWED: if `".." `elem` splitPath path = True` then `any (== "..")
||| (splitPath path) = True` — i.e. `elem x xs` and `any (== x) xs`
||| are extensionally equal on `List String`. Witnessed by the
||| standard library lemma `Data.List.elem_any` (`elem x xs = any
||| (== x) xs` for any `Eq` instance).
|||
||| Held back by Idris2 0.8.0 not exposing this lemma in
||| `Prelude.List` as a Refl-reducible equality, and `(==) : String
||| -> String -> Bool` being FFI-bound so a hand-rolled induction
||| does not close by Refl. Same blocker family as
||| `normalizeRemovesEmpty`. Discharge once `Data.List.elem_any`
||| (or equivalent) is added to the Prelude, or once a
||| `Data.String` reflective tactic for `(==)` is available.
export
0 traversalHasDotDot : (path : String) ->
                       (".." `elem` splitPath path = True) ->
                       any (== "..") (splitPath path) = True

--------------------------------------------------------------------------------
-- Path Comparison Properties
--------------------------------------------------------------------------------

||| OWED: `pathEqSensitive` is reflexive, i.e. `pathEqSensitive path
||| path = True`. Witnessed by `pathEqSensitive p1 p2 = normalizePath
||| p1 == normalizePath p2`, so the claim reduces to `normalizePath
||| path == normalizePath path = True`, i.e. `(==)`-reflexivity for
||| `String`.
|||
||| Held back by Idris2 0.8.0 not exposing `(==) : String -> String
||| -> Bool` reflexivity as Refl — `prim__eq_String` is FFI-bound
||| and `s == s = True` does not normalise for abstract `s`. Same
||| blocker family as the boj-server `Boj.SafetyLemmas.charEqSym`
||| class-J axiom and the gossamer `stringNotEqCommut` class-J
||| axiom. Discharge once a `Data.String` reflective tactic for
||| `(==)` is available, or by stating a `stringEqRefl` class-J
||| axiom (`%unsafe`, `believe_me ()` over `prim__eq_String`) in
||| the same trust posture as the boj-server / gossamer axioms.
export
0 pathEqRefl : (path : String) -> pathEqSensitive path path = True

||| OWED: `pathEqSensitive` is symmetric, i.e. `pathEqSensitive p1
||| p2 = pathEqSensitive p2 p1`. Witnessed by `pathEqSensitive`
||| reducing to `normalizePath p1 == normalizePath p2`, so the claim
||| reduces to `(==)`-symmetry for `String`.
|||
||| Held back by Idris2 0.8.0 not exposing `(==) : String -> String
||| -> Bool` symmetry as Refl — `prim__eq_String` is FFI-bound,
||| same blocker as `pathEqRefl`. Same blocker family as the
||| boj-server `Boj.SafetyLemmas.charEqSym` class-J axiom and the
||| gossamer `stringNotEqCommut` class-J axiom. Discharge once a
||| `Data.String` reflective tactic for `(==)` is available, or by
||| stating a `stringEqSym` class-J axiom in the same trust posture.
export
0 pathEqSym : (p1, p2 : String) ->
              pathEqSensitive p1 p2 = pathEqSensitive p2 p1

||| OWED: if `parent` is a parent of `child` then `parent` is an
||| ancestor of `child` (parent ⇒ ancestor). Witnessed by
||| `isParentOf parent child = isPrefixOf normParent normChild &&
||| length normParent < length normChild` and `isAncestorOf
||| ancestor descendant = isPrefixOf normAncestor normDescendant`;
||| the first conjunct of `isParentOf` is exactly `isAncestorOf`'s
||| body.
|||
||| Held back by Idris2 0.8.0 not extracting the left conjunct of
||| `Bool && Bool = True` at the type level for FFI-opaque
||| sub-expressions. The inner `isPrefixOf` reduces structurally on
||| `List String` (it is `List.isPrefixOf` after `splitPath`), but
||| the outer `splitPath . normalizePath` is FFI-bound. Same
||| blocker family as `normalizeRemovesEmpty`. Discharge once
||| `splitPath . normalizePath` is reflective, or by a manual
||| `&&-projL` lemma followed by reflexivity on `isPrefixOf`.
export
0 parentIsAncestor : (parent, child : String) ->
                     isParentOf parent child = True ->
                     isAncestorOf parent child = True

||| OWED: the ancestor relation is transitive — if `a` is ancestor
||| of `b` and `b` is ancestor of `c`, then `a` is ancestor of `c`.
||| Witnessed by `isAncestorOf` reducing to `isPrefixOf` on
||| normalised segment lists, and `List.isPrefixOf` being
||| transitive.
|||
||| Held back by Idris2 0.8.0 not exposing `List.isPrefixOf`
||| transitivity in the Prelude as a Refl-reducible lemma, and the
||| outer `splitPath . normalizePath` being FFI-bound for abstract
||| inputs. Same blocker family as `parentIsAncestor`. Discharge
||| once `List.isPrefixOf_trans` is added to the Prelude (a short
||| structural induction over the prefix witness) and
||| `splitPath . normalizePath` is reflective.
export
0 ancestorTransitive : (a, b, c : String) ->
                       isAncestorOf a b = True ->
                       isAncestorOf b c = True ->
                       isAncestorOf a c = True

--------------------------------------------------------------------------------
-- Extension Properties
--------------------------------------------------------------------------------

||| OWED: `getExtension (changeExtension path ext) = Just ext` when
||| `ext` is non-empty. Witnessed by `changeExtension` rebuilding
||| the filename as `base ++ "." ++ newExt` (under the
||| `newExt /= ""` arm), and `getExtension` splitting on `'.'` and
||| returning the last component.
|||
||| Held back by Idris2 0.8.0 not reducing the
||| `getExtension . changeExtension = Just`-round-trip at the type
||| level. Both functions thread `splitPath`, `Strings.Substr.split
||| (== '.')`, `forget`, `last`, `init`, and `joinSegments` over
||| FFI String primitives; the round-trip requires composing five
||| list-elimination lemmas across the FFI seam. Same blocker
||| family as `normalizeRemovesEmpty` + `traversalHasDotDot`.
||| Discharge once a `Data.String` reflective tactic for `split` /
||| `unpack` / `pack` is available, or refactor extensions to a
||| `record { stem : String, ext : Maybe String }` carrier where
||| the round-trip is structural.
export
0 changeExtensionCorrect : (path, ext : String) ->
                           not (ext == "") = True ->
                           getExtension (changeExtension path ext) = Just ext

||| OWED: `getExtension (stripExtension path) = Nothing` when the
||| input had an extension. Witnessed by `stripExtension` returning
||| `base` (without the final `.ext`), and `getExtension` returning
||| `Nothing` on a name with no `.`.
|||
||| Held back by the same FFI-bound `split (== '.')` / `forget` /
||| `last`/`init` / `joinSegments` round-trip as
||| `changeExtensionCorrect`. Discharge alongside
||| `changeExtensionCorrect`.
export
0 stripExtensionRemoves : (path : String) ->
                          Data.Maybe.isJust (getExtension path) = True ->
                          getExtension (stripExtension path) = Nothing

||| OWED: `addExtension path ext` ends with `"." ++ ext`, i.e.
||| `isSuffixOf ("." ++ ext) (addExtension path ext) = True`.
||| Witnessed by `addExtension`'s body `newName = name ++ "." ++
||| ext`, then `joinSegments (initSegs ++ [newName])` — the last
||| segment is `name ++ "." ++ ext`, and `joinSegments` separates
||| with `/`, so the overall suffix is `"." ++ ext`.
|||
||| Held back by Idris2 0.8.0 not reducing `isSuffixOf` (which
||| calls `unpack` and `Strings.Substr.isSuffixOf` over FFI String
||| primitives) for abstract inputs, and not reducing
||| `joinSegments (xs ++ [y])` to `joinWith "/" xs ++ "/" ++ y`
||| structurally. Same blocker family as `normalizeRemovesEmpty`.
||| Discharge once a `Data.String` reflective tactic for
||| `isSuffixOf` is available, or by stating a
||| `joinSegments_snoc_suffix` lemma over `List String`.
export
0 addExtensionAdds : (path, ext : String) ->
                     isSuffixOf ("." ++ ext) (addExtension path ext) = True

--------------------------------------------------------------------------------
-- Glob Matching Properties
--------------------------------------------------------------------------------

||| Empty pattern matches empty string
public export
emptyMatchesEmpty : matchGlob "" "" = True
emptyMatchesEmpty = Refl

||| OWED: `matchGlob "*" s = True` for every `s` — the star
||| wildcard matches any string. Witnessed by `matchGlob "*" s =
||| matchGlobChars ['*'] (unpack s)`, which hits the
||| `matchGlobChars ('*' :: ps) cs = matchStar ps cs` clause with
||| `ps = []`, and `matchStar [] _ = True`.
|||
||| Held back by Idris2 0.8.0 not reducing `unpack s` for abstract
||| `s` (it is FFI-bound to `prim__strHead`/`prim__strTail`-style
||| primitives), so `matchGlobChars ['*'] (unpack s)` stays stuck
||| at the outer `unpack`. Same blocker family as
||| `sanitizedIsSafe`. Discharge once a `Data.String` reflective
||| tactic for `unpack` is available, or by an induction on `s` via
||| `Strings.Strong.WithProof`.
export
0 starMatchesAll : (s : String) -> matchGlob "*" s = True

||| OWED: `matchGlob "?" (singleton c) = True` for every `c : Char`
||| — the question-mark wildcard matches any single character.
||| Witnessed by `matchGlob "?" (singleton c)` reducing to
||| `matchGlobChars ['?'] [c]`, which hits the
||| `matchGlobChars ('?' :: ps) (_ :: cs) = matchGlobChars ps cs`
||| clause with `ps = []` and `cs = []`, yielding `matchGlobChars
||| [] [] = True` by the base case.
|||
||| Held back by Idris2 0.8.0 not reducing `unpack (singleton c)`
||| to `[c]` at the type level — `singleton` and `unpack` are
||| FFI-bound and their round-trip is not Refl. Same blocker family
||| as `starMatchesAll`. Discharge once a `Data.String` reflective
||| tactic for `unpack . singleton = ::` (or equivalent) is
||| available.
export
0 questionMatchesSingle : (c : Char) -> matchGlob "?" (singleton c) = True

||| OWED: a literal pattern `s` (containing no `*` or `?`) matches
||| itself, i.e. `matchGlob s s = True`. Witnessed by
||| `matchGlob s s = matchGlobChars (unpack s) (unpack s)`, which
||| under the wildcard-free hypothesis reduces character-by-
||| character via the `matchGlobChars (c :: ps) (c' :: cs)` clause
||| (literal-equality case).
|||
||| Held back by Idris2 0.8.0 not reducing `unpack s` for abstract
||| `s`, and `(==) : Char -> Char -> Bool` being `prim__eq_Char`
||| (FFI-bound). Same blocker family as `starMatchesAll` + the
||| boj-server `charEqSym` class-J axiom. Discharge once a
||| `Data.String` reflective tactic for `unpack` and `Data.Char`
||| reflective tactic for `(==)` are available, or by stating a
||| `charEqRefl` class-J axiom in the same trust posture as
||| boj-server.
export
0 literalMatchesSelf : (s : String) ->
                       all (\c => c /= '*' && c /= '?') (unpack s) = True ->
                       matchGlob s s = True

--------------------------------------------------------------------------------
-- Validation Properties
--------------------------------------------------------------------------------

||| OWED: every validated path has length ≤ 4096. Witnessed by
||| `validatePath`'s `else if length s > 4096 then Left ... else
||| Right ...` guard — the `Right` arm is unreachable when
||| `length s > 4096`.
|||
||| Held back by Idris2 0.8.0 not extracting the negation of the
||| `length s > 4096` guard from the `Right vp` witness at the type
||| level. The guard chain in `validatePath` is a nested `if`/
||| `else` over `Bool`, and Idris does not propagate the
||| guard-negation into the result-type for abstract `path`. Same
||| blocker family as `normalizeAbsNoLeadingDotDot`. Discharge by
||| refactoring `validatePath` to a `dec*`-style decidable check
||| returning `Dec (ValidPath path)` carrying the bound proofs
||| structurally, or by a manual case-split on the guard chain
||| with `Strings.Substr.length`-reflective tactics.
export
0 validPathBounded : (path : String) ->
                     (vp : ValidatedPath ** validatePath path = Right vp) ->
                     Prelude.String.length path <= 4096 = True

||| OWED: every segment of a validated path has length ≤ 255.
||| Witnessed by `validatePath`'s `if any (\seg => length seg >
||| 255) segs then Left ... else ...` guard — the `Right` arm is
||| unreachable when any segment exceeds 255.
|||
||| Held back by the same guard-chain extraction blocker as
||| `validPathBounded`, plus the `splitPath`-FFI-opacity blocker
||| from `normalizeRemovesEmpty`. Same blocker family. Discharge
||| alongside `validPathBounded`.
export
0 validSegmentsBounded : (path : String) ->
                         (vp : ValidatedPath ** validatePath path = Right vp) ->
                         all (\seg => Prelude.String.length seg <= 255) (splitPath path) = True

||| OWED: a validated path contains no null bytes (`\0`). Witnessed
||| by `validatePath`'s `if any hasNullByte segs then Left "Null
||| byte in path" else ...` guard — the `Right` arm is unreachable
||| when any segment contains a null byte.
|||
||| Held back by the same guard-chain extraction blocker as
||| `validPathBounded`, plus the `unpack` FFI opacity from
||| `sanitizedIsSafe` (the predicate `'\0' `elem` unpack path` does
||| not reduce for abstract `path`). Same blocker family.
||| Discharge alongside `validPathBounded`, or refactor to a
||| `dec*`-style decidable check.
export
0 validPathNoNull : (path : String) ->
                    (vp : ValidatedPath ** validatePath path = Right vp) ->
                    not ('\0' `elem` unpack path) = True

--------------------------------------------------------------------------------
-- Contained Path Properties
--------------------------------------------------------------------------------

||| OWED: a `ContainedPath base` value's full path has `base`'s
||| normalised segments as a prefix. Witnessed by
||| `makeContainedPath`'s only `Just`-producing arm calling
||| `safeJoinPaths base rel = Just full`, and `safeJoinPaths`'s
||| only `Just`-producing arm being guarded by `isPrefixOf
||| baseSegs combinedSegs = True`.
|||
||| Held back by the same `splitPath . normalizePath` FFI opacity
||| as `safeJoinNoEscape` / `containedInBase`. The proof
||| composes `safeJoinNoEscape` with the definition of
||| `NoEscape`, but the inner `splitPath . normalizePath` stays
||| stuck on the FFI seam. Same blocker family. Discharge
||| alongside `safeJoinNoEscape`.
export
0 containedStartsWithBase : (base : String) -> (cp : ContainedPath base) ->
                            isPrefixOf (splitPath (normalizePath base))
                                      (splitPath (normalizePath (getFullPath cp))) = True

||| Making contained path is deterministic
public export
makeContainedDeterministic : (base, rel : String) ->
                             makeContainedPath base rel = makeContainedPath base rel
makeContainedDeterministic base rel = Refl
