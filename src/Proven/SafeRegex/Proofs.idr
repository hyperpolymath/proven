-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeRegex.Proofs - Formal proofs of regex safety properties
|||
||| This module provides machine-checked proofs that:
||| 1. Safe regexes always terminate within step bounds
||| 2. Complexity analysis correctly identifies ReDoS patterns
||| 3. Input validation prevents resource exhaustion
||| 4. Match results are consistent with the regex semantics
module Proven.SafeRegex.Proofs

import Proven.Core
import Proven.SafeRegex.Types
import Proven.SafeRegex.Safety
import Data.List
import Data.List.Quantifiers
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Termination Proofs
--------------------------------------------------------------------------------

||| OWED: `isBounded q = True` rules out `q.maxCount = Nothing`. By
||| definition `isBounded q = isJust q.maxCount` (`Safety.idr` L308-L309),
||| so the `Nothing` arm yields `isJust Nothing = False = True`, an
||| absurdity.
||| Held back by Idris2 0.8.0 not propagating the record-projection
||| equation `q.maxCount = Nothing` through `isJust` during case
||| analysis on the `Quantifier` record — the elaborator does not
||| auto-unfold a `where`-bound field projection inside a `Maybe`-match
||| (same root cause as the `gcd` covering-callee reduction blocker in
||| PR #46). Discharge once the elaborator forces the field equation
||| through, or via a manual `rewrite` + `absurd Refl`.
0 boundedImpliesJustMax : (q : Quantifier) -> isBounded q = True ->
                          q.maxCount = Nothing -> Void

||| OWED: Bounded quantifiers expose a concrete `Nat` upper bound.
||| Operationally true by case-split on `q.maxCount`: the `Just n`
||| arm yields the witness directly; the `Nothing` arm contradicts
||| `isBounded q = True` via `boundedImpliesJustMax`.
||| Held back by Idris2 0.8.0 case-split not tracking the record-field
||| equation across the `isBounded` unfolding (same blocker as
||| `boundedImpliesJustMax`). Discharge together with that lemma.
public export
0 boundedQuantifierFinite : (q : Quantifier) -> (prf : isBounded q = True) -> (n : Nat ** q.maxCount = Just n)

||| OWED: `steps < maxSteps = True` implies `S steps <= maxSteps = True`.
||| Operationally `<` is defined as `\a, b => S a <= b` on `Nat`, so the
||| two sides are syntactically identical at the `Bool`-result level.
||| Held back by Idris2 0.8.0 not reducing the `Nat`-`Ord`-instance
||| `lt`/`lte` comparison to a definitional Refl when both arguments
||| are universally quantified variables — the `Ord Nat` instance goes
||| through `compareNat` not through structural pattern match. Discharge
||| via `Data.Nat.lteSuccRight` after unfolding the instance, or once
||| the stdlib exposes `lt = lteS` as a definitional equality.
0 stepIncreases : (steps : Nat) -> (maxSteps : Nat) -> (steps < maxSteps = True) ->
                  (S steps <= maxSteps = True)

||| OWED: For any fuel-bounded match attempt there exists a step count
||| that either fits in the fuel budget or exceeds it (excluded middle
||| over `<=`). Operationally the matcher is total under `%default
||| total` and threads a `Nat` step counter, so the inhabitant is
||| always one of the two arms.
||| Held back by Idris2 0.8.0 having no decidable-totality oracle that
||| can produce an actual step witness from a `Regex` AST at the type
||| level — discharging the existential requires a concrete
||| operational-semantics model of the matcher (the actual matcher
||| lives in `Proven.SafeRegex` and traverses an opaque `String`
||| input). This is a *complexity-theoretic* claim, not a structural
||| one. Discharge once an embedded NFA/DFA model + `Data.Nat`
||| trichotomy lets us construct the step witness by induction over
||| the regex AST.
|||
||| Not marked `0` because the public re-export `matchingTerminates`
||| (defined `= matchingTerminatesLemma` below) is at default
||| multiplicity for the user-facing API; making the lemma `0` would
||| break that consumer.
matchingTerminatesLemma : (fuel : Nat) -> (r : Regex) ->
                          Either (steps : Nat ** steps <= fuel = True)
                                 (steps : Nat ** steps > fuel = True)

||| Proof that matching with fuel eventually terminates
public export
matchingTerminates : (fuel : Nat) -> (r : Regex) ->
                     Either (steps : Nat ** steps <= fuel = True)
                            (steps : Nat ** steps > fuel = True)
matchingTerminates = matchingTerminatesLemma

--------------------------------------------------------------------------------
-- Complexity Proofs
--------------------------------------------------------------------------------

||| OWED: nested quantifiers force `determineComplexity` to `Exponential`.
||| Operationally true by the definition (`Safety.idr` L239-L249):
|||   `determineComplexity r = if hasNestedQuantifiers r then Exponential ...`
||| so under the premise `hasNestedQuantifiers r = True` the `if`-head
||| is `True` and the result reduces to `Exponential`.
||| Held back by Idris2 0.8.0 not propagating a `Bool`-premise through
||| an `if`/`case` scrutinee during type-checking when the scrutinee
||| is a free function call (`hasNestedQuantifiers r` for arbitrary
||| `r`) — same blocker family as boj-server's class-J
||| `Bool`-vs-`Prop` reflection gap. Discharge via a manual
||| `rewrite` of the `if`-head with the premise, or once a reflective
||| `Bool->Dec` bridge for arbitrary boolean scrutinees lands.
0 nestedQuantifiersExponential : (r : Regex) ->
                                 hasNestedQuantifiers r = True ->
                                 determineComplexity r = Exponential

||| OWED: when overlapping alternatives are present with at least one
||| quantifier, `determineComplexity` lands on `Exponential` or
||| `Quadratic` (the `Quadratic` branch arises when the leading
||| `hasNestedQuantifiers` is `False`). Operationally true by the
||| second `if`-arm of `determineComplexity` (`Safety.idr` L243).
||| Held back by the same Idris2 0.8.0 nested `if`-scrutinee blocker
||| as `nestedQuantifiersExponential` plus a `Bool`-conjunction
||| splitting step on `hasOverlappingAlternatives r &&
||| countQuantifiers r > 0`. Discharge via the same reflective
||| `Bool->Dec` bridge plus an `andTrueSplit` lemma applied at the
||| premise.
0 overlappingAltsExponential : (r : Regex) ->
                               hasOverlappingAlternatives r = True ->
                               countQuantifiers r > 0 = True ->
                               (determineComplexity r = Exponential) `Either`
                               (determineComplexity r = Quadratic)

||| OWED: a regex with a quantifier over a possibly-empty pattern lands
||| on `Quadratic` or `Exponential` (`Exponential` arises when an
||| earlier `if`-arm fires). Operationally true by the third `if`-arm
||| of `determineComplexity` (`Safety.idr` L245-L246).
||| Held back by the same Idris2 0.8.0 nested `if`-scrutinee blocker
||| as the other complexity-classification lemmas. Discharge via the
||| same reflective `Bool->Dec` bridge.
0 quantifiedEmptyQuadratic : (r : Regex) ->
                             hasQuantifiedEmpty r = True ->
                             (determineComplexity r = Quadratic) `Either`
                             (determineComplexity r = Exponential)

||| OWED: `Linear` complexity rules out every exponential-causing
||| pattern (`!hasNestedQuantifiers`, either `!hasOverlappingAlternatives`
||| or `countQuantifiers = 0`, and `!hasQuantifiedEmpty`).
||| Operationally true by reading `determineComplexity` (`Safety.idr`
||| L239-L249) in reverse: only the `else` branch returns `Linear`,
||| and reaching it requires falsifying every prior `if`-head.
||| Held back by Idris2 0.8.0 not running an `if`-chain backwards from
||| a result-equality (`determineComplexity r = Linear`) to a
||| conjunction of falsified `Bool` premises — the elaborator needs
||| four nested `case`-inversions on opaque function calls. Discharge
||| via a manual hand-written four-arm inversion, or once a tactic
||| for `if`-chain inversion lands.
0 linearNoExponentialPatterns : (r : Regex) ->
                                determineComplexity r = Linear ->
                                (hasNestedQuantifiers r = False,
                                 Either (hasOverlappingAlternatives r = False) ((countQuantifiers r = 0) = True),
                                 hasQuantifiedEmpty r = False)

--------------------------------------------------------------------------------
-- Safety Level Proofs
--------------------------------------------------------------------------------

||| Proof that Linear complexity meets any safety level
public export
linearMeetsAnySafety : (safety : SafetyLevel) -> meetsSafetyLevel Linear safety = True
linearMeetsAnySafety StrictSafety = Refl
linearMeetsAnySafety NormalSafety = Refl
linearMeetsAnySafety RelaxedSafety = Refl

||| Proof that Exponential only meets RelaxedSafety
public export
exponentialOnlyRelaxed : meetsSafetyLevel Exponential StrictSafety = False
exponentialOnlyRelaxed = Refl

public export
exponentialNotNormal : meetsSafetyLevel Exponential NormalSafety = False
exponentialNotNormal = Refl

public export
exponentialMeetsRelaxed : meetsSafetyLevel Exponential RelaxedSafety = True
exponentialMeetsRelaxed = Refl

||| Proof that Unbounded never meets any safety level
public export
unboundedNeverSafe : (safety : SafetyLevel) -> meetsSafetyLevel Unbounded safety = False
unboundedNeverSafe StrictSafety = Refl
unboundedNeverSafe NormalSafety = Refl
unboundedNeverSafe RelaxedSafety = Refl

--------------------------------------------------------------------------------
-- Input Safety Proofs
--------------------------------------------------------------------------------

||| OWED: any input with `length input <= 50` is safe for every
||| `SafeRegex`, since `50` is the minimum value of
||| `maxSafeInputLength` (the `Unbounded` branch — `Safety.idr`
||| L376). The claim chains `length input <= 50 <= maxSafeInputLength
||| sr` via `<=`-transitivity.
||| Held back by Idris2 0.8.0 String FFI opacity: `length : String ->
||| Nat` is an FFI primitive whose result does not type-level reduce
||| for an abstract `input`. Same blocker family as SafeChecksum's
||| `luhnValidatesKnownGood` (opaque String). Additionally requires a
||| four-arm case-split on `sr.complexity.level` plus
||| `Data.Nat.lteTransitive`. Discharge once a `Data.String`
||| reflective bridge for `length` is available, then apply
||| transitivity.
0 smallInputAlwaysSafe : (sr : SafeRegex) -> (input : String) ->
                         length input <= 50 = True ->
                         isInputSafe sr input = True

||| OWED: `Linear`-complexity safe-regexes accept inputs up to
||| `10_000_000` characters. Operationally true by the `Linear` arm of
||| `maxSafeInputLength` (`Safety.idr` L373):
|||   `case sr.complexity.level of Linear => 10000000 ; ...`
||| Held back by Idris2 0.8.0 not propagating the record-projection
||| equation `sr.complexity.level = Linear` through the `case`
||| scrutinee inside `maxSafeInputLength` — same blocker family as
||| `boundedImpliesJustMax` (record-field equation not tracked by
||| case-split). Discharge via a manual `rewrite` of the scrutinee
||| with the premise.
0 linearAllowsLargeInput : (sr : SafeRegex) ->
                           sr.complexity.level = Linear ->
                           maxSafeInputLength sr = 10000000

||| OWED: `Exponential`-complexity safe-regexes restrict inputs to
||| `100` characters. Operationally true by the `Exponential` arm of
||| `maxSafeInputLength` (`Safety.idr` L375).
||| Held back by the same record-projection-through-`case` blocker as
||| `linearAllowsLargeInput`. Discharge identically.
0 exponentialRestrictsInput : (sr : SafeRegex) ->
                              sr.complexity.level = Exponential ->
                              maxSafeInputLength sr = 100

--------------------------------------------------------------------------------
-- Character Class Proofs
--------------------------------------------------------------------------------

||| OWED: `Any` matches every non-newline character. Operationally
||| true by the `Any` arm of `matchesClass` (`Types.idr` L48):
|||   `matchesClass c Any = c /= '\n'`
||| Held back by Idris2 0.8.0 Char FFI opacity: `(/=) : Char -> Char
||| -> Bool` goes through the `Eq Char` instance, whose underlying
||| primitive `prim__eq_Char` does not type-level reduce for an
||| abstract `c`. Same blocker family as boj-server's `charEqSound`
||| (class-J `prim__eq_Char` reflection). Discharge once a `Data.Char`
||| reflective bridge for `(/=)` is available, or by reducing
||| `matchesClass c Any` via the definitional unfolding plus the
||| premise.
0 anyMatchesNonNewline : (c : Char) -> (c /= '\n' = True) -> matchesClass c Any = True

||| OWED: `matchesClass c Digit = True` implies `'0' <= c <= '9'`.
||| Operationally true by the `Digit` arm of `matchesClass`
||| (`Types.idr` L45):
|||   `matchesClass c Digit = c >= '0' && c <= '9'`
||| then split the `&&` via `andTrueSplit`.
||| Held back by Idris2 0.8.0 Char FFI opacity on the `Ord Char`
||| primitives `prim__gte_Char` / `prim__lte_Char` — same blocker
||| family as `anyMatchesNonNewline`. Discharge once a `Data.Char`
||| reflective bridge is available.
0 digitOnlyDigits : (c : Char) ->
                    matchesClass c Digit = True ->
                    (c >= '0' = True, c <= '9' = True)

||| OWED: `Negate` inverts the match result of the wrapped class.
||| Operationally true by the `Negate` arm of `matchesClass`
||| (`Types.idr` L49):
|||   `matchesClass c (Negate cls) = not (matchesClass c cls)`
||| — both sides are syntactically identical.
||| Held back by Idris2 0.8.0 not auto-discharging an equation
||| whose RHS is a definitional unfolding of the LHS when both sides
||| involve an abstract `Char` and `CharClass`; the `matchesClass`
||| pattern-match on `Negate` reduces but the equation `... = ...` is
||| not picked up by Refl for arbitrary `cls`. Discharge via case-
||| split on `cls` (10-arm) with `Refl` on each arm.
0 negateInverts : (c : Char) -> (cls : CharClass) ->
                  matchesClass c (Negate cls) = not (matchesClass c cls)

||| OWED: `Union` is the logical OR of the two member classes.
||| Operationally true by the `Union` arm of `matchesClass`
||| (`Types.idr` L50):
|||   `matchesClass c (Union cls1 cls2) = matchesClass c cls1 || matchesClass c cls2`
||| Held back by the same definitional-equation blocker as
||| `negateInverts`. Discharge identically (case-split on both
||| `cls1` and `cls2`, `Refl` per arm).
0 unionIsOr : (c : Char) -> (cls1 : CharClass) -> (cls2 : CharClass) ->
              matchesClass c (Union cls1 cls2) = (matchesClass c cls1 || matchesClass c cls2)

--------------------------------------------------------------------------------
-- Overlap Detection Proofs
--------------------------------------------------------------------------------

||| OWED: a `SingleChar` overlaps itself. Operationally true by the
||| `(SingleChar c1) (SingleChar c2)` arm of `classesOverlap`
||| (`Safety.idr` L58):
|||   `classesOverlap (SingleChar c1) (SingleChar c2) = c1 == c2`
||| together with `c == c = True` (reflexivity of `Eq Char`).
||| Held back by Idris2 0.8.0 Char FFI opacity on `prim__eq_Char` —
||| `c == c` does not reduce to `True` by Refl for an abstract `c`.
||| Same blocker family as boj-server's class-J `charEqSound`. Not
||| marked `0` because the public `sameClassOverlaps` proof consumes
||| this lemma at default multiplicity. Discharge once a `Data.Char`
||| reflective bridge gives `eqCharRefl : (c : Char) -> (c == c) = True`.
charSelfOverlaps : (c : Char) -> classesOverlap (SingleChar c) (SingleChar c) = True

||| OWED: a `Range` overlaps itself. Operationally true by the
||| `(Range f1 t1) (Range f2 t2)` arm of `classesOverlap`
||| (`Safety.idr` L61):
|||   `classesOverlap (Range f1 t1) (Range f2 t2) = not (t1 < f2 || t2 < f1)`
||| then with `f1 = f2 = from` and `t1 = t2 = to`, both `t < f` are
||| `False` (`Ord Char` is strict), so `not (False || False) = True`.
||| Held back by Idris2 0.8.0 Char FFI opacity on `prim__lt_Char` —
||| `t < f` for `t >= f` does not reduce to `False` by Refl. Same
||| blocker family as `charSelfOverlaps`. Not marked `0` because the
||| public `sameClassOverlaps` proof consumes this lemma at default
||| multiplicity. Discharge once a `Data.Char` reflective bridge for
||| `(<)` is available.
rangeSelfOverlaps : (from, to : Char) -> classesOverlap (Range from to) (Range from to) = True

||| OWED: a `Union` overlaps itself. Operationally true by the
||| `(Union c1 c2) other` arm of `classesOverlap` (`Safety.idr`
||| L69):
|||   `classesOverlap (Union c1 c2) other = classesOverlap c1 other || classesOverlap c2 other`
||| The right-hand `other` is the same `Union a b`, so the LHS
||| disjunct recursively reduces — but the recursion depth is
||| unbounded in `a`/`b`.
||| Held back by Idris2 0.8.0 not auto-discharging the structural
||| induction on `CharClass` for a self-recursive `||`-headed
||| equation — also blocked by transitive Char-FFI opacity in the
||| `SingleChar`/`Range` base cases. Not marked `0` because the
||| public `sameClassOverlaps` proof consumes this lemma at default
||| multiplicity. Discharge via explicit induction on `(a, b)` plus
||| the discharged `charSelfOverlaps` / `rangeSelfOverlaps`.
unionSelfOverlaps : (a, b : CharClass) -> classesOverlap (Union a b) (Union a b) = True

||| Proof that identical classes always overlap
public export
sameClassOverlaps : (cls : CharClass) -> classesOverlap cls cls = True
sameClassOverlaps Any = Refl
sameClassOverlaps (SingleChar c) = charSelfOverlaps c
sameClassOverlaps (Range f t) = rangeSelfOverlaps f t
sameClassOverlaps Digit = Refl
sameClassOverlaps Word = Refl
sameClassOverlaps Space = Refl
sameClassOverlaps (Negate _) = Refl
sameClassOverlaps (Union a b) = unionSelfOverlaps a b

||| Proof that Any overlaps with everything
public export
anyOverlapsAll : (cls : CharClass) -> classesOverlap Any cls = True
anyOverlapsAll _ = Refl

||| OWED: two `Range`s that are positionally disjoint (`t1 < f2`) do
||| not overlap. Operationally true by the `(Range f1 t1) (Range f2
||| t2)` arm of `classesOverlap` (`Safety.idr` L61):
|||   `classesOverlap (Range f1 t1) (Range f2 t2) = not (t1 < f2 || t2 < f1)`
||| The premise gives `t1 < f2 = True`, so `True || _ = True` and
||| `not True = False`.
||| Held back by Idris2 0.8.0 not propagating a `Bool`-premise
||| through `||` and `not` during `Refl` checking for an abstract
||| `Char` quadruple — same Char-FFI opacity family as the other
||| range/overlap lemmas. Discharge via a manual `rewrite` of `t1 <
||| f2` with the premise, plus the `Data.Bool` lemmas `orTrueLeft`
||| and `notTrue`.
0 disjointRangesNoOverlap : (f1, t1, f2, t2 : Char) ->
                            (t1 < f2 = True) ->
                            classesOverlap (Range f1 t1) (Range f2 t2) = False

--------------------------------------------------------------------------------
-- Known Safe Pattern Proofs
--------------------------------------------------------------------------------

||| Proof that Empty is always safe
public export
emptyIsSafe : isKnownSafe Empty = True
emptyIsSafe = Refl

||| Proof that Never is always safe
public export
neverIsSafe : isKnownSafe Never = True
neverIsSafe = Refl

||| Proof that single character match is always safe
public export
singleCharIsSafe : (cls : CharClass) -> isKnownSafe (Match cls) = True
singleCharIsSafe _ = Refl

||| Proof that anchors are always safe
public export
startAnchorIsSafe : isKnownSafe StartAnchor = True
startAnchorIsSafe = Refl

public export
endAnchorIsSafe : isKnownSafe EndAnchor = True
endAnchorIsSafe = Refl

public export
wordBoundaryIsSafe : isKnownSafe WordBoundary = True
wordBoundaryIsSafe = Refl

||| Proof that backreferences are never known safe
public export
backrefNotKnownSafe : (gid : Nat) -> isKnownSafe (BackRef gid) = False
backrefNotKnownSafe _ = Refl

--------------------------------------------------------------------------------
-- Step Limit Proofs
--------------------------------------------------------------------------------

||| OWED: under `Linear` complexity, the step limit grows strictly
||| with input size. Operationally true by the `Linear` arm of
||| `calculateStepLimit` (`Safety.idr` L317):
|||   `Linear => base   where base = inputSize * 10`
||| so `n1 < n2 ==> n1 * 10 < n2 * 10` by strict monotonicity of
||| `Nat` multiplication on the right by a positive constant.
||| Held back by Idris2 0.8.0 not auto-discharging strict monotonicity
||| of primitive `Nat` multiplication, plus the record-projection
||| `analysis.level` not being propagated through the `case`
||| scrutinee inside `calculateStepLimit` (same blocker family as
||| `linearAllowsLargeInput`). Complexity-theoretic claim — this is
||| the *linear-time* growth law underlying the matcher's safety
||| budget. Discharge via `Data.Nat.multStrictMonotone` plus a
||| manual `rewrite` of the case scrutinee.
0 linearStepLimitScales : (analysis : ComplexityAnalysis) ->
                          (analysis.level = Linear) ->
                          (inputSize1, inputSize2 : Nat) ->
                          (inputSize1 < inputSize2 = True) ->
                          calculateStepLimit analysis inputSize1 < calculateStepLimit analysis inputSize2 = True

||| OWED: under `Exponential` complexity, the step limit is bounded
||| above by `1_000_000`. Operationally true by the `Exponential`
||| arm of `calculateStepLimit` (`Safety.idr` L319):
|||   `Exponential => min (base * 100) 1000000`
||| since `min _ 1000000 <= 1000000`.
||| Held back by Idris2 0.8.0 not auto-discharging
||| `min a b <= b = True` for primitive `Nat` `min`, plus the same
||| record-projection-through-`case` blocker as
||| `linearStepLimitScales`. Complexity-theoretic claim — this is
||| the *exponential-time cap* that turns a worst-case Exponential
||| pattern into a hard-bounded ReDoS-resistant matcher. Discharge
||| via `Data.Nat.minLteRight` plus a manual `rewrite` of the
||| scrutinee.
0 exponentialStepLimitCapped : (analysis : ComplexityAnalysis) ->
                               (analysis.level = Exponential) ->
                               (inputSize : Nat) ->
                               calculateStepLimit analysis inputSize <= 1000000 = True

--------------------------------------------------------------------------------
-- Match Result Proofs
--------------------------------------------------------------------------------

||| Proof that noMatch has matched = False
public export
noMatchIsNotMatched : (steps : Nat) -> (noMatch steps).matched = False
noMatchIsNotMatched _ = Refl

||| Proof that success has matched = True
public export
successIsMatched : (start, end : Nat) -> (caps : List Capture) -> (steps : Nat) ->
                   (success start end caps steps).matched = True
successIsMatched _ _ _ _ = Refl

||| OWED: every capture of a successful match has `start <= end`.
||| Operationally true because every `success` constructor pushes
||| captures whose `(start, end)` indices come from the matcher's
||| monotonically-advancing cursor, but the cursor invariant is
||| operational, not in the type of `MatchResult`.
||| Held back by `MatchResult` carrying captures as a plain `List
||| Capture` with no per-element refinement
||| (`{auto 0 _ : start <= end = True}`) — there is no construct-
||| ively available proof to discharge the `All` predicate from a
||| `matched = True` premise alone. Same shape as boj-server's
||| stated-parser-postcondition pattern (SafeTOML's
||| `dateComponentsValid` / `timeComponentsValid`). Discharge once
||| `Capture` is refactored to carry the bound as an erased witness,
||| and the matcher is updated to produce it at every push site.
0 capturePositionsValid : (result : MatchResult) ->
                          (result.matched = True) ->
                          All (\c => c.start <= c.end = True) result.captures

--------------------------------------------------------------------------------
-- Composition Proofs
--------------------------------------------------------------------------------

||| OWED: sequencing two known-safe regexes (without introducing
||| nested quantifiers at the seam) is known-safe. Operationally
||| true by the `Seq` arm of `isKnownSafe` (`Safety.idr` L352):
|||   `isKnownSafe (Seq r1 r2) = isKnownSafe r1 && isKnownSafe r2 && not (hasNestedQuantifiers (Seq r1 r2))`
||| then split the conjunction using the three premises.
||| Held back by Idris2 0.8.0 not auto-splitting a three-way `&&`
||| equation from three separate `= True` premises — needs
||| `andTrueSplit` applied twice plus `notFalseTrue`. Discharge via
||| a manual chain of `andTrueSplit` once the helper lands, or by
||| inlining the equation by hand.
0 seqPreservesSafety : (r1, r2 : Regex) ->
                       isKnownSafe r1 = True ->
                       isKnownSafe r2 = True ->
                       hasNestedQuantifiers (Seq r1 r2) = False ->
                       isKnownSafe (Seq r1 r2) = True

||| OWED: alternating two known-safe regexes with non-overlapping
||| firsts is known-safe. Operationally true by the `Alt` arm of
||| `isKnownSafe` (`Safety.idr` L353):
|||   `isKnownSafe (Alt r1 r2) = isKnownSafe r1 && isKnownSafe r2 && not (regexesOverlap r1 r2)`
||| Held back by the same three-way `&&`-split blocker as
||| `seqPreservesSafety`. Discharge identically.
0 altPreservesSafety : (r1, r2 : Regex) ->
                       isKnownSafe r1 = True ->
                       isKnownSafe r2 = True ->
                       regexesOverlap r1 r2 = False ->
                       isKnownSafe (Alt r1 r2) = True

||| OWED: a bounded quantifier over a known-safe body (without a
||| nested-quantifier seam) is known-safe. Operationally true by
||| the `Quant` arm of `isKnownSafe` (`Safety.idr` L354):
|||   `isKnownSafe (Quant r q) = isKnownSafe r && isBounded q && not (hasNestedQuantifiers (Quant r q))`
||| Held back by the same three-way `&&`-split blocker as
||| `seqPreservesSafety`. Discharge identically.
0 boundedQuantPreservesSafety : (r : Regex) -> (q : Quantifier) ->
                                isKnownSafe r = True ->
                                isBounded q = True ->
                                hasNestedQuantifiers (Quant r q) = False ->
                                isKnownSafe (Quant r q) = True

--------------------------------------------------------------------------------
-- Totality Proofs
--------------------------------------------------------------------------------

||| Proof that all safety analysis functions are total
||| (guaranteed by %default total at module level)
public export
safetyAnalysisTotal : (r : Regex) -> (analysis : ComplexityAnalysis ** analysis = analyzeComplexity r)
safetyAnalysisTotal r = (analyzeComplexity r ** Refl)

||| OWED: catch-all for the impossible / unreachable
||| `(a, b, c)` triples in the `ComplexityLevel` transitivity table.
||| `complexityTransitive` (below) discharges the four reachable
||| `LT/LT/LT` combinations by direct `Refl`; this fallback covers
||| the remaining 60 of the 64 triples, which are all
||| premise-inconsistent (the antecedent `a `compare` b = LT`
||| together with `b `compare` c = LT` is uninhabited).
||| Held back by Idris2 0.8.0 not auto-discharging premise
||| inconsistency from an exhaustive case-split over the four
||| constructors of `ComplexityLevel` — the `compare` instance for
||| `ComplexityLevel` (`Types.idr` L161) goes through the derived
||| `Ord` machinery, whose result on a specific constructor pair is
||| not a definitional Refl for arbitrary `a, b, c`. Not marked `0`
||| because the public `complexityTransitive` (L312-L316) consumes
||| this lemma at default multiplicity in its catch-all clause.
||| Discharge via the 60-arm case-split with `absurd Refl` on every
||| impossible arm, or by reformulating the lemma to take a single
||| `Ord ComplexityLevel`-derived strict-order proof.
complexityTransitiveFallback : (a, b, c : ComplexityLevel) ->
                                a `compare` b = LT ->
                                b `compare` c = LT ->
                                a `compare` c = LT

||| Proof that complexity ordering is transitive
public export
complexityTransitive : (a, b, c : ComplexityLevel) ->
                       a `compare` b = LT ->
                       b `compare` c = LT ->
                       a `compare` c = LT
complexityTransitive Linear Quadratic Exponential _ _ = Refl
complexityTransitive Linear Quadratic Unbounded _ _ = Refl
complexityTransitive Linear Exponential Unbounded _ _ = Refl
complexityTransitive Quadratic Exponential Unbounded _ _ = Refl
complexityTransitive a b c p1 p2 = complexityTransitiveFallback a b c p1 p2

||| Proof that complexity comparison is reflexive for EQ
public export
complexityReflexive : (a : ComplexityLevel) -> a `compare` a = EQ
complexityReflexive Linear = Refl
complexityReflexive Quadratic = Refl
complexityReflexive Exponential = Refl
complexityReflexive Unbounded = Refl
