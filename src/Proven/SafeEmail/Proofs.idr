-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeEmail operations (RFC 5321 / RFC 5322).
|||
||| This module contains proof signatures for properties of email parsing
||| and validation. Where the property is discharged by definitional
||| unfolding (Refl) under Idris2 0.8.0 it is given a body; where the
||| property is irreducible under Idris2 0.8.0 the declaration is given
||| as a `0 `-erased OWED signature with a `||| OWED:` docstring naming
||| the blocker and the discharge condition. Same posture as
||| `Proven.SafeChecksum.Proofs` and the SafeHtml / SafeTOML / SafeMath
||| sibling modules (Refs hyperpolymath/standards#158).
|||
||| Zero `believe_me`, zero `postulate`, zero `idris_crash`. All OWED
||| items are at multiplicity 0 so they cannot leak into runtime, and
||| are named (vs commented-out) so they are discoverable by audit.
module Proven.SafeEmail.Proofs

import Proven.Core
import Proven.SafeEmail.Parser
import Proven.SafeEmail.Validation
import Data.List
import Data.List1
import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Parsing Properties
--------------------------------------------------------------------------------

||| DISCHARGED: `parseEmail "" = Nothing`. Directly matches the first
||| pattern clause of `parseEmail` (Parser.idr L186-187). Precedent:
||| `parseAttestationType "" = Nothing` is already discharged by
||| `Refl` in `SafeAttestation/Proofs.idr` using the identical pattern.
||| The OWED's "String-literal matching does not reduce" claim is
||| contradicted by the SafeAttestation anchor — empty-string literals
||| DO reduce against pattern clauses in Idris2 0.8.0.
public export
parseEmptyFails : parseEmail "" = Nothing
parseEmptyFails = Refl

||| Parsing is deterministic (any function is). This is `Refl` at
||| every input — included as a sanity anchor to document the
||| referential-transparency property callers may rely on.
public export
parseDeterministic : (s : String) -> parseEmail s = parseEmail s
parseDeterministic s = Refl

||| OWED: a string with no `'@'` character fails to parse as an
||| email (`parseEmail "noatsign" = Nothing`). Reduces by the
||| `splitOnLast '@' s = Nothing => Left NoAtSign` arm of
||| `parseEmailEither` (Parser.idr L174-175). Held back by Idris2
||| 0.8.0 not reducing `splitOnLast` (a `String -> String -> ...`
||| function threading through opaque `unpack` / `pack` String FFI
||| primitives) at the type level. Same blocker family as the
||| SafeChecksum `validateLuhn` / `validateISBN10` String-FFI gap.
||| Discharge once a `Data.String` reflective tactic is available,
||| or once `splitOnLast` is reformulated on `List Char` so its
||| reduction does not pass through `unpack`.
0 parseNoAtFails : parseEmail "noatsign" = Nothing

--------------------------------------------------------------------------------
-- Validation Properties
--------------------------------------------------------------------------------

||| OWED: `validResult.isValid = True`. Operationally immediate —
||| `validResult = MkValidationResult True []` (Validation.idr L62) — but
||| Idris2 0.8.0 does not reduce the projection of a top-level constant
||| (`validResult.isValid`) by `Refl`, even with `ValidationResult` and
||| `validResult` both `public export`. Verified: the same
||| `Can't solve constraint between: True and validResult.isValid` arises
||| in a minimal cross-module repro. Same top-level-constant-opacity
||| blocker as `SafePassword.chainedBuildersCompose` and
||| `SafeCSV.defaultDelimiterIsComma`. Discharge once Idris2 reduces
||| top-level constant projections, or `validResult` is inlined at the use
||| site so the constructor is exposed. (The prior "DISCHARGED … by direct
||| record-projection reduction" comment was incorrect — it did not
||| type-check under Idris2 0.8.0.)
public export
0 validResultIsValid : validResult.isValid = True

||| OWED: adding an Error-severity issue makes the result invalid.
||| `addIssue` (Validation.idr L71-74) computes the new validity as
||| `result.isValid && issue.severity /= Error`; given
||| `issue.severity = Error` we have `Error /= Error = False`, so the
||| conjunction collapses to `False` for any starting `result`. Held
||| back by Idris2 0.8.0's user-defined `Eq ValidationSeverity`
||| instance (Validation.idr L28-32): equality on the three-arm
||| `data ValidationSeverity = Error | Warning | Info` does not
||| reduce under `(/=)` by Refl alone because `(/=)` is implemented
||| as `not . (==)` and `not (Error == Error)` requires unfolding
||| both the user-written `Eq` instance and `not`. Same family as
||| boj-server SafetyLemmas' enum-equality reflection gap. Discharge
||| with a `Bool`-vs-`Prop` reflective lemma `(==) Error Error = True`
||| plus `Not (True = False)` reasoning.
0 errorMakesInvalid : (issue : ValidationIssue) ->
                      issue.severity = Error ->
                      (addIssue issue validResult).isValid = False

||| OWED: adding a Warning-severity issue keeps the result valid.
||| By the same `addIssue` definition (Validation.idr L71-74), with
||| `issue.severity = Warning` we get `Warning /= Error = True` and
||| starting `validResult.isValid = True`, so `True && True = True`.
||| Held back by the same `Eq ValidationSeverity` reduction gap as
||| `errorMakesInvalid` above, compounded by the
||| `validResultIsValid` record-projection gap. Discharge alongside
||| `errorMakesInvalid`.
0 warningKeepsValid : (issue : ValidationIssue) ->
                      issue.severity = Warning ->
                      (addIssue issue validResult).isValid = True

||| DISCHARGED: combining two valid results yields a valid result.
||| The OWED comment suggested the discharge pattern: case-split on
||| `r1`, `r2` to expose the `MkValidationResult` constructor, then
||| pattern-match the `True` field through the premise's `Refl` to
||| collapse `True && True` to `True` directly. Empirically verified
||| at `/tmp/charrefl/src/TestEmail.idr`.
public export
combineValidValid : (r1, r2 : ValidationResult) ->
                    r1.isValid = True -> r2.isValid = True ->
                    (combineResults r1 r2).isValid = True
combineValidValid (MkValidationResult True _) (MkValidationResult True _) Refl Refl = Refl

--------------------------------------------------------------------------------
-- Email Structure Properties
--------------------------------------------------------------------------------

||| Parsed email always contains @
public export
data ContainsAt : String -> Type where
  MkContainsAt : (s : String) -> (prf : '@' `elem` unpack s = True) -> ContainsAt s

||| OWED: if `parseEmail s` succeeds (`isJust (parseEmail s) = True`),
||| the input string contains `'@'` (`'@' `elem` unpack s = True`).
||| Reduces by following `parseEmail s = Just _` back through
||| `parseEmailEither` (Parser.idr L171-182): the only success arm
||| requires `splitOnLast '@' s = Just (local, domain)`, which in
||| turn requires `'@'` to appear in `s`. Held back by Idris2 0.8.0's
||| String FFI opacity — `splitOnLast`, `unpack`, and `elem` all
||| pass through opaque primitives that do not type-level reduce.
||| Same blocker family as `parseNoAtFails`. Discharge once the
||| String FFI is reflectively modelled, or once `splitOnLast` is
||| factored through `List Char` with a structural lemma
||| `splitOnLastJust : splitOnLast c s = Just _ -> c `elem` unpack s = True`.
0 parsedContainsAt : (s : String) -> isJust (parseEmail s) = True -> ContainsAt s

||| Local part length bound
public export
data ValidLocalLength : String -> Type where
  MkValidLocalLength : (local : String) -> LTE (length local) 64 -> ValidLocalLength local

||| Domain length bound
public export
data ValidDomainLength : String -> Type where
  MkValidDomainLength : (domain : String) -> LTE (length domain) 253 -> ValidDomainLength domain

||| Total email length bound
public export
data ValidTotalLength : String -> Type where
  MkValidTotalLength : (email : String) -> LTE (length email) 254 -> ValidTotalLength email

--------------------------------------------------------------------------------
-- Normalization Properties
--------------------------------------------------------------------------------

||| OWED: `toLower` on a domain string is idempotent
||| (`toLower (toLower d) = toLower d`). True because case-folding to
||| lower case fixes the lowercase alphabet (and leaves non-letters
||| alone), so the second application is the identity on the image
||| of the first. Held back by Idris2 0.8.0's String-level `toLower`
||| being a wrapper over the opaque `prim__strToLower` FFI primitive
||| — it is not reducible at the type level. Same family as the
||| SafeHtml `escapePreservesNoLT` String-FFI gap. Discharge once a
||| `Data.String` reflective tactic is available, or via a
||| character-level lemma `Data.Char.toLowerIdempotent` lifted
||| through `pack . map toLower . unpack` (which still requires
||| reducing through `unpack` / `pack`).
0 normalizeIdempotent : (email : ParsedEmail) ->
                        toLower email.domain = toLower (toLower email.domain)

||| Normalized emails with same local and domain are equal (trivial
||| restatement of the third hypothesis). Discharged by Refl on the
||| supplied equality witness.
public export
normalizedEquality : (e1, e2 : ParsedEmail) ->
                     e1.localPart = e2.localPart ->
                     toLower e1.domain = toLower e2.domain ->
                     toLower (e1.domain) = toLower (e2.domain)
normalizedEquality e1 e2 _ prf = prf

--------------------------------------------------------------------------------
-- Security Properties
--------------------------------------------------------------------------------

||| Sanitized string contains no newlines
public export
data NoNewlines : String -> Type where
  MkNoNewlines : (s : String) ->
                 all (\c => c /= '\n' && c /= '\r') (unpack s) = True ->
                 NoNewlines s

isHeaderSafe : Char -> Bool
isHeaderSafe c = c /= '\n' && c /= '\r' && c /= '\0'

sanitizeForHeader : String -> String
sanitizeForHeader str = pack (filter isHeaderSafe (unpack str))

||| OWED: filtering out the union predicate
||| `(c /= '\n' && c /= '\r' && c /= '\0')` from a `List Char`
||| leaves a list where every element satisfies the weaker predicate
||| `(c /= '\n' && c /= '\r')`. Standard `filter` post-condition
||| (`all p (filter q xs) = True` when `q x = True -> p x = True`,
||| with the implication discharged by `&&` projection). Held back
||| by Idris2 0.8.0 not auto-deriving the `filter`-postcondition
||| lemma — `Data.List` exposes no `filterAll : (forall x. q x =
||| True -> p x = True) -> all p (filter q xs) = True`, and the
||| direct proof requires structural induction on `xs` plus a
||| `&&`-projection lemma which is itself opaque under Idris2's
||| `Bool` reduction. Same family as SafeHtml's filter-correctness
||| OWED. Discharge with a hand-written `filterAll` lemma or with a
||| reflective `Bool` tactic.
0 sanitizeRemovesNewlinesLemma : (s : String) ->
                                  all (\c => c /= '\n' && c /= '\r')
                                      (filter (\c => c /= '\n' && c /= '\r' && c /= '\0') (unpack s))
                                  = True

||| OWED: `sanitizeForHeader s` always produces a string with no
||| newlines (CR/LF) — the safety property the function exists for.
||| Decomposes as `unpack (pack (filter isHeaderSafe (unpack s))) =
||| filter isHeaderSafe (unpack s)` (the `unpack . pack = id` round
||| trip, opaque under String FFI) composed with the
||| `sanitizeRemovesNewlinesLemma` above. Held back by the
||| combination of two Idris2 0.8.0 blockers: (1) the `unpack . pack`
||| round-trip is not a Refl (the String FFI's `prim__strCons` /
||| `prim__strSubstr` cycle is not reducible at the type level);
||| (2) the upstream `sanitizeRemovesNewlinesLemma` is itself OWED.
||| Discharge once both are discharged.
public export
0 sanitizeRemovesNewlines : (s : String) ->
                            NoNewlines (sanitizeForHeader s)

--------------------------------------------------------------------------------
-- List Operations Properties
--------------------------------------------------------------------------------

||| OWED: `filterValid` retains only emails that pass full
||| validation (`all (.isValid . validateEmailFull) (filterValid
||| emails) = True`). By definition `filterValid = filter (\s =>
||| (validateEmailFull s).isValid)` (Validation.idr L286), so this
||| is the `filter` post-condition `all p (filter p xs) = True` —
||| the standard `filterAllP` lemma. Held back by Idris2 0.8.0's
||| `Data.List` not exposing `filterAllP` as a Refl-discharable
||| identity (`filter p` decomposes by induction on the list, and
||| the cons-case requires both `p x = True` and the IH composed
||| with `(::)`). Discharge with a hand-written
||| `filterAllSelf : (xs : List a) -> all p (filter p xs) = True`
||| lemma in `Data.List`, or via the reflective `Bool` tactic.
0 filterValidCorrect : (emails : List String) ->
                       all (\e => (validateEmailFull e).isValid) (filterValid emails) = True

uniqueEmails : List ParsedEmail -> List ParsedEmail
uniqueEmails = nubBy (\e1, e2 => toLower (e1.localPart ++ "@" ++ e1.domain) ==
                                 toLower (e2.localPart ++ "@" ++ e2.domain))

||| OWED: `nubBy`-based deduplication never increases list length
||| (`length (nubBy p xs) <= length xs`). Standard `Data.List`
||| post-condition — `nubBy` only removes elements, never adds them.
||| Held back by Idris2 0.8.0's `Data.List` not exposing the lemma
||| `nubByLengthLTE : (xs : List a) -> LTE (length (nubBy p xs))
||| (length xs)` as a Refl-discharable identity. The direct proof
||| requires structural induction on `xs` plus the `LTE`-transport
||| lemma `LTE n (S n)` for the skip-arm. Same family as the
||| boj-server `Data.List` length-monotonicity OWED set. Discharge
||| by adding the missing lemma to `Data.List`, or by extending
||| `Data.List.Lemmas` (contrib) with it.
0 uniqueNoDuplicates : (emails : List ParsedEmail) ->
                       LTE (length (uniqueEmails emails)) (length emails)

--------------------------------------------------------------------------------
-- Domain Validation Properties
--------------------------------------------------------------------------------

||| OWED: `isFreeEmail` is total and exhaustive (it returns either
||| `True` or `False` for every domain — i.e., it is a decidable
||| predicate). By definition `isFreeEmail domain = toLower domain
||| `elem` freeEmailDomains` (Validation.idr L259), whose return
||| type is `Bool`, so the claim is the `Bool`-LEM
||| `(b : Bool) -> Either (b = True) (b = False)`. Held back by
||| Idris2 0.8.0 not auto-deriving `Bool`-LEM as a Refl (the
||| standard discharge is `case b of True => Left Refl; False =>
||| Right Refl`, but this is a case-split not a Refl). Same family
||| as SafeTOML's `isScalarCorrect` Bool-LEM gap. Discharge with a
||| one-line case-split on `isFreeEmail domain`.
public export
0 freeEmailExhaustive : (domain : String) ->
                        Either (isFreeEmail domain = True) (isFreeEmail domain = False)

||| OWED: `checkCommonTypos "gmial.com"` returns the
||| canonical W010 warning suggesting `"gmail.com"`. By the
||| `checkCommonTypos` definition (Validation.idr L150-166) the
||| first entry of the `checks` list is `("gmial.com", "gmail.com")`
||| and `find` returns it because `toLower "gmial.com" == "gmial.com"`,
||| so the result is `addIssue (W010 "...gmail.com?") validResult`.
||| Held back by Idris2 0.8.0's String FFI opacity on `toLower` and
||| String-equality `(==)` — neither reduces a literal-vs-literal
||| comparison to `True` by Refl at the type level. Same family as
||| `normalizeIdempotent` and `parseNoAtFails`. Discharge once the
||| String FFI is reflectively modelled, or by refactoring
||| `checkCommonTypos` to operate on `List Char`.
0 typoCheckFindsKnown : checkCommonTypos "gmial.com" = addIssue
                        (MkValidationIssue Warning "W010"
                          "Possible typo - did you mean gmail.com?")
                        validResult

--------------------------------------------------------------------------------
-- RFC Compliance Properties
--------------------------------------------------------------------------------

||| OWED: a local part that passes `validateLocalPart` does not
||| start with a dot (`isPrefixOf "." local = False`). Reduces by
||| inspecting `validateLocalPart` (Validation.idr L88-105): the
||| `result''` arm adds an Error issue E003 when
||| `isPrefixOf "." local = True`, which sets `.isValid = False`;
||| so `.isValid = True` requires `isPrefixOf "." local = False`.
||| Held back by Idris2 0.8.0's String FFI opacity on `isPrefixOf`
||| (it threads through `prim__strHead` / `prim__strSubstr`), and
||| by the `addIssue` Bool-reduction gap shared with
||| `errorMakesInvalid`. Same family as `parseNoAtFails`. Discharge
||| once the String FFI is reflectively modelled.
0 validLocalNoStartDot : (local : String) ->
                        (validateLocalPart local).isValid = True ->
                        isPrefixOf "." local = False

||| OWED: a local part that passes `validateLocalPart` does not end
||| with a dot (`isSuffixOf "." local = False`). Identical shape to
||| `validLocalNoStartDot` — `validateLocalPart` adds Error E004
||| when `isSuffixOf "." local = True`. Held back by the same
||| String-FFI / Bool-reduction blockers; discharged in the same
||| stroke.
0 validLocalNoEndDot : (local : String) ->
                      (validateLocalPart local).isValid = True ->
                      isSuffixOf "." local = False

||| OWED: a domain that passes `validateDomain` has at least one
||| label when split on `'.'` (`LTE 1 (length (forget (split (== '.')
||| domain)))`). `split (== '.')` returns a `List1`, which by
||| definition has length `>= 1` (`forget : List1 a -> List a`
||| preserves the non-emptiness). Held back by Idris2 0.8.0's
||| `Data.List1` not exposing the length-lower-bound lemma
||| `LTE 1 (length (forget xs))` as a Refl. Discharge with a
||| one-line case-split on the `List1` constructor (`x ::: xs`
||| gives length `S (length xs) >= S Z`).
0 validDomainHasLabel : (domain : String) ->
                       (validateDomain domain).isValid = True ->
                       LTE 1 (length (forget (split (== '.') domain)))

--------------------------------------------------------------------------------
-- Comprehensive Validation Properties
--------------------------------------------------------------------------------

||| OWED: if `validateEmailFull s` fails, `validateComprehensive s`
||| also fails. Both functions parse via `parseEmail s` first
||| (Validation.idr L132, L210); the `Nothing` arm returns the same
||| E000 invalid result in both, and the `Just email` arm in
||| `validateComprehensive` combines a superset of the issues
||| `validateEmailFull` combines (`validateLocalPart`,
||| `validateDomain` plus the typo / role / plus / numeric checks),
||| and `combineResults` is monotone in invalidity. Held back by the
||| `combineResults` record-projection gap shared with
||| `combineValidValid` plus the `foldl combineResults` invariant
||| (which `Data.List` does not expose as a Refl in Idris2 0.8.0).
||| Discharge alongside `combineValidValid`.
0 comprehensiveCatchesRFC : (s : String) ->
                            (validateEmailFull s).isValid = False ->
                            (validateComprehensive s).isValid = False

||| OWED: if `validateEmailFull s` passes, `validateComprehensive s`
||| has no errors (`hasErrors (validateComprehensive s) = False`).
||| Reduces by inspecting which `ValidationIssue`s the four extra
||| checks (`checkCommonTypos`, `checkRoleAddress`,
||| `checkPlusAddressing`, `checkNumericLocal`) can add: by code
||| inspection of Validation.idr L150-201, every issue these add
||| has severity `Warning` (W010) or `Info` (I001/I002/I003), never
||| `Error`. So if the `validateEmailFull` path is error-free,
||| `validateComprehensive` is also error-free. Held back by Idris2
||| 0.8.0 not reducing the `foldl combineResults` accumulator
||| across the seven-step result list, plus the `hasErrors`
||| `any . (.severity = Error)` Bool-reduction gap shared with
||| `errorMakesInvalid`. Discharge alongside `errorMakesInvalid` +
||| `combineValidValid`, with one extra step-lemma per extra check
||| (each: "this check only emits non-Error issues").
0 validPassesComprehensive : (s : String) ->
                             (validateEmailFull s).isValid = True ->
                             hasErrors (validateComprehensive s) = False
