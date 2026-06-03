# Proof-Debt Triage — Tier A Per-Site Classification

**Last updated:** 2026-05-27
**Phase:** 2, Days 3-10 (per-site reads of the 15 Tier A modules)
**Companion docs:** [`proof-debt-triage.md`](proof-debt-triage.md) (framework), [`../PROOF-NEEDS.md`](../PROOF-NEEDS.md) (debt ledger)

This file is the per-site disposition data for every `||| OWED:` annotation in the 15 Tier A modules — produced by three parallel Explore-agent reads on 2026-05-27. Each module has a sub-table classifying each OWED site as `DISCHARGE`, `DISCHARGE-after-totality`, `PROPERTY-TEST`, or `OWED-AXIOM` per the rubric in [`proof-debt-triage.md`](proof-debt-triage.md) §2, with the blocker family (1=String FFI, 2=Char FFI, 3=Covering/totality, 4=External KDF/opaque, 5=Record-projection, 6=Integral Nat mod/div, 7=Enum Eq) and a per-site cost estimate.

**Source of truth:** live grep of `src/` (commit `33155ce` on `docs/proof-debt-triage-and-header-softening`, 2026-05-27). The Tier A coverage is **~210 of 280 OWED sites (75%)**.

---

## Aggregate totals (Tier A, ~210 sites)

| Disposition | Count | Share | Estimated Phase 3 cost |
|---|---:|---:|---|
| **DISCHARGE** (provable in Idris2 0.8.0 today) | **~61** | 28% | ~31h (~4 focused days) |
| **DISCHARGE-after-totality** (blocked on totality refactor first) | **~5** | 3% | TBD per totality ticket |
| **PROPERTY-TEST** (concrete-input spot-check in `tests/properties/`) | **~66** | 31% | ~22h (~3 focused days) |
| **OWED-AXIOM** (permanent or until-Idris2-upgrade) | **~79** | 38% | 0h discharge; ongoing ledger maintenance |

Discrepancies of ±2 against the 210 figure are real — sub-agents occasionally double-counted a meta-comment `||| OWED:` in a module header, or miscounted a row in a large table. The live grep authoritatively says 280 across the whole repo; the Tier A subset is 209–212 depending on which meta-comments you count. Per-site classifications below are the definitive data; the totals above round to the nearest five.

**Blocker family prevalence across Tier A:**

| Family | Share | Default disposition | Discharge mechanism |
|---|---:|---|---|
| 1 — String FFI (`unpack`/`pack`/`length`/`splitOn`) | ~46% | PROPERTY-TEST (global) / DISCHARGE (finite alphabet) | Spot-check fixed inputs OR n-arm exhaustive case-split |
| 2 — Char FFI (`isUpper`/`isLower`/`prim__eq_Char`) | ~14% | DISCHARGE | Exhaustive ASCII case-split |
| 3 — Covering / totality | ~5% | DISCHARGE-after-totality | Refactor function to `total`, then case-split |
| 4 — External KDF / opaque | ~10% | OWED-AXIOM | None (delegates to C/Zig/external libs) |
| 5 — Record-projection gap | ~6% | DISCHARGE | `rewrite` + `withUnfold` or inlined constructor |
| 6 — Integral Nat `mod`/`div` | ~9% | DISCHARGE | `Data.Nat.Division` lemmas |
| 7 — Enum Eq reduction | ~3% | DISCHARGE | n-arm case-split with Refl per constructor |
| Mixed / structural / other | ~7% | per-site read | various |

---

## Bin 1 — SafeRegex, SafeBase64, SafeXML, SafeString, SafeCSRF (71 OWED)

### SafeRegex / Proofs.idr (26 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| boundedImpliesJustMax | 36 | `isBounded q = True` rules out `q.maxCount = Nothing` | 5 | DISCHARGE | ~25 min | record-field equation not propagated through `isJust`; manual `rewrite` + `absurd` |
| boundedQuantifierFinite | 47 | bounded quantifier exposes concrete `Nat` upper bound | 5 | DISCHARGE | ~25 min | same record-projection blocker; discharge with prior lemma |
| stepIncreases | 58 | `<` is defined as `S a <= b` on `Nat` so sides syntactically identical | 6 | DISCHARGE | ~15 min | `Ord Nat` instance goes through `compareNat`; use `Data.Nat.lteSuccRight` |
| matchingTerminatesLemma | 80 | for fuel-bounded match, steps fit budget xor exceed it | 3 | OWED-AXIOM | 0 | covering-not-total function; needs operational semantics model |
| nestedQuantifiersExponential | 107 | nested quantifiers force complexity to `Exponential` | 6 | DISCHARGE | ~20 min | Bool-premise through `if` scrutinee; manual `rewrite` + Bool→Dec bridge |
| overlappingAltsExponential | 122 | overlapping alternatives + quantifier ⇒ `Exponential` or `Quadratic` | 6 | DISCHARGE | ~25 min | nested `if`-scrutinee + Bool `&&` split; `andTrueSplit` lemma |
| quantifiedEmptyQuadratic | 135 | quantifier over possibly-empty pattern ⇒ `Quadratic` or `Exponential` | 6 | DISCHARGE | ~20 min | same nested `if`-scrutinee blocker |
| linearNoExponentialPatterns | 152 | `Linear` ⇒ all exponential-causing patterns absent | 6 | DISCHARGE | ~30 min | if-chain backwards inversion; four-arm manual inversion |
| smallInputAlwaysSafe | 206 | input length ≤ 50 safe for every safe-regex (50 = min maxSafeInputLength) | 1 | PROPERTY-TEST | ~25 min | `length : String → Nat` FFI-opaque; spot-check fixed inputs |
| linearAllowsLargeInput | 220 | `Linear`-complexity accepts inputs up to 10M chars | 5 | DISCHARGE | ~20 min | record-projection through `case`; manual `rewrite` |
| exponentialRestrictsInput | 229 | `Exponential`-complexity restricts inputs to 100 chars | 5 | DISCHARGE | ~20 min | same record-projection blocker |
| ~~anyMatchesNonNewline~~ | 248 | `Any` matches every non-newline char | 2 | **DISCHARGED** | done 2026-05-30 | premise pass-through after `matchesClass c Any` reduces to `c /= '\n'` — no Char FFI reduction needed |
| digitOnlyDigits | 259 | `matchesClass c Digit = True` ⇒ `'0' <= c <= '9'` | 2 | DISCHARGE | ~20 min | Char FFI on `Ord Char`; `Data.Char` bridge + `andTrueSplit` |
| ~~negateInverts~~ | 274 | `Negate` inverts match result | 2 | **DISCHARGED** | done 2026-05-30 | `Refl` — `matchesClass` arm on `Negate _` reduces directly; `cls` stays abstract on both sides (no case-split needed) |
| ~~unionIsOr~~ | 284 | `Union` is logical OR of two classes | 2 | **DISCHARGED** | done 2026-05-30 | `Refl` — `matchesClass` arm on `Union _ _` reduces directly; `cls1`/`cls2` stay abstract (no case-split needed) |
| charSelfOverlaps | 302 | `SingleChar` overlaps itself via `c == c = True` | 2 | DISCHARGE | ~15 min | Char FFI on `prim__eq_Char`; `eqCharRefl` lemma |
| rangeSelfOverlaps | 316 | `Range from to` overlaps itself — `not (to < from \|\| to < from) = True` | 2 | DISCHARGE | ~20 min | Char FFI on `prim__lt_Char`; `Data.Char` bridge for `(<)` |
| unionSelfOverlaps | 332 | `Union` overlaps itself — recursive disjunction reduces | 2 | DISCHARGE | ~35 min | structural induction on `CharClass` + transitive Char-FFI |
| disjointRangesNoOverlap | 363 | positionally disjoint ranges (`t1 < f2`) do not overlap | 2 | DISCHARGE | ~20 min | Bool through `\|\|` and `not`; manual `rewrite` + `Data.Bool.orTrueLeft` |
| linearStepLimitScales | 422 | under `Linear`, step limit grows strictly with input size | 1 | DISCHARGE | ~25 min | `Nat` multiplication monotonicity; `Data.Nat.multStrictMonotone` |
| exponentialStepLimitCapped | 441 | under `Exponential`, step limit bounded above by 1M | 1 | DISCHARGE | ~20 min | `min a b <= b = True` for primitive `Nat` `min` |
| capturePositionsValid | 475 | every capture of successful match has `start <= end` | 3 | OWED-AXIOM | 0 | `MatchResult` lacks per-element refinement; needs `Capture` type refactor |
| seqPreservesSafety | 493 | sequencing two safe regexes without nested quantifiers is safe | 6 | DISCHARGE | ~20 min | three-way `&&` split; `andTrueSplit` × 2 + `notFalseTrue` |
| altPreservesSafety | 505 | alternating two safe regexes with non-overlapping firsts is safe | 6 | DISCHARGE | ~20 min | same three-way `&&`-split |
| boundedQuantPreservesSafety | 517 | bounded quantifier over known-safe body is safe | 6 | DISCHARGE | ~20 min | same three-way `&&`-split |
| complexityTransitiveFallback | 551 | 60-arm catch-all for impossible/unreachable complexity triples | 7 | DISCHARGE | ~30 min | exhaustive split on 4 `ComplexityLevel` constructors (64 cases); `absurd Refl` on impossibles |

**Module summary:** 20 DISCHARGE + 1 PROPERTY-TEST + 5 OWED-AXIOM. Cost: ~7.5h. Family 5 + 6 dominate; Family 2 is exhaustive-split-friendly. `matchingTerminatesLemma` and `capturePositionsValid` are principled OWED-AXIOM (operational semantics gap + type-refinement).

### SafeBase64 / Proofs.idr (22 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| standardAlphabetValid | 86 | every char in standard Base64 alphabet satisfies `isValidBase64Char Standard` | 1 | DISCHARGE | ~15 min | 64-arm exhaustive case-split on alphabet chars |
| urlSafeAlphabetValid | 98 | every char in URL-safe Base64 alphabet satisfies `isValidBase64Char URLSafe` | 1 | DISCHARGE | ~15 min | same shape with `+/` → `-_` |
| encodeOutputValid | 118 | encoder emits only valid Base64 chars + padding + optional MIME whitespace | 1 | PROPERTY-TEST | ~30 min | lift to pre-`pack` `List Char` (reducible by induction on 6-bit chunking) |
| paddedLengthMultipleOf4 | 145 | for padded variants, `encodedLength n mod 4 = 0` | 6 | DISCHARGE | ~20 min | `(n+2)/3 * 4` is `4*k`; `Data.Nat.divMod`-elimination |
| decodedLengthBound | 162 | `decodedLength encodedLen <= (encodedLen * 3) div 4 + 1` | 6 | DISCHARGE | ~20 min | trivial `x <= x + 1` once Bool-Prop bridge exposed |
| encodingIncreasesLength | 182 | for non-empty input, `encodedLength variant n >= n` (4/3 expansion) | 6 | DISCHARGE | ~30 min | chained `Nat` `div`/`>=`; `Data.Nat.lteMultRight` / `divLteRight` |
| roundtripCorrect | 209 | `decode variant (encode bytes) = Ok bytes` | 1+4 | OWED-AXIOM | 0 | String FFI + `Data.Bits` opacity compound; needs both reflective tactics |
| roundtripEmpty | 227 | `decode variant (encode []) = Ok []` | 1 | OWED-AXIOM | 0 | transitively blocked by roundtripCorrect |
| roundtripSingleByte | 245 | round-trip on single byte — 1-of-3 padding arm | 1+4 | OWED-AXIOM | 0 | transitively blocked by roundtripCorrect |
| roundtripString | 261 | `decodeToString variant (encodeString s) = Ok s` | 1 | OWED-AXIOM | 0 | `pack (unpack s) = s` not Refl-exposed; transitively blocked |
| variantsEqualLength | 284 | `Standard` and `URLSafe` produce equal-length output | 1 | PROPERTY-TEST | ~20 min | `length (unpack s)` FFI; spot-check with known-length vectors |
| noPadShorter | 301 | `URLSafeNoPad` output length ≤ `Standard` output length | 1+6 | DISCHARGE | ~25 min | `URLSafeNoPad` strips trailing `=` (0/1/2); `Data.Nat.lte` bridge + length lemma |
| decodeNeverCrashes | 329 | `decode variant input` always returns `Err` or `Ok` (disjunctive sigma) | 3 | DISCHARGE | ~20 min | case-split on `Base64Result` with equational witnesses |
| invalidCharDetected | 349 | input containing invalid base64 char ⇒ `decode` returns `Err` | 1 | OWED-AXIOM | 0 | `elem`/`unpack` FFI + decoder's validation short-circuit unexposed |
| invalidPaddingDetected | 377 | `=` before position `length - 2` causes decode failure | 1 | OWED-AXIOM | 0 | chained `index'`/`unpack`/`length` over opaque String |
| mimeLineBreaksCorrect | 402 | every MIME-encoded line ≤ 77 chars (76 + `\r`) | 1 | PROPERTY-TEST | ~25 min | `split`/`length`/`unpack` FFI; spot-check fixed byte-length inputs |
| mimeIgnoresWhitespace | 426 | MIME decoding invariant under whitespace insertion | 1 | OWED-AXIOM | 0 | `pack (filter p (unpack s))` FFI-opaque |
| urlSafeContainsNoUnsafe | 449 | `URLSafe` output never contains `+` or `/` | 1 | DISCHARGE | ~20 min | alphabet is fixed; lift to pre-`pack` + case-split on Fin 64 |
| standardMayContainUnsafe | 470 | disjunctive witness: `Standard` contains `+` or `/` or neither | 1 | DISCHARGE | ~30 min | `+`, `/` are Fin 64 indices (62, 63); lift to pre-`pack` list |
| threeToFourRatio | 497 | when `n mod 3 = 0`, `encodedLength Standard n = (n div 3) * 4` | 6 | DISCHARGE | ~25 min | `Data.Nat.Division` `divides`-elimination |
| paddingMatchesRemainder | 523 | padding count matches input residue mod 3 (0→0, 1→2, 2→1) | 6 | DISCHARGE | ~30 min | case-split on `n mod 3` + lift to pre-`pack`; `replicate` induction |
| segmentedRoundtrip | 547 | independently encoded segments decode independently | 1 | OWED-AXIOM | 0 | transitively blocked by roundtripCorrect |
| encodingPreservesOrder | 558 | name-alias of roundtripCorrect | 1 | OWED-AXIOM | 0 | duplicate; same blocker |

**Module summary:** 10 DISCHARGE + 6 PROPERTY-TEST + 6 OWED-AXIOM. Cost: ~5.5h. Family 6 well-supported by `Data.Nat.Division`; Family 1 splits between global (PROPERTY-TEST) and alphabet-finite (DISCHARGE) claims.

### SafeXML / Proofs.idr (9 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| builderNoXXE | 100 | builder API never emits `<!ENTITY ... SYSTEM/PUBLIC ...>` | 1 | PROPERTY-TEST | ~20 min | `isInfixOf` over `renderNode`; spot-check fixed inputs |
| builderNoDTD | 111 | builder API never emits `<!DOCTYPE ...>` | 1 | PROPERTY-TEST | ~20 min | `isInfixOf "<!DOCTYPE"`; spot-check |
| builderNoEntities | 137 | builder escapes `&` to `&amp;`, preventing raw entity refs | 1 | PROPERTY-TEST | ~25 min | spot-check multiple texts with entities |
| textEscapingSound | 159 | `xmlText` escapes `<` to `&lt;` — output contains no raw `<` | 1 | PROPERTY-TEST | ~25 min | `unpack`/`pack` opaque; spot-check `<script>`, `<>`, mixed |
| attrEscapingSound | 171 | `xmlAttrValue` escapes `"` to `&quot;` — output contains no raw `"` | 1 | PROPERTY-TEST | ~25 min | same blocker; spot-check `onclick="..."` |
| elementNameValidation | 212 | when `xmlName` returns `Ok`, `isValidXMLName` holds | 1 | DISCHARGE | ~20 min | both check same char class via `unpack`; share reduction path |
| qnameComponentsValid | 225 | when `xmlQName pfx local` returns `Ok`, both are valid XML names | 1 | DISCHARGE | ~25 min | rewrite `xmlQName` to call `xmlName pfx` + `xmlName local` |
| attrValueNoQuotes | 262 | `xmlAttrValue` output contains no unescaped `"` | 1 | PROPERTY-TEST | ~20 min | `all (\c => c /= '"')` over `unpack`; spot-check quoted attrs |
| nestedElementsSafe | 316 | adding properly-escaped child preserves escaping in parent | 3 | OWED-AXIOM | 0 | DOM-traversal structural induction gap; needs `XMLNode` children induction |

**Module summary:** 2 DISCHARGE + 6 PROPERTY-TEST + 1 OWED-AXIOM. Cost: ~3.5h. All Family 1; `nestedElementsSafe` is principled OWED-AXIOM (tree induction).

### SafeString / Proofs.idr (7 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| concatLength | 42 | `length (s1 ++ s2) = length s1 + length s2` | 1 | PROPERTY-TEST | ~20 min | String FFI `++` and `length` opaque; spot-check known cases |
| trimNoWhitespace | 70 | string with no whitespace satisfies `trim s = s` | 1 | PROPERTY-TEST | ~20 min | `pack . unpack` round-trip; spot-check non-whitespace strings |
| escapeHTMLSafe | 104 | HTML escape idempotent on safe single-char strings | 1 | PROPERTY-TEST | ~20 min | `pack . singleton` round-trip; spot-check safe chars |
| escapeSQLSafeProperty | 148 | after SQL escaping, no unescaped `'` remains | 1 | PROPERTY-TEST | ~25 min | spot-check `O'Reilly`, empty, pure quotes |
| escapeHTMLSafeProperty | 184 | after HTML escaping, no raw `<` or `>` survives | 1 | PROPERTY-TEST | ~25 min | spot-check `<tag>`, `&amp;`, mixed |
| splitJoinIdentity | 212 | without delimiter in string, `join (split delim s) = s` | 1 | PROPERTY-TEST | ~20 min | spot-check strings without separator |
| linesUnlinesApprox | 236 | `unlines (lines s) = s ++ ""` (trailing-newline slack) | 1 | PROPERTY-TEST | ~20 min | spot-check with/without trailing newlines |

**Module summary:** 0 DISCHARGE + 7 PROPERTY-TEST + 0 OWED-AXIOM. Cost: ~2.5h. Pure Family 1; uniform spot-check strategy.

### SafeCSRF / Proofs.idr (7 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| constantTimeEqRefl | 56 | `constantTimeEqual s s = True` for every `s : String` | 1+4 | OWED-AXIOM | 0 | String FFI + Char primitive + `Data.Bool.andTrueNeutral` — three reflective tactics |
| constantTimeEqSym | 79 | `constantTimeEqual a b = constantTimeEqual b a` | 2+1 | OWED-AXIOM | 0 | `prim__eq_Char` symmetry not exposed (class-J parallel) |
| differentLengthUnequal | 104 | different-length strings unequal by `constantTimeEqual` | 1+6 | DISCHARGE | ~25 min | Bool-Prop reflection on `String.length`/`unpack` vs `List.length`; `decEqToNeq` lemma or `LengthEq`-tagged refactor |
| tokenValidatesSelf | 131 | `validateToken tok (tokenString tok) = True` | 1 | OWED-AXIOM | 0 | transitively blocked by constantTimeEqRefl |
| identicalDoubleSubmitValid | 148 | `validateDoubleSubmit (MkDoubleSubmit val val) = True` | 1 | OWED-AXIOM | 0 | transitively blocked by constantTimeEqRefl |
| fullValidationRequiresToken | 179 | invalid token ⇒ full validation fails | 1+5 | OWED-AXIOM | 0 | `tokenString` accessor opacity prevents `rewrite prf in Refl` |
| wrongSessionFails | 206 | (already discharged; not OWED — included for inventory completeness) | — | (proven) | 0 | `rewrite prf in Refl` discharged at L210-211 |

**Module summary:** 1 DISCHARGE + 0 PROPERTY-TEST + 6 OWED-AXIOM. Cost: ~0.5h. RNG-bound module; most claims trace back to constantTimeEqual family.

---

## Bin 2 — SafePassword, SafePath, SafeUrl, SafeEnv (71 OWED)

### SafePassword / Proofs.idr (25 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| validPasswordLength | 81–85 | policy valid pwd ⇒ length in [min,max] | 1 | PROPERTY-TEST | ~20 min | global String claim; checkPolicy threads `unpack`/`length` |
| emptyPasswordFails | 108–110 | min-length policy rejects `""` | 1 | PROPERTY-TEST | ~15 min | empty-string corner; `unpack`/`checkLength` FFI |
| longerPasswordBetter | 123–126 | longer pwd ⇒ ≤ violations | 1+3 | OWED-AXIOM | 0 | stacked: String FFI + analyzeStrength covering |
| constantTimeRefl | 209–210 | hash XOR self = True | 6 | DISCHARGE | ~40 min | `Bits8` self-xor; needs `Data.Bits.xor_self` or hand-rolled |
| constantTimeSym | 220–221 | CTC symmetric | 6 | DISCHARGE | ~40 min | xor commutativity on `Bits8`; inline algebraic lemma |
| differentLengthNoMatch | 236–238 | different length ⇒ no match | 7 | DISCHARGE | ~30 min | `Nat /=` reflection from guard via Bool-Prop bridge |
| strengthScoreBounded | 257–258 | score ≤ 100 | 3 | DISCHARGE-after-totality | ~60 min | analyzeStrength covering; needs `detectDatePattern.windows` total — tracked in **#80** |
| entropyNonNegative | 273–274 | entropy ≥ 0 | 3+? | OWED-AXIOM | 0 | analyzeStrength covering + Double non-neg missing from stdlib |
| longerHigherEntropy | 290–292 | longer ⇒ ≥ entropy | 1+2+3 | OWED-AXIOM | 0 | triple stack: String FFI + Char FFI + covering |
| veryStrongMax | 305 | VeryStrong is max | 7 | DISCHARGE | ~35 min | 5-constructor enum; one Refl per arm |
| strengthTransitive | 319–322 | `<=` transitive | 7 | DISCHARGE | ~60 min | custom Ord StrengthLevel; 125-leaf split, prefer `Fin 5` cast |
| commonPasswordDetected | 340–342 | common pwd detected | 1+3 | OWED-AXIOM | 0 | `toLower`/`elem` String FFI + `detectPatterns` covering |
| patternPenaltyNonNeg | 353 | penalty ≥ 0 | 7 | DISCHARGE | ~30 min | 8-constructor Pattern; one Refl per arm |
| paramsAtLeastRefl | 371–372 | paramsAtLeast refl | 4 | OWED-AXIOM | 0 | external KDF opacity; non-reducing field projections |
| strongerRequiresRehash | 387–390 | weak < strong ⇒ rehash needed | 4 | OWED-AXIOM | 0 | external KDF opacity |
| builderProducesPolicy | 407 | builder empty = default | 5 | DISCHARGE | ~20 min | `MkPolicyBuilder` newtype unwrap; `%inline` or definitional split |
| withMinLengthCorrect | 419–420 | withMinLength sets field | 5 | DISCHARGE | ~25 min | record-update through newtype |
| chainedBuildersCompose | 434–435 | chained updates compose | 5 | DISCHARGE | ~25 min | reduces to withMinLengthCorrect after one update-norm step |
| higherImpliesLower | 454–458 | higher req ⇒ lower req | 3+7 | DISCHARGE-after-totality | ~50 min | `meetsRequirement`/`quickStrengthCheck` covering + custom Ord — tracked in **#80** |
| veryStrongSatisfiesAll | 471–474 | VeryStrong ⇒ all reqs | 3+7 | DISCHARGE-after-totality | ~40 min | reduces to veryStrongMax + strengthScoreBounded after totality — tracked in **#80** |
| argon2ParamsValid | 144–148 | Argon2 fields in bounds | 4 | OWED-AXIOM | 0 | if-chain over abstract Argon2Params; external lib correctness |
| bcryptCostBounded | 158–160 | bcrypt cost in [10,31] | 4 | OWED-AXIOM | 0 | if-chain over abstract BcryptParams |
| defaultArgon2Valid | 175 | defaultArgon2Params valid | 4 | OWED-AXIOM | 0 | Nat-literal `<`-comparison in if-chain |
| defaultBcryptValid | 185 | defaultBcryptParams valid | 4 | OWED-AXIOM | 0 | same |
| defaultScryptValid | 193 | defaultScryptParams valid | 4 | OWED-AXIOM | 0 | same |

**Module summary:** 9 DISCHARGE + 3 DISCHARGE-after-totality + 2 PROPERTY-TEST + 11 OWED-AXIOM. Cost: ~5.5h discharge + 3h totality-blocked. Triple-stack on `longerHigherEntropy` is expensive; the 5 KDF params proofs should live in a dedicated `Hash.Proofs` once external-library correctness is formalised.

### SafePath / Proofs.idr (23 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| normalizeIdempotent | 106–107 | normalizePath idempotent | 2+? | OWED-AXIOM | 0 | splitPath.joinSegments round-trip FFI-opaque |
| normalizeRemovesEmpty | 123–124 | `""` not in normalized | 1 | PROPERTY-TEST | ~20 min | `elem ""` over splitPath; concrete paths suffice |
| normalizeRemovesDot | 136–137 | `"."` not in normalized | 1 | PROPERTY-TEST | ~20 min | `elem "."`; same blocker |
| normalizeAbsNoLeadingDotDot | 154–158 | abs normalized ⇒ no leading `..` | 1 | PROPERTY-TEST | ~25 min | `isPrefixOf "/"` + splitPath; concrete abs paths |
| safeJoinNoEscape | 189–191 | safeJoinPaths ⇒ NoEscape witness | 1 | PROPERTY-TEST | ~30 min | splitPath FFI; test base + traversal attempts |
| sanitizedIsSafe | 209–210 | sanitizeSegment ⇒ isSafeSegment | 1 | PROPERTY-TEST | ~30 min | unpack/pack/map round-trip; test charset coverage |
| containedInBase | 227–228 | ContainedPath in base | 1 | OWED-AXIOM | 0 | composes safeJoinNoEscape + isAncestorOf; inner splitPath FFI |
| sanitizedNoTraversal | 255–256 | sanitized ⇒ no `..` | 1 | PROPERTY-TEST | ~25 min | sanitizeSegment(`..`) → `__`; concrete test |
| traversalHasDotDot | 272–274 | `elem ".."` ⇔ any `(== "..")` | 1 | OWED-AXIOM | 0 | `List.elem_any` lemma not exposed |
| pathEqRefl | 296 | pathEqSensitive refl | 1 | OWED-AXIOM | 0 | `prim__eq_String` FFI; abstract path |
| pathEqSym | 311–312 | pathEqSensitive symmetric | 1 | OWED-AXIOM | 0 | same `prim__eq_String` FFI |
| parentIsAncestor | 331–333 | parent ⇒ ancestor | 1 | OWED-AXIOM | 0 | outer splitPath FFI; List.isPrefixOf structural |
| ancestorTransitive | 349–352 | ancestor transitive | 1 | OWED-AXIOM | 0 | `List.isPrefixOf_trans` not exposed; structural after splitPath |
| changeExtensionCorrect | 376–378 | changeExt round-trip | 1 | OWED-AXIOM | 0 | five-step FFI round-trip |
| stripExtensionRemoves | 390–392 | stripExt removes .ext | 1 | OWED-AXIOM | 0 | same split/last/init chain |
| addExtensionAdds | 410–411 | addExt adds suffix | 1 | OWED-AXIOM | 0 | isSuffixOf FFI + joinSegments-snoc |
| starMatchesAll | 436 | `*` matches any string | 1 | PROPERTY-TEST | ~15 min | `unpack s` FFI; concrete (empty, "a", mixed) tests |
| questionMatchesSingle | 453 | `?` matches one char | 1 | PROPERTY-TEST | ~15 min | unpack.singleton round-trip; singleton tests |
| literalMatchesSelf | 471–473 | literal pattern matches self | 1 | PROPERTY-TEST | ~20 min | unpack + `(==)` Char FFI; wildcard-free strings |
| validPathBounded | 495–497 | validated ⇒ length ≤ 4096 | 1 | PROPERTY-TEST | ~20 min | guard-extraction on validatePath; boundary tests |
| validSegmentsBounded | 509–511 | validated segments ≤ 255 | 1 | PROPERTY-TEST | ~25 min | `any (\seg => length seg > 255)` + splitPath FFI |
| validPathNoNull | 525–527 | validated ⇒ no null bytes | 1 | PROPERTY-TEST | ~20 min | guard-extraction + unpack; null-injection tests |
| containedStartsWithBase | 547–549 | ContainedPath starts with base | 1 | OWED-AXIOM | 0 | composes safeJoinNoEscape (also blocked) |

**Module summary:** 0 DISCHARGE + 11 PROPERTY-TEST + 12 OWED-AXIOM. Cost: ~4h (property-tests with concrete inputs). SafePath has **zero immediate-DISCHARGE sites** — entirely wedged behind splitPath/normalizePath FFI or guard-extraction. Property-tests are uniformly spot-checkable; the 12 axioms form a dependency stack.

### SafeUrl / Proofs.idr (16 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| unreservedNotEncoded | 48–50 | `isAlphaNum c ⇒ percentEncode c = singleton c` | 2 | PROPERTY-TEST | ~20 min | isAlphaNum Char FFI; concrete unreserved chars (A-Z, a-z, 0-9, `-`, `.`) |
| encodePreservesAlphaNum | 67–69 | `all isAlphaNum s ⇒ urlEncode s = s` | 2+1 | PROPERTY-TEST | ~25 min | per-char + unpack/pack FFI; spot-check "abc123", "AaBbCc" |
| decodeUnreservedIdentity | 90–92 | `all isAlphaNum s ⇒ urlDecode s = Just s` | 2+1 | PROPERTY-TEST | ~25 min | same family |
| encodeDecodeIdentity | 107–108 | `urlDecode (urlEncode s) = Just s` | 1+2 | OWED-AXIOM | 0 | unpack/pack/concat FFI + chr arithmetic; external-lib shape |
| emptyBuilderEmpty | 129 | emptyQuery builder = `""` | 1 | PROPERTY-TEST | ~10 min | joinWith FFI on empty list; `""` literal by Refl |
| ~~addParamIncreasesCount~~ | 143–145 | addParam increases count | 6 | **DISCHARGED** | done 2026-05-30 | `Data.List.Equalities.lengthSnoc` (contrib) — `addParamIncreasesCount key val qb = lengthSnoc qb.params (key, val)` |
| setGetIdentity | 160–161 | `setParam k v; getParam k = Just v` | 1 | PROPERTY-TEST | ~25 min | String `==` FFI; concrete key-value pairs |
| removeHasNot | 173–174 | after removeAllParams ⇒ not hasParam | 1 | PROPERTY-TEST | ~20 min | String `/=` FFI in filter; concrete keys |
| filterPreservesOnly | 187–188 | filterParams preserves only given keys | 1 | PROPERTY-TEST | ~20 min | filterAll lemma + String elem FFI |
| parseIntValid | 205–206 | `parseInteger "42" = Just 42` | 1+2 | OWED-AXIOM | 0 | parseInteger unpack/all isDigit/foldl FFI chain |
| parseBoolTrue | 219–220 | `parseBool "true" = Just True` | 1 | PROPERTY-TEST | ~15 min | toLower "true" literal; concrete "true"/"TRUE"/"True" |
| parseBoolFalse | 228–229 | `parseBool "false" = Just False` | 1 | PROPERTY-TEST | ~15 min | symmetric |
| mergeEmptyLeft | 254–255 | `mergeQueryStrings [] qs = qs` | 1+6 | OWED-AXIOM | 0 | per-step hasParam (String FFI) + snoc-length induction |
| appendAssociative | 267–269 | appendQueryStrings assoc | 6 | DISCHARGE | ~20 min | `List.appendAssociative` stdlib lemma; import and rewrite |
| isSafeSchemeNotJavascript | 305–306 | isSafeScheme True ⇒ not javascript | 1 | PROPERTY-TEST | ~15 min | custom "javascript" String match; HTTP/HTTPS/data/vbscript/javascript |
| lteFrom65535Check | 346 | `p <= 65535` Bool ⇒ `LTE p 65535` | 6 | DISCHARGE | ~20 min | `Data.Nat.lteReflectsLTE` stdlib lemma |

**Module summary:** 3 DISCHARGE + 10 PROPERTY-TEST + 3 OWED-AXIOM. Cost: ~1.5h. SafeUrl is cleanest in this bin — 3 axioms total, 10/16 sites spot-check-friendly, the 3 DISCHARGE route to stdlib lemmas.

### SafeEnv / Proofs.idr (7 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| digitStartInvalid | 47–49 | digit-start ⇒ invalid name | 1+2 | PROPERTY-TEST | ~20 min | unpack/pack/isDigit FFI; concrete 0-9 prefix tests |
| wellKnownValid | 63–65 | wellKnown names valid | 1 | PROPERTY-TEST | ~15 min | elem name list FFI; test PATH/HOME/USER/SHELL literals |
| emptyBounded | 104 | empty string bounded by any maxLen | 1 | OWED-AXIOM | 0 | unpack `""` → `[]` FFI; refactor to primitive `String.length` could help |
| publicClassification | 162–164 | not sensitive ⇒ Public | 1 | PROPERTY-TEST | ~20 min | isSensitiveName Bool FFI in if-head; test non-sensitive names |
| sensitiveClassification | 177–179 | sensitive ⇒ Sensitive | 1 | PROPERTY-TEST | ~20 min | test PASSWORD/TOKEN/SECRET/KEY patterns |
| validBoolParses | 231–233 | known-bool string ⇒ Just _ | 1+2 | PROPERTY-TEST | ~20 min | toLower FFI + String literal; true/false/yes/no/1/0/on/off |
| validIntParses | 247–249 | all-digit string ⇒ parseInteger Just | 1+2 | OWED-AXIOM | 0 | parseInteger unpack/all isDigit/foldl chain |

**Module summary:** 0 DISCHARGE + 5 PROPERTY-TEST + 2 OWED-AXIOM. Cost: ~1.5h. Smallest, most homogeneous module — all blockers String/Char FFI, all property-tests concrete identity checks.

---

## Bin 3 — SafeEmail, SafeCrypto, SafeJson, SafeArchive, SafeChecksum, SafeTOML (67 OWED)

### SafeEmail / Proofs.idr (20 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| parseEmptyFails | 45 | `parseEmail "" = Nothing` | 1 | PROPERTY-TEST | ~10 min | concrete input; String FFI literal match gap |
| parseNoAtFails | 65 | `parseEmail "noatsign" = Nothing` | 1 | PROPERTY-TEST | ~10 min | concrete input; splitOnLast opaque |
| ~~validResultIsValid~~ | 83 | `validResult.isValid = True` | 5 | **DISCHARGED** | done 2026-05-30 | `Refl` — `validResult` is `public export`, elaborator unfolds field projection directly |
| errorMakesInvalid | 99 | severity=Error ⇒ addIssue.isValid = False | 7 | DISCHARGE | ~30 min | enum Eq on ValidationSeverity; 3-arm split |
| warningKeepsValid | 111 | severity=Warning ⇒ addIssue.isValid = True | 7 | DISCHARGE | ~30 min | enum Eq + record-projection combined |
| combineValidValid | 128 | r1, r2 valid ⇒ combineResults valid | 5 | DISCHARGE | ~30 min | record-projection on universally-quantified records |
| parsedContainsAt | 153 | `isJust (parseEmail s) ⇒ '@' elem unpack s` | 1 | OWED-AXIOM | 0 | global String/unpack claim; parseEmail path opaque |
| normalizeIdempotent | 186 | `toLower (toLower d) = toLower d` | 1 | OWED-AXIOM | 0 | String FFI `prim__strToLower` opaque |
| sanitizeRemovesNewlinesLemma | 230 | filter post-condition on List Char | 1 | DISCHARGE | ~60 min | induction + Bool `&&`-projection |
| sanitizeRemovesNewlines | 247 | `NoNewlines (sanitizeForHeader s)` | 1 | DISCHARGE | ~60 min | unpack.pack round-trip + upstream lemma |
| filterValidCorrect | 266 | all (validateEmailFull . isValid) (filterValid emails) | 1 | DISCHARGE | ~40 min | filter post-condition; `Data.List.filterAllP` shape |
| uniqueNoDuplicates | 284 | `LTE (length (uniqueEmails xs)) (length xs)` | 6 | DISCHARGE | ~50 min | nubBy-length monotonicity; structural induction + LTE transport |
| freeEmailExhaustive | 303 | Bool LEM on isFreeEmail | 7 | DISCHARGE | ~15 min | one-line case-split |
| typoCheckFindsKnown | 318 | `checkCommonTypos "gmial.com" = ...` | 1 | PROPERTY-TEST | ~10 min | concrete input; toLower + String == opaque |
| validLocalNoStartDot | 338 | validated ⇒ no leading `.` | 1 | PROPERTY-TEST | ~20 min | bounded inputs; String FFI isPrefixOf |
| validLocalNoEndDot | 348 | validated ⇒ no trailing `.` | 1 | PROPERTY-TEST | ~20 min | symmetric |
| validDomainHasLabel | 361 | `LTE 1 (length (forget (split (== '.') domain)))` | — | DISCHARGE | ~15 min | List1 length lower-bound; one-line case-split |
| comprehensiveCatchesRFC | 381 | full-fail ⇒ comprehensive-fail | 5 | DISCHARGE | ~50 min | combineResults monotonicity + foldl invariant |
| validPassesComprehensive | 400 | full-pass ⇒ no errors in comprehensive | 7 | DISCHARGE | ~60 min | foldl combineResults + hasErrors any-reduction |

**Module summary:** 10 DISCHARGE + 5 PROPERTY-TEST + 5 OWED-AXIOM (one row in inventory but ~20 OWED via grep; minor count discrepancy). Cost: ~8-10h. Most "String FFI" sites reduce to bounded-alphabet or concrete-input tests rather than full OWED-AXIOM.

### SafeCrypto / Proofs.idr (16 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| constantTimeRefl | 58 | `digestEq d d = True` | 6 | OWED-AXIOM | 0 | `Data.Bits xor x x = 0` not exposed |
| constantTimeSym | 69 | `digestEq d1 d2 = digestEq d2 d1` | 6 | OWED-AXIOM | 0 | `Data.Bits xor` commutativity not exposed |
| modernIsSecure | 138 | `securityLevel alg = Modern ⇒ isSecure alg = True` | (Idris2 0.9.0 dep, not Family 3) | DISCHARGE-after-Idris2-upgrade | ~30 min | `case` not eta-expanded under rewrite; tracked in **#83** |
| standardIsSecure | 148 | `securityLevel alg = Standard ⇒ isSecure alg = True` | (Idris2 0.9.0 dep, not Family 3) | DISCHARGE-after-Idris2-upgrade | ~30 min | symmetric to modernIsSecure; tracked in **#83** |
| digestEqRefl | 162 | duplicate of constantTimeRefl | 6 | OWED-AXIOM | 0 | same xor blocker |
| digestEqSym | 168 | duplicate of constantTimeSym | 6 | OWED-AXIOM | 0 | same |
| differentDigestsUnequal | 180 | `Not (d1 = d2) ⇒ digestEq d1 d2 = False` | 6 | OWED-AXIOM | 0 | xor injectivity not exposed |
| randomBytesLength | 202 | `length v = n` (ByteVec constructor) | 4 | OWED-AXIOM | 0 | FFI entropy source opaque |
| randomNatBounded | 217 | `LT n max` (from mod-based randomNat) | 6 | DISCHARGE | ~40 min | `Integral Nat mod + LT`; `modNatNZ` + `Data.Nat.Division` lemmas |
| randomRangeBounded | 229 | `(LTE mn n, LTE n mx)` (randomNat + offset) | 6 | DISCHARGE | ~60 min | randomNatBounded + plusLte monotonicity |
| counterNonceUnique | 249 | `Not (c1 = c2) ⇒ Not (counterNonce pfx c1 = counterNonce pfx c2)` | 6 | OWED-AXIOM | 0 | `Data.Bits shiftR + cast` round-trip injectivity |
| freshNonceSize | 258 | duplicate-shape of randomBytesLength | 4 | OWED-AXIOM | 0 | FFI entropy + dependent index |
| tokenLengthApprox | 279 | `LTE (length s) ((bytes * 4 div 3) + 3)` | 1 | OWED-AXIOM | 0 | String FFI on base64 encoder |
| uuidLength | 293 | `length s = 36` (UUID-v4) | 1 | OWED-AXIOM | 0 | String FFI pack/length round-trip |
| hexEncodeEvenLength | 335 | `mod (length (bytesToHex bs)) 2 = 0` | 1 | OWED-AXIOM | 0 | String FFI concat/pack/map opaque |

**Module summary:** 2 DISCHARGE + 2 DISCHARGE-after-totality + 0 PROPERTY-TEST + 12 OWED-AXIOM. Cost: ~2h. SafeCrypto is the most axiom-dense — Data.Bits primitives + FFI entropy compound. The type-level invariants express FFI invariants, not Idris-side logic.

### SafeJson / Proofs.idr (13 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| setGetIdentity | 105 | `get k (set k v obj) = Just v` | 1 | OWED-AXIOM | 0 | `prim__eq_String` opaque in lookup/update |
| setPreservesOther | 123 | `Not (k1 = k2) ⇒ get k2 (set k1 v obj) = get k2 obj` | 1 | OWED-AXIOM | 0 | String disequality not derivable from `Not (k1=k2)` |
| setHasKey | 139 | `isObject obj ⇒ hasKey k (set k v obj) = True` | 1 | OWED-AXIOM | 0 | same `prim__eq_String` blocker |
| removeNotHasKey | 157 | `hasKey k (remove k obj) = False` | 1 | OWED-AXIOM | 0 | String `!=` on pairs + filter |
| appendLengthInc | 197 | `length (arr ++ [v]) = length arr + 1` | — | DISCHARGE | ~40 min | `lengthAppend` + `plusZeroRightNeutral` + S congruence |
| singleKeyPath | 228 | `getPath [Key k] (JsonObject obj) = lookup k obj` | 3 | DISCHARGE | ~50 min | getPath covering→total refactor complete (proven#81); six-arm split on JsonValue |
| parseNullCorrect | 258 | `parseJson "null" = Just JsonNull` | 1 | PROPERTY-TEST | ~10 min | concrete input |
| parseTrueCorrect | 264 | `parseJson "true" = Just (JsonBool True)` | 1 | PROPERTY-TEST | ~10 min | concrete input |
| parseFalseCorrect | 270 | `parseJson "false" = Just (JsonBool False)` | 1 | PROPERTY-TEST | ~10 min | concrete input |
| parseEmptyFails | 279 | `parseJson "" = Nothing` | 1 | PROPERTY-TEST | ~10 min | concrete input |
| parseEmptyArray | 287 | `parseJson "[]" = Just (JsonArray [])` | 1 | PROPERTY-TEST | ~10 min | two-char lookahead via strSubstr |
| parseEmptyObject | 294 | `parseJson "{}" = Just (JsonObject [])` | 1 | PROPERTY-TEST | ~10 min | same lookahead pattern |
| anyMatchesTAny | 339 | `matchesType v TAny = True` for all `v` | 3 | DISCHARGE | ~50 min | matchesType covering→total refactor complete (proven#81); six-arm split on JsonValue |

**Module summary:** 3 DISCHARGE + 0 DISCHARGE-after-totality + 6 PROPERTY-TEST + 4 OWED-AXIOM. Cost: ~2-2.5h. Half the module is concrete-input PROPERTY-TEST (parser tests); covering-function gap resolved (proven#81).

### SafeArchive / Proofs.idr (6 OWED — note: 7 grep hits include 1 meta-comment in module header)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| maxCompressionRatioAnchor | 113 | `maxCompressionRatio = 1000` | — | OWED-AXIOM | 0 | Nat literal opacity (standards#128); large unary Nat not Refl-reducible |
| maxTotalSizeAnchor | 120 | `maxTotalSize = 1073741824` (2^30) | — | OWED-AXIOM | 0 | Nat literal opacity |
| modestRatioNotZipBomb | 143 | `isZipBomb (Entry 1 100) = False` | — | OWED-AXIOM | 0 | Nat literal opacity on ratio comparison |
| extremeRatioIsZipBomb | 149 | `isZipBomb (Entry 1 1001) = True` | — | OWED-AXIOM | 0 | same |
| plainPathHasNoTraversal | 197 | `hasPathTraversal "normal.txt" = False` | 1 | PROPERTY-TEST | ~10 min | concrete input; `isInfixOf`/`isPrefixOf` opaque but fixed path |
| dotDotPathHasTraversal | 202 | `hasPathTraversal "../etc/passwd" = True` | 1 | PROPERTY-TEST | ~10 min | concrete input |

**Module summary:** 0 DISCHARGE + 2 PROPERTY-TEST + 4 OWED-AXIOM. Cost: ~30 min. Nat-literal opacity is the only blocker for 4 sites; the string-path checks are concrete spot-checks.

### SafeChecksum / Proofs.idr (7 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| crc32PolynomialIsIEEE | 60 | `crc32Polynomial = 0xEDB88320` | — | OWED-AXIOM | 0 | Bits32 numeric-literal opaque |
| adler32ModIsLargestPrimeBelow2Pow16 | 70 | `adler32Mod = 65521` | — | OWED-AXIOM | 0 | Nat literal opacity |
| sumChecksumEmpty | 117 | `sumChecksum [] = 0` | 6 | OWED-AXIOM | 0 | `Integral Nat mod` not reducing `0 mod 256 = 0` by Refl |
| twosComplementEmpty | 121 | `twosComplement [] = 0` | 6 | OWED-AXIOM | 0 | same Integral Nat mod opacity |
| luhnValidatesKnownGood | 126 | `validateLuhn "4111111111111111" = True` | 1 | PROPERTY-TEST | ~10 min | concrete known-valid card; String FFI `unpack`/`ord` opaque |
| isbn10ValidatesKnownGood | 131 | `validateISBN10 "0306406152" = True` | 1 | PROPERTY-TEST | ~10 min | concrete known-valid ISBN-10 |
| isbn13ValidatesKnownGood | 136 | `validateISBN13 "9780306406157" = True` | 1 | PROPERTY-TEST | ~10 min | concrete known-valid ISBN-13 |

**Module summary:** 0 DISCHARGE + 3 PROPERTY-TEST + 4 OWED-AXIOM. Cost: ~45 min. Nat literal + `Integral Nat mod` opacities are permanent without a reflective tactic roadmap.

### SafeTOML / Proofs.idr (5 OWED)

| Site name | Line | Goal (1-line paraphrase) | Family | Disposition | Cost | Notes |
|---|---:|---|---|---|---|---|
| isScalarCorrect | 155 | `isScalar val = True ⇒ not (isTable val \|\| isArray val) = True` | 7 | DISCHARGE | ~30 min | 10-arm case-split on TOMLValue; Refl per scalar, absurd per non-scalar |
| bareKeyCharsValid | 188 | `isValidBareKey key ⇒ all isValidBareKeyChar (unpack key)` | 1 | OWED-AXIOM | 0 | String FFI `unpack` opaque + `&&`-elimination on abstract key |
| specialCharsNeedQuoting | 216 | `any (\c => not (isValidBareKeyChar c)) (unpack key) ⇒ needsQuoting key` | 1 | OWED-AXIOM | 0 | unpack opaque + De Morgan any/all duality not exposed |
| dateComponentsValid | 303 | `TOMLDate.month ∈ [1,12] && day ∈ [1,31]` | 4 | OWED-AXIOM | 0 | parser-postcondition, not type-level invariant; TOMLDate is plain record |
| timeComponentsValid | 328 | `TOMLTime.hour ≤ 23 && minute ≤ 59 && second ≤ 60` | 4 | OWED-AXIOM | 0 | same shape — parser-postcondition |

**Module summary:** 1 DISCHARGE + 0 PROPERTY-TEST + 4 OWED-AXIOM. Cost: ~30 min. `isScalarCorrect` is tractable; the DateTime sites are I/O oracle assumptions, not logic proofs.

---

## Cross-module observations

1. **Family 1 (String FFI) is the dominant blocker** (~46% of all Tier A sites). Splits roughly 50/50 between global claims that become PROPERTY-TEST (concrete-input spot-checks) and finite-alphabet claims that become DISCHARGE (n-arm case-split).

2. **Family 2 (Char FFI) is almost always DISCHARGE** via exhaustive ASCII case-split (~14% of sites). The cost is mechanical (one Refl per Char arm), not blocked.

3. **Family 4 (External KDF / opaque libs) is almost always OWED-AXIOM** — these are honest trusted-base entries. SafeCrypto and SafePassword's KDF-params proofs are the canonical examples. There is no Idris2 fix that helps here; the axiom names which external library claim is being trusted.

4. **Family 3 (covering / totality)** sites are DISCHARGE-after-totality. Two modules still have them: SafePassword (`analyzeStrength`) and SafeRegex (`matchingTerminatesLemma`). SafeJson's covering→total refactor (proven#81) discharged both `getPath` and `matchesType`. Each remaining module needs a per-module totality-refactor ticket.

5. **The 5 SafePassword KDF params proofs** (argon2ParamsValid, bcryptCostBounded, defaultArgon2Valid, defaultBcryptValid, defaultScryptValid) should likely be lifted into a dedicated `Hash.Proofs` module once external-library correctness is formalised as a separate layer. They're not really logic proofs — they're parameter-bound assertions over abstract record fields.

6. **SafeCrypto's Data.Bits dependency** is a chokepoint — 7 sites blocked on `xor x x = 0`, `xor` commutativity, `shiftR/shiftL` injectivity, and `cast`-roundtrip lemmas that Idris2 0.8.0's `Data.Bits` doesn't expose. A standalone `Data.Bits.Reflection` lemma pack would unblock most of SafeCrypto in one stroke.

7. **The DISCHARGE backlog is concentrated**: SafeRegex (20 sites) + SafeBase64 (10) + SafeEmail (10) account for **~70% of all immediate-DISCHARGE work**. A focused 2-3 day sprint on these three modules clears the bulk of Phase 3 Tier A.

8. **The PROPERTY-TEST backlog is uniform**: every module with String FFI claims has spot-checkable concrete-input tests. The existing `tests/properties/<Module>Props.idr` infrastructure (wired to CI in proven#76 via #78) is the natural landing site — most modules already have 10-30 `prop_X = Refl` discharges; the new tests slot in alongside.

---

## What Phase 2 Days 11-21 will produce

This file covers **Days 3-10 (Tier A per-site reads)**. The remaining Phase 2 days produce:

- **Days 11-14 (Tier B per-site reads — totality-blocked):** file a per-module totality-refactor ticket for SafeRegex, SafeJson, SafeTOML, SafePassword (the modules with Family 3 blockers). Per-site dispositions land as "DISCHARGE blocked on totality ticket #N".
- **Days 15-18 (Tier C confirmation — 30+ OWED-AXIOM permanents):** for each OWED-AXIOM site enumerated above, verify the doc comment names the external claim with library + version where applicable. Fix where missing.
- **Days 19-21 (witness-only long tail):** enumerate the ~55 long-tail single-file overclaim modules from PROOF-NEEDS.md §"Owed" extrapolation. Produce a PROOF-NEEDS.md patch promoting them out of "extrapolated" into a named list with severity assignments.

The data in this file makes Phase 3 sequencing concrete: start with SafeUrl (3 DISCHARGE, ~1.5h) as a warm-up, then SafeRegex (20 DISCHARGE, ~7.5h) as the highest-leverage single module.
