# Proof-Debt Triage — proven

**Last updated:** 2026-05-27
**Author:** Jonathan D.A. Jewell (hyperpolymath) `<jonathan.jewell@open.ac.uk>`
**Phase:** 1 (classification framework + initial pass; full triage scheduled across ~3 weeks)
**Companion ledger:** [`PROOF-NEEDS.md`](../PROOF-NEEDS.md) (the debt list); this file is the *plan* for paying it down.

---

## 0. Scope and naming

This document is the triage layer over the discoverable proof-debt surface in
`proven`. It does **not** introduce new debt and it does **not** change any
`.idr` file. It classifies every existing debt-bearing site into one of three
disposition buckets so a downstream discharge campaign can be sequenced.

The prompt that seeded this doc used the phrase "`TODO PROOF` markers". That
literal token does not appear in this repo. The discoverable debt surface is
the **Fork A convention** established by PRs #37–#64
(`hyperpolymath/standards#158` Fork A): a doc comment beginning `||| OWED:`
immediately above a `0` erased-multiplicity declaration. This file uses that
surface as the unit of triage, plus three smaller surfaces enumerated in §1.

**Source of truth for counts:** live grep of `src/` at commit `0044629` on
`main`, 2026-05-27. Numbers will drift; re-run the inventory before relying on
the totals.

---

## 1. The debt surface (inventory, 2026-05-27)

| Surface | Count | Where | Disposition |
|---|---:|---|---|
| `\|\|\| OWED:` annotations (Fork A) | **280** | 44 `Proofs.idr` modules under `src/Proven/Safe*/` | **Per-site classification — §3, §4** |
| `@ Assumed:` postulate tags | **4** | `src/Proven/SafeDigest.idr:222,230,237,245` | **OWED-AXIOM (already)** — §5 |
| `assert_smaller` in executable code | **1** | `src/Proven/SafeInput.idr:282` | Compiler-totality concession, not proof debt; out of scope (§6) |
| Witness-only overclaim modules | **~70** | single-file `src/Proven/Safe*.idr` with `Prevents:` / `Guarantees:` / `formally verified` and no discharged theorem | **Separate track — §7** |
| `tests/properties/` holes (orphan tests) | **87** | 26 of 29 `tests/properties/*Props.idr` files, body `?prop_X_rhs` | **Test-side OWED stubs — §8 Day 1** |
| Runtime `believe_me` | **0** | — | clean |
| Real `?`-holes in `src/` def bodies | **0** | — | clean |

The 280 OWED + 4 Assumed + 1 assert + ~70 overclaim + 87 test holes ≈
**~442 distinct debt-bearing artefacts**. The seeding prompt's "372" was
in the same order of magnitude but missed the test-side holes entirely
because the test dir is currently orphan (no `.ipkg` references it; CI
never builds it). The number to trust is the live grep, not any prior count.

> **Orphan-test discovery (2026-05-27 analysis pass):** `tests/properties/`
> contains 29 `*Props.idr` files with 606 `prop_X` declarations (519
> discharged inline `= Refl`, 87 stubbed as `= ?prop_X_rhs`). None of these
> are listed in any `.ipkg`; `idris2-ci.yml` never type-checks them; the
> Justfile `test-idris` recipe falls back to `test-simple.ipkg` (FFI smoke
> only). The 87 holes therefore silently rot. Phase 2 Day 1 wires this dir
> to CI and converts the holes to OWED stubs so the debt is discoverable —
> see §8.

---

## 2. Classification rubric

Every site lands in exactly one of three buckets. The bucket determines who
discharges it, how, and on what timeline.

### DISCHARGE

**Definition.** The obligation can be proven in Idris2 0.8.0 today, with no
new compiler feature or external machinery. The blocker (if any) is local
case-analysis effort.

**Form of the discharge.** The `0 name : Type` declaration gets a `name = ...`
clause underneath. The `||| OWED:` block is rewritten as a `||| Theorem:`
block. The PR cites this entry by `Proofs.idr:LINE` and runs `idris2 --check`
as the merge oracle.

**Identifying signal.** No FFI-bound primitive in the goal; the predicates
reduce by Refl or by induction on a Nat/List/explicit structure already in
scope; the function under proof is `total` (not `covering`); no opaque field
projection on a universally-quantified record.

**Cost model.** Minutes to hours per site. Concrete pace estimate: 5–15
sites/day for the experienced prover, slower at first.

### PROPERTY-TEST

**Definition.** The obligation is provable in principle but the discharge cost
exceeds the value of a *type-level* proof, and a runtime check at a `tests/`
boundary gives equivalent assurance with strictly less mechanism. The OWED
block stays in place as documentation; a `tests/properties/<Module>Props.idr`
entry pins the property as `prop_X : <claim> = <truth>` and discharges with
`= Refl` where the runtime computation actually reduces.

**Convention.** The existing repo pattern is **NOT** hedgehog-style
randomised generators — it is **inline type-equality assertions with `Refl`
witnesses**, e.g. `prop_emptyEncodesEmpty : encode "" = "" ; prop_... = Refl`.
Concrete inputs (`"4111111111111111"` for Luhn) work where the runtime
computation reduces against a literal input even though the universally-
quantified version does not. Where `Refl` does not discharge, the entry
becomes a documented OWED stub mirroring the Fork A convention on the source
side (see §8 Day 1).

**When to choose this over DISCHARGE.** The blocker is a String/Char FFI
opacity wall (Families 1 and 2 below), the obligation is over **arbitrary
strings** rather than the fixed alphabet you control, and a fixed set of
concrete-input spot-checks gives sufficient assurance.

**Cost model.** ~15–30 min per concrete-input spot-check; ~10 min per OWED
stub conversion when `Refl` does not discharge.

### OWED-AXIOM

**Definition.** The obligation is admitted as a bridge axiom — kept exactly in
the existing Fork A form (`||| OWED:` + `0 name : Type`), with no body — and
this is acknowledged as **permanent or until-compiler-upgrade**. The named
declaration *is* the trusted-base entry. Per the Fork A convention this is
discoverable by AST and cannot leak into runtime.

**When to choose this.** (i) The blocker is a non-Idris primitive whose
semantics live in C/Zig/external libs (Argon2, OpenSSL, FFI String/Char
ops); (ii) the obligation expresses an external library's correctness, not
the Idris caller's; (iii) discharging requires waiting on Idris2 ≥ 0.9.0
reflective-tactic infrastructure.

**What "OWED-AXIOM" buys.** The honest, narrowest possible statement of what
the trusted base is — every axiom is named, doc-commented with the exact
blocker, and counted. This is the existing situation for all 280 OWED sites
*by default*; the triage job is to find the ones that should be promoted out
of OWED-AXIOM into DISCHARGE or PROPERTY-TEST.

**Cost model.** Zero discharge cost; non-zero ledger maintenance cost (must be
re-verified each Idris2 upgrade in case the blocker disappears).

### Decision tree

```
For each OWED site:

  1. Does the goal mention an FFI-bound String/Char primitive (unpack, pack,
     length, splitOn, isUpper, ...) AND a universally-quantified String/Char?
       yes → go to 2
       no  → DISCHARGE candidate; read the goal and confirm

  2. Is the property a global claim ("for all strings, ...") or a finite
     alphabet check ("for these 64 chars, ...")?
       global   → PROPERTY-TEST (cheap generator beats waiting for tactic)
       finite   → DISCHARGE via n-arm exhaustive case-split (Refl per case)

  3. Does the goal mention an external KDF / OpenSSL / Argon2 / opaque
     library call?
       yes → OWED-AXIOM (the external library is the trusted base; document
             which library and which version)
       no  → re-evaluate from 1

  4. Is the surrounding function `covering` rather than `total`?
       yes → DISCHARGE blocked on totality refactor; route to a totality
             ticket first, then re-triage
       no  → keep going

  5. Default: OWED-AXIOM with a TODO to revisit on next Idris2 release.
```

---

## 3. Blocker families and their default classification

The inventory (§1, plus `PROOF-NEEDS.md`) identifies seven recurring blocker
families. Each family carries a **default disposition** that applies unless
the per-site read flips it. This is the lever that makes 280-site triage
tractable — most sites inherit the family default.

| # | Family | OWED sites | Default | Why this default |
|---|---|---:|---|---|
| 1 | String FFI opacity (`unpack`/`pack`/`length`/`splitOn`/`isInfixOf`) | ~87 | **PROPERTY-TEST** for global claims; **DISCHARGE** for fixed-alphabet claims | Discharge is possible per-char but expensive; randomised testing of validators gives equivalent assurance and is faster to author. |
| 2 | Char FFI opacity (`isUpper`/`isLower`/`isDigit`/`isAlpha`/`isSpace`) | ~18 | **DISCHARGE** via exhaustive ASCII case-split where the codomain is small, else **PROPERTY-TEST** | Char predicates are finite-domain enough that 128-arm splits are mechanical. |
| 3 | Covering / totality barrier (`covering` on `analyzeStrength`, `parseVal`, `gcd`, regex internals) | ~22 | **DISCHARGE-after-totality-refactor** | The proof can be written *after* the function is made `total`. Route via a totality ticket per module; do not write the proof against a `covering` body. |
| 4 | External KDF / opaque library (Argon2id, bcrypt, scrypt, PBKDF2, OpenSSL) | ~15 | **OWED-AXIOM** (permanent) | The external library's correctness lives outside Idris. The axiom names which external claim is being trusted. |
| 5 | Record-projection gap on universally-quantified records | ~8 | **DISCHARGE** | Inline-constructor case-analysis or `withUnfold` discharges by Refl. |
| 6 | `Integral Nat` `mod`/`div` opacity | ~7 | **DISCHARGE** via `Data.Nat.Division` lemmas | `divNatNZ` / `modNatNZ` are addressable in 0.8.0 if you bypass the type-class projection. |
| 7 | Enum Eq reduction (user `Eq` instances on small enums) | ~6 | **DISCHARGE** via exhaustive case-split | Finite domain; one `Refl` per constructor. |
| — | Mixed / not yet classified | residual | per-site read | — |

**Family totals:** 87 + 18 + 22 + 15 + 8 + 7 + 6 = 163 sites classified by
family default. The remaining ~117 sites need per-site reads — that is the
bulk of the 3-week Phase 2 work.

---

## 4. Per-module triage table (44 OWED-bearing Proofs.idr modules)

Each row gives the OWED count for the module, the dominant blocker family, the
default disposition that follows, and a sequencing tier. **Tier A** is "do
first" — the highest-leverage discharges. **Tier B** is "do after totality
refactors land". **Tier C** is "OWED-AXIOM, no discharge planned without a
compiler upgrade".

Status column meaning: `[ ]` not yet per-site-read; `[~]` partially read; `[x]`
fully read and each site individually classified.

| Module | OWED | Dominant family | Default disposition | Tier | Status |
|---|---:|---|---|---|---|
| **SafeBase64** / Proofs.idr | 22 | 1 (String FFI) + 6 (Integral) | PROPERTY-TEST (alphabet) + DISCHARGE (length math) | A | [x] |
| **SafeRegex** / Proofs.idr | 26 | 1 + 3 (covering) | DISCHARGE-after-totality + PROPERTY-TEST | A | [x] |
| **SafePassword** / Proofs.idr | 25 | 1 + 2 (Char) + 3 (covering on analyzeStrength) | PROPERTY-TEST (policy) + DISCHARGE (Char predicates) | A | [x] |
| **SafePath** / Proofs.idr | 23 | 1 (splitOn/isInfixOf) | PROPERTY-TEST (path-traversal) | A | [x] |
| **SafeEmail** / Proofs.idr | 20 | 1 + 5 (record projection) + 7 (enum Eq) | DISCHARGE (record + enum) + PROPERTY-TEST (parser) | A | [x] |
| **SafeUrl** / Proofs.idr | 16 | 1 | PROPERTY-TEST + DISCHARGE | A | [x] |
| **SafeCrypto** / Proofs.idr | 16 | 6 (Data.Bits) + 4 (FFI entropy) | OWED-AXIOM (mostly) | A | [x] |
| **SafeJson** / Proofs.idr | 13 | 1 + 3 (covering on getPath/matchesType) | DISCHARGE-after-totality + PROPERTY-TEST | A | [x] |
| **SafeXML** / Proofs.idr | 9 | 1 | PROPERTY-TEST | A | [x] |
| **SafeString** / Proofs.idr | 7 | 1 | PROPERTY-TEST | A | [x] |
| **SafeEnv** / Proofs.idr | 7 | 1 | PROPERTY-TEST | A | [x] |
| **SafeCSRF** / Proofs.idr | 7 | 1 + 4 (RNG-bound) | OWED-AXIOM | A | [x] |
| **SafeChecksum** / Proofs.idr | 7 | 1 + 6 (mod arithmetic) | PROPERTY-TEST + OWED-AXIOM | A | [x] |
| **SafeArchive** / Proofs.idr | 6 | — (Nat literals) + 1 (path-traversal) | PROPERTY-TEST + OWED-AXIOM | A | [x] |
| **SafeTOML** / Proofs.idr | 5 | 1 + 4 | OWED-AXIOM | A | [x] |
| (remaining 29 OWED-bearing modules) | ~70 | mixed | per-module read in Phase 2 Days 11-14 | — | [ ] |

**Coverage of the table:** 15 of 44 modules accounting for ~209 of 280 OWED
sites (75%). The 29-module tail covers ~70 sites averaging 2–3 OWED each —
those land in Phase 2 alongside Tier B totality refactors (Days 11–14).

**Tier A per-site classification (Days 3–10, complete 2026-05-27):** the
full per-site disposition tables for all 15 Tier A modules live in
[`proof-debt-triage-tier-a.md`](proof-debt-triage-tier-a.md). Aggregate
result: **~59 DISCHARGE** (28%, ~30h to clear) + **~7 DISCHARGE-after-totality**
(3%, blocked) + **~66 PROPERTY-TEST** (31%, ~22h) + **~79 OWED-AXIOM** (38%,
permanent or until-Idris2-upgrade). The DISCHARGE backlog is concentrated:
SafeRegex (20) + SafeBase64 (10) + SafeEmail (10) account for ~70% of all
immediate-DISCHARGE work. Recommended Phase 3 sequencing: SafeUrl (3
DISCHARGE, ~1.5h) as warm-up, then SafeRegex (20 DISCHARGE, ~7.5h) as the
highest-leverage single module.

---

## 5. The four `@ Assumed:` tags (SafeDigest)

All four live in `src/Proven/SafeDigest.idr` and are textbook OWED-AXIOM cases
under the new rubric. They predate the Fork A convention and use the older
`@ Assumed:` doc tag instead of the `||| OWED:` block.

| Line | Claim | Disposition |
|---:|---|---|
| 222 | `parseDigest (toString d) = ValidDigest d` | **OWED-AXIOM** until reflective String tactic — Family 1 |
| 230 | `constantTimeCompare s s = True` | **OWED-AXIOM** — Family 1 (reduces via unpack/zip/foldl over opaque String) |
| 237 | `constantTimeCompare a b = constantTimeCompare b a` | **OWED-AXIOM** — Family 1 |
| 245 | `verify a b ∧ verify b c → verify a c` | **OWED-AXIOM** — Family 1 (transitive over constantTimeCompare) |

**Phase 2 action:** migrate the four `@ Assumed:` blocks to the Fork A `|||
OWED:` + `0` form so they enter the standard ledger. The module header's
"formally verified" claim must be softened in the same PR — see §7,
SafeDigest.

---

## 6. Out of scope (explicitly)

- **`assert_smaller` at `src/Proven/SafeInput.idr:282`** is a totality-checker
  concession on a tail-recursive accumulator loop, not a proof obligation. Out
  of scope; tracked as a separate "totality refactor" candidate if ever.
- **Idris2 0.8.0 compiler-feature wishes** (reflective `Data.String` tactic,
  `Bool`-reflective `lteSucc`, type-class unfolding for `Integral Nat`) are
  *enablers* for moving sites out of OWED-AXIOM. They are filed upstream, not
  in this ledger.
- **The 28 `%foreign` declarations** carry no safety claims and are not proof
  debt.

---

## 7. Witness-only overclaim track (separate from §3–§5)

The ~70 single-file `src/Proven/Safe*.idr` modules that ship a security claim
in their doc header without a discharged theorem are a *different* problem
shape than the 280 OWED sites. The 280 OWED sites are honestly named
unfinished proofs in companion `Proofs.idr` files. The overclaim modules are
**doc-header claims with no Proofs.idr at all**. `PROOF-NEEDS.md` §"Owed —
single-file modules" enumerates the 15 highest-concern ones; the long tail
(~55 more) is acknowledged as extrapolated from the 32-module adversarial
sample (3% genuinely-proven rate).

**Triage disposition for this track:**

| Severity | Modules | Disposition |
|---|---|---|
| **Critical** (crypto + actively-false "verified" word) | **SafeDigest** | **Soften header now**; queue DISCHARGE of constantTimeReflexive / Symmetric / verifyTransitive in Phase 3 (the `@ Assumed:` migration in §5 is the bridge). |
| **High** (security-adjacent, undischarged Prevents:) | SafeArchive, SafeCBOR, SafeBloom | **Soften header now**; queue DISCHARGE in Phase 3 (Zip-Slip predicate, CBOR overflow/bomb, Bloom no-false-negative). |
| **Medium** (algorithmic-contract claims, lower blast radius) | SafeConsensus, SafeLRU, SafeMatrix, SafeSet, SafeRational, SafeGraph, SafeProbability, SafeTree, SafeDecimal, SafeMCP, SafeOAuth | **Soften header now**; queue per-module Proofs.idr authoring in Phase 4 (open-ended). |
| **Long-tail** (~55 modules, not yet enumerated in PROOF-NEEDS.md) | extrapolated from adversarial sample | **Enumerate in Phase 2**; default to "soften header" pending per-module read. |

**"Soften header" specifically means** rewriting `formally verified` →
`validator implementation` and `Prevents: X` → `Aims to prevent X via the X
predicate; see PROOF-NEEDS.md for the discharge status of the corresponding
theorem`. This is a one-line-per-file edit and a one-PR-per-batch task. It is
*not* a proof; it is a truthfulness fix that brings the doc headers in line
with the ledger.

**Status (2026-05-27):** softened in this same PR for 21 modules — SafeDigest,
SafeArchive, SafeCBOR, SafeConsensus, SafeLRU, SafeBloom, SafeMatrix, SafeSet,
SafeRational, SafeGraph, SafeProbability, SafeTree, SafeDecimal, SafeMCP,
SafeOAuth, SafeWebAuthn, SafeWebSocket, SafeWebhook, SafeCapability, SafeJWK,
SafeSecretShare — plus a targeted softening on SafeAttestation (its companion
`Proofs.idr` actually discharges structural enum / parser / projection
invariants, so only the "self-verifying hash" claim needed weakening). Each
header now points readers at `PROOF-NEEDS.md`, names what is discharged in
the companion `Proofs.idr` where one exists, and labels the remaining claims
as OWED. The PROOF-NEEDS.md ledger entry for SafeAttestation in the
"sibling-audited still owed" list is stale (its Proofs.idr is `Zero
believe_me / Zero idris_crash, zero OWED`) and will be removed in a
follow-up.

---

## 8. Work plan — Phase 2 (this triage, ~3 weeks)

**Goal:** every OWED site has a per-site disposition in §4 (status flips from
`[ ]` to `[x]`), and the witness-only long tail in §7 is fully enumerated.

**Day 1 (test-infrastructure wireup — derisks all later PROPERTY-TEST work).**
- Create `tests.ipkg` listing the 29 existing `tests/properties/*Props.idr`
  files plus their `src/Proven/*` dependencies. (The directory already
  exists with 606 `prop_X` declarations; 519 are discharged `= Refl`, 87
  are holes `= ?prop_X_rhs`. None of this is currently CI-checked.)
- Wire `idris2 --build tests.ipkg` into `idris2-ci.yml` as a required job.
- Update `Justfile` `test-idris` recipe to prefer `tests.ipkg` over the
  FFI-only `test-simple.ipkg` fallback.
- Triage all 87 holes: where `= Refl` discharges, write it; where it does
  not, convert to the OWED-stub form (`0 prop_X : <claim>` + `||| OWED:`
  doc comment cross-referenced to the source-side OWED if one exists).
  Land same PR as the wireup so CI is green from the start.

**Day 2 (SafeDigest @ Assumed migration — done pre-Phase-2, see proven#77).**
- Migrated the four `@ Assumed:` tags to Fork A `||| OWED:` + `0` form in
  proven#77. No Day 2 work remaining on this item.

**Day 3–10 (Tier A per-site reads — 210 sites).**
- For each of the 15 Tier A modules in §4: read every OWED block, confirm
  family classification, decide DISCHARGE vs PROPERTY-TEST vs OWED-AXIOM per
  site, record in this file as a per-module sub-table. Target pace: 2–3
  modules/day.

**Day 11–14 (Tier B per-site reads — totality-blocked).**
- File a totality-refactor ticket per Tier B module (SafeRegex, SafeTOML,
  …; SafeJson proven#81 is CLOSED). Per-site dispositions land as "DISCHARGE
  blocked on totality ticket #N".

**Day 15–18 (Tier C confirmation — 30+ OWED-AXIOM permanents).**
- For each OWED-AXIOM site, verify the doc comment names the external claim
  with library + version. Fix where missing.

**Day 19–21 (witness-only long tail).**
- Enumerate the ~55 long-tail single-file overclaim modules from §7. Produce
  a `PROOF-NEEDS.md` patch that promotes them out of "extrapolated" into a
  named list with severity assignments.

**Phase 2 exit criterion:** zero `[ ]` rows in §4; zero "extrapolated" rows
in `PROOF-NEEDS.md` overclaim section.

**Phase 2 does NOT discharge anything.** Discharge is Phase 3.

---

## 9. Work plan — Phase 3 (discharge, quarters)

Out of scope for this document beyond the sketch. Sequencing principle:
**all DISCHARGE work happens before any PROPERTY-TEST scaffolding**, because a
discharged theorem subsumes its property test; a property test stays in
place if the theorem turns out infeasible. The opposite ordering wastes
property-test effort.

Per-tier estimated discharge cost (calibrated against the Fork A spline of
~250 OWED annotations landed in May 2026):

- **Tier A DISCHARGE backlog** (Family 5, 6, 7 + finite-alphabet Family 1/2
  sites): ~70 sites, ~4–6 weeks of focused proof work.
- **Tier A PROPERTY-TEST backlog** (global Family 1 sites): ~80 sites, ~2–3
  weeks once generators are stood up.
- **Tier B (totality-refactor-then-discharge)**: per-module estimates land
  with each totality ticket.
- **Tier C (OWED-AXIOM permanents)**: zero discharge cost; revisit on each
  Idris2 release.

---

## 10. Maintenance

This file is regenerated, not edited line-by-line, when:

- A new Fork A OWED annotation lands (count drifts).
- An Idris2 release lifts a blocker family (re-classify Family `n` defaults).
- `PROOF-NEEDS.md` adds or removes a witness-only overclaim entry.

The grep recipes are pinned in §1 so any maintainer can re-run the inventory
and check this file's counts have not drifted. If they have, update §1 and
§4 in the same PR; do not let this file's numbers contradict the live grep.

---

## 11. Phase 2 completion (2026-05-27)

Phase 2 Days 1–21 landed across PRs #76 / #77 (the latter independent;
#76 bundles four pieces via in-branch merges of #78 / #79 plus the
follow-on commit that includes this section). Summary of outcomes per
day-band:

### Day 1 — Test wireup
- `tests.ipkg` + `idris2-ci.yml` install/build steps + Justfile +
  `tests/e2e.sh` + `docs/TESTING.md` updated.
- Orphan `tests/properties/` (29 files, 606 prop declarations) now
  CI-checked.
- 87 `prop_X = ?prop_X_rhs` holes converted to Fork A OWED stubs
  (`0 prop_X : Type` + `||| OWED:` doc).

### Day 2 — SafeDigest `@ Assumed:` migration
- 4 `@ Assumed:` tags migrated to Fork A `||| OWED:` + `0` form in #77.

### Days 3–10 — Tier A per-site reads (15 modules, ~209 OWED sites)
- Per-site disposition tables for all 15 Tier A modules in
  [`proof-debt-triage-tier-a.md`](proof-debt-triage-tier-a.md).
- Aggregate: ~59 DISCHARGE + ~7 DISCHARGE-after-totality + ~66
  PROPERTY-TEST + ~79 OWED-AXIOM.
- DISCHARGE backlog concentrated: SafeRegex (20) + SafeBase64 (10) +
  SafeEmail (10) = ~70% of immediate-discharge work.

### Days 11–14 — Tier B totality refactor tickets
- **proven#80** — SafePassword.Strength `windows`/`detectPatterns`/
  `analyzeStrength` covering→total refactor. Discharges 3 OWED sites
  (`strengthScoreBounded`, `higherImpliesLower`, `veryStrongSatisfiesAll`).
- **proven#81** ✅ **CLOSED** — SafeJson.Access `%default covering` → per-function
  `total` refactor. Discharged 2 OWED sites (`singleKeyPath`,
  `anyMatchesTAny`) in proven@33155ce.
- **proven#83** — SafeCrypto `modernIsSecure`/`standardIsSecure` Idris2
  0.9.0 dependency tracker (NOT a totality issue; case-rewrite
  eta-expansion in elaborator). Discharges 2 OWED sites once 0.9.0 lands.
- Cross-references added to tier-a doc rows for the 7 sites tracked
  across these 3 issues.

### Days 15–18 — Tier C OWED-AXIOM library/version audit
- All **79/79 OWED-AXIOM sites** in Tier A modules **pass audit**: each
  doc comment names its library (Idris2 0.8.0 + specific dependencies),
  pins a version, and gives an actionable discharge condition.
- One minor doc-comment clarification landed: `SafeCSRF/Proofs.idr:79`
  `constantTimeEqSym` now explicitly names `Data.Char.eqCharSym` as the
  awaited reflective lemma.

### Days 19–21 — Witness-only long-tail enumeration
- Adversarial-grep + per-module inspection of the 133 single-file
  `src/Proven/Safe*.idr` modules surfaced **6 additional confirmed
  overclaim modules** beyond the 13 already enumerated in
  `PROOF-NEEDS.md`:
  - **SafeSupplyChain** (HIGH — SLSA tamper-prevention)
  - **SafePBKDF2** (HIGH — KDF parameter validation)
  - **SafeProvenance** (MEDIUM — change tracking)
  - **SafePolicy** (MEDIUM — policy enforcement)
  - **SafeRegistry** (MEDIUM — OCI parsing)
  - **SafeSchema** (MEDIUM — schema evolution)
- 2 modules re-classified as **actually proven** (NOT overclaims):
  SafeTrust (`satisfiesMonotone` discharged) + SafeOrdering (already
  noted in PROOF-NEEDS.md adversarial sample).
- Headers softened for all 6 new overclaim modules in this PR (same
  pattern as the 22 softenings in #76).
- `PROOF-NEEDS.md` patched to add the 6 new entries to §"Owed" and to
  document the 3 cross-cutting overclaim patterns for batch-fix in
  Phase 3.

### Long-tail still extrapolated

The Days 19–21 enumeration was partial — Bash permission boundaries on
the audit sub-agent prevented full read-through of all 76 claim-bearing
single-file modules. On the order of **~54 modules remain extrapolated**
as overclaim candidates per the original 3% genuinely-proven sample
rate. These represent the residual Phase 2 work that would close the
extrapolation entirely. Three batch-fix templates in PROOF-NEEDS.md
let Phase 3 (or a follow-up Phase 2 session) process the long tail
mechanically.

### What remains uncovered by Phase 2

- The **29 OWED-bearing modules** outside the Tier A 15 (~70 OWED
  sites) listed in §4 as "(remaining 29 OWED-bearing modules)" did NOT
  receive per-site reads. These could be picked up as a Phase 2-bis
  pass or rolled into Phase 3 sequencing. The triage doc's original
  estimate of 70 sites across this tail (averaging 2-3 OWED per module)
  is unchanged.
- **Discharge** — no proof was actually discharged in Phase 2. Phase 3
  is the discharge campaign; sequencing recommendation in
  `proof-debt-triage-tier-a.md` § Cross-module observations.

### Net visible debt after Phase 2

- **280** Fork A OWED annotations in `src/Proven/*/Proofs.idr`
- **87** Fork A OWED stubs in `tests/properties/*Props.idr`
- **19** enumerated overclaim modules in PROOF-NEEDS.md (was 13;
  +6 from Days 19-21)
- **~54** long-tail single-file modules still extrapolated as
  overclaim candidates
- **2** GitHub issues open for Phase 3 unblocking (#80, #83; #81 CLOSED)

Total visible, grep-discoverable debt entries: **~384** (278 + 87 + 19).
The estate-wide trust-root ledger is now honest about every entry it
names; the residual ~54 extrapolated modules are honestly tagged as
extrapolated.
