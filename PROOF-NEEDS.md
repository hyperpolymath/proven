# PROOF-NEEDS.md — proven

> **Re-audit update 2026-05-20.** The 39 "carried forward, pending Proofs.idr
> re-audit" directories have now been re-audited (hyperpolymath/standards#158
> Fork A). Net finding: of those 39, **6 are clean** (no bodyless decls;
> SafeChecksum, SafeBuffer, SafeBloom, SafeCryptoAccel, SafeHKDF, SafeFPGA —
> these set the OWED convention), **28 carried bodyless decls** which have
> now been annotated as `||| OWED:` + `0 ` erased-multiplicity per the
> convention (PRs hyperpolymath/proven#37-64, all DRAFT pending estate CI
> clear; 8 locally idris2-0.8.0-verified). Total: 250 bodyless decls
> surfaced and made discoverable. See the new "OWED-with-justification
> convention" section below and `.machine_readable/6a2/META.a2ml` ADR-001.
>
> **Honesty refresh 2026-05-18.** `proven` is the estate trust root, so this
> ledger must not overclaim. The prior edition asserted "the most complete
> Idris2 proof infrastructure in the ecosystem" and "~30 modules lack proof
> files". Adversarial re-audit shows that framing was **dishonest by a wide
> margin**: the real gap is ~3x larger, and dozens of modules ship verbatim
> security claims in their doc headers (`Prevents:`, `formally verified`,
> `guaranteed`, `cannot crash`) with **zero discharged theorem**. This is the
> same silent-overclaim class the sibling security audit found in SafeMCP /
> SafeOAuth / SafeWebAuthn / SafeWebhook / SafeCapability / SafeAttestation /
> SafeJWK / SafeSecretShare / SafeWebSocket. This edition enumerates the debt
> honestly. Refs hyperpolymath/standards#124.

## Definitions (what counts)

- **Proven** = a discharged theorem exists: either `src/Proven/<M>/Proofs.idr`
  states and discharges ≥1 theorem (`Refl` / `with`-block / `impossible` /
  `absurd` / decidable `Dec`), **or** the single-file `src/Proven/<M>.idr`
  contains such a discharged theorem inline.
- **Witness-only (OVERCLAIM, marked ✗)** = the module ships a `Prevents:` /
  `Guarantees:` / `formally verified` / `cannot crash` / `no false negative`
  doc claim (or `data ...Proof` witness types / proof-carrying record
  *fields*) but **no discharged theorem**. A `data` witness type is an
  *obligation a constructor must satisfy*, not a discharged proof. A record
  field of proof type is an obligation pushed onto the caller, not discharged
  here. A signature with `@ Assumed:` in its doc is an explicit postulate.
- **Claim-free** = the module makes no safety/security promise; absence of
  proof is honest.
- ABI/FFI wrappers (`src/Proven/FFI/Safe*.idr`, `bindings/...`) inherit, never
  independently establish, proofs.

## Honest counts

| Bucket | Count |
|--------|------:|
| Safe\* directories with a companion `Proofs.idr` | 41 |
| — of which are **stubs** (header only, no theorem) → treat as UNPROVEN | **0** (`SafeCommand` discharged by proven#21; `SafeDateTime` discharged on branch `proof-debt/standards-132-safedatetime-stub`) |
| Single-file `src/Proven/Safe*.idr` modules (no `Proofs.idr` dir) | 133 |
| — single-file modules shipping a safety/security **doc claim** | **76** |

## Security-critical decidable proofs landed (2026-05-18)

The following security-critical modules previously shipped only inline
witness *types* (`StrongKey`, `StrongCert`, …) with a "Prevents: …"
module-doc claim but **no discharged theorem** that the predicate
rejects the bad case. Real verified `Proofs.idr` files now exist
(`idris2 --check` exit 0), modelled on `SafeCommand/Proofs.idr`:

- **SafeSSH/Proofs.idr** — exhaustive per-constructor `Refl`: DSA
  provably rejected by `isWeakAlgorithm`/`validateKey`; `StrongKey`
  uninhabitable for DSA. Pure enum, **no bridge axiom**.
- **SafeCert/Proofs.idr** — SHA-1 provably weak, RSA-2048 provably not
  strong; `StrongCert` uninhabitable for SHA-1 / RSA-2048; temporal
  expiry guard. Pure enum, **no bridge axiom**.
- **SafePromptInjection/Proofs.idr** — per-char delimiter-escape
  soundness + canonical attack-vector detection + `RejectUnsafe`
  enforcement; one explicit named erased string bridge axiom only.

Still owed (single-file, witness-type-only, "Prevents" doc claim
undischarged — honest ledger): SafeMCP, SafeOAuth, SafeWebAuthn,
SafeWebSocket, SafeWebhook, SafeCapability, SafeJWK, SafeSecretShare.

`SafeAttestation` was previously listed here but its companion
`src/Proven/SafeAttestation/Proofs.idr` (189 ln, `idris2 --check` exit 0,
"Zero `believe_me` / `idris_crash`, zero OWED") discharges enum
self-equality, parser rejection of unknown / weak algorithms, and
record-projection anchors. The remaining hash-recomputation claim is
OWED at the FFI seam (the actual hash computation happens in
`Proven.SafeCrypto`); the module header was softened in proven#76 to
reflect this. It therefore belongs in the "Security-critical decidable
proofs landed" section above, not in this still-owed list.

Companion `Proofs.idr` content was *not* re-verified theorem-by-theorem in this
pass; the 39 non-stub directories are carried forward as claimed-proven pending
a separate Proofs.idr discharge audit. **The single-file population is the
exposure this ledger now owns honestly.**

### Adversarial sample of the single-file non-security population

32 modules sampled across the alphabet (SafeAngle…SafeUnit), excluding the 12
modules the sibling security audit handled and SafeCommand/SafeDateTime:

| Outcome | Count | Fraction |
|---------|------:|---------:|
| Genuinely proven (≥1 inline discharged theorem) | **1** | ~3% |
| Witness-only / doc-claim **OVERCLAIM (✗)** | **13** | ~41% |
| Claim-free (honest silence) | **18** | ~56% |

- Only **SafeOrdering** carries a real discharged theorem
  (`seqTotalOrder`, `with`-block, `Refl`, `%default total`). Its
  `PartialOrder`/`TotalOrder` records additionally push proof *fields* onto
  callers (obligations, not discharged here).
- **Extrapolation:** with 76/133 single-file modules carrying a safety/security
  doc claim and the sampled proven-rate ≈3%, on the order of **~70 single-file
  modules are silent overclaims** — the prior ledger acknowledged ~30 *gaps*
  total and never used the word "overclaim". Ledger accuracy verdict: the
  origin/main edition was **dishonest — it understated the unproven gap by
  roughly 3x and entirely omitted the doc-claim-without-theorem class.**

## Owed — single-file modules shipping a security claim with NO theorem (✗)

Each line: module — verbatim doc claim — why it is owed.

### Security-relevant overclaims (highest concern)

- **SafeDigest** ✗ — `src/Proven/SafeDigest.idr:5` *"formally verified digest
  parsing, validation, and constant-time comparison to prevent timing
  attacks."* — WORST OFFENDER: claims "formally verified" yet
  `constantTimeReflexive`/`constantTimeSymmetric`/`verifyTransitive`
  (`:225–248`) are **explicitly undischarged** (`@ Assumed:` in their own
  docstrings). Crypto material + actively false "verified" word.
- **SafeArchive** ✗ — `src/Proven/SafeArchive.idr:5` *"Provides type-safe
  archive member validation that prevents: Zip Slip / Symlink attacks / Zip
  bombs / Path injection via null bytes."* — `hasPathTraversal` is a `Bool`
  predicate with no theorem that a non-flagged entry is traversal-free. Supply-
  chain / unpack surface.
- **SafeCBOR** ✗ — `src/Proven/SafeCBOR.idr:7` *"Prevents: integer overflow,
  indefinite-length bombs, tag confusion."* — used in COSE/WebAuthn/FIDO2 per
  its own header; no overflow/bomb/tag theorem. Authn-adjacent.
- **SafeConsensus** ✗ — `src/Proven/SafeConsensus.idr:9` *"Log replication with
  consistency guarantees"* — no safety/agreement theorem. Distributed-state
  surface.
- **SafeLRU** ✗ — `src/Proven/SafeLRU.idr:6` *"that cannot overflow or corrupt
  cache state."* — no invariant theorem; cache-poisoning adjacency.

### Lower-blast-radius overclaims (still owed)

- **SafeBloom** ✗ — `:6` *"with guaranteed no false negatives."* (the entire
  correctness contract of a Bloom filter; undischarged)
- **SafeMatrix** ✗ — `:3` *"matrix operations that cannot crash"*
- **SafeSet** ✗ — `:3` *"set operations that cannot crash"*
- **SafeRational** ✗ — `:6` *"safe operations that cannot crash."*
- **SafeGraph** ✗ — `:197` *"Bounded by fuel … to guarantee termination"*
  (asserted, not proven)
- **SafeProbability** ✗ — `:20` *"A probability value guaranteed to be in
  [0, 1]"* (no smart-constructor refinement proof; `impossible` is a *value*
  named impossible, not a proof)
- **SafeTree** ✗ — `:6` traversal/manipulation safety, no theorem
- **SafeDecimal** ✗ — `:54` "Structurally decreasing on scale ensures
  totality" (relies on `%default total` only; no stated lemma)

### Long-tail enumeration (Phase 2 Days 19-21, partial — 2026-05-27)

Adversarial-grep + per-module inspection of the long tail surfaced **6
additional confirmed single-file overclaims** beyond the 13 enumerated
above. Headers softened in proven#76 (follow-on commit; see
`docs/proof-debt-triage.md` §11):

- **SafeSupplyChain** ✗ HIGH — `src/Proven/SafeSupplyChain.idr:7`
  *"Prevents: tampered builds, unattested artifacts, provenance forgery."*
  — SLSA attestation layer; tamper-prevention theorems OWED. Real-world
  supply-chain blast radius.
- **SafePBKDF2** ✗ HIGH — `src/Proven/SafePBKDF2.idr:6` *"Prevents: low
  iteration counts, short salts, weak derived key lengths."* — crypto
  parameter validator; prevention theorems OWED.
- **SafeProvenance** ✗ MEDIUM — `src/Proven/SafeProvenance.idr:4`
  *"Formally verified change tracking and audit trails"* — provides
  records + predicates only; integrity/lineage theorems OWED.
- **SafePolicy** ✗ MEDIUM — `src/Proven/SafePolicy.idr:4` *"Formally
  verified policy enforcement"* — AST-level predicates only; enforcement
  soundness theorems OWED.
- **SafeRegistry** ✗ MEDIUM — `src/Proven/SafeRegistry.idr:5-6`
  *"formally verified parsing of OCI/Docker image references with
  guarantees of correctness and termination"* — parsers via Bool
  validators; correctness/termination theorems OWED.
- **SafeSchema** ✗ MEDIUM — `src/Proven/SafeSchema.idr:4-10` *"Formally
  verified schema evolution"* + *"compatibility proofs"* +
  *"correctness guarantees"* — type definitions + migration scaffolding;
  compatibility/correctness theorems OWED.

**Confirmed actually-proven** (re-classification, NOT in the OWED list):

- **SafeTrust** — `src/Proven/SafeTrust.idr:296-303` `satisfiesMonotone`
  discharged via exhaustive pattern-match + `Refl` per arm. The header's
  "Formally verified" / "proven monotone" claim IS valid.
- **SafeOrdering** — already noted in §"Adversarial sample" as the only
  single-file module in that sample with a discharged theorem.

### Long-tail still extrapolated

The Phase 2 Days 19-21 audit surfaced 6 new instances but did NOT
exhaustively read all 76 claim-bearing single-file modules (Bash
permission boundaries on the audit sub-agent prevented full enumeration).
On the order of **~54 single-file modules remain extrapolated** as
overclaim candidates per the original 3% genuinely-proven sample rate.
A full enumeration is mechanical: every `src/Proven/Safe*.idr` matching
`Prevents:|formally verified|cannot crash|guarantee|no false negative|
traversal|injection|attack` without a discharged theorem (no `Refl` /
`Dec` / `impossible` / `absurd` / `with` block) is OWED. Phase 3 may
batch-process these via the three cross-cutting overclaim patterns
identified in the Days 19-21 audit:

1. **"Formally verified" without evidence** — batch-fix template:
   `formally verified X` → `X via Bool predicates; theorems OWED — see
   PROOF-NEEDS.md`.
2. **"Prevents: X" without prevention theorem** — batch-fix template:
   `Prevents: X` → `Aims to prevent (via Bool predicates; soundness
   theorems OWED — see PROOF-NEEDS.md): X`.
3. **"guarantees" / "correctness" / "termination" prose claims** —
   batch-fix template: remove adjectives + add OWED pointer.

## OWED-with-justification convention (adopted 2026-05-20)

Bodyless type signatures in `Proofs.idr` files are *implicit postulates* —
Idris2 parses them as axioms with no proof body. To make the trust posture
visible (to readers and to grep), each bodyless declaration carries:

```idris
||| OWED: <one-sentence statement of what's claimed>
||| Held back by <specific Idris2 0.8.0 blocker>. Discharge once
||| <what would unblock it>.
0 declarationName : Type
```

Three parts: triple-pipe doc-comment with claim+blocker+discharge condition;
leading `0` (Idris2 quantitative-type-theory erased-multiplicity marker —
runtime-erased); original bare type signature.

**Do NOT use the `postulate` keyword.** Zero `Proofs.idr` files in `proven`
use it. The OWED+`0`+bare-sig pattern is chosen so that the reason each
obligation exists is discoverable, and erasure means proof gaps cannot
silently affect runtime behaviour.

Canonical example: `src/Proven/SafeChecksum/Proofs.idr` (L24-100). Also see
SafeBuffer, SafeBloom, SafeCryptoAccel, SafeHKDF, SafeFPGA — these landed
2026-05-20 and set the convention.

### Blocker families surfaced in the 2026-05-20 audit

| Family | Typical shape | Discharge route |
|--------|---------------|-----------------|
| String FFI opacity | claims about `unpack`/`pack`/`ord`/`toLower`/`prim__eq_String` | Typed String/Char primitive layer, or Class-J axiom set parallel to gossamer/boj-server |
| Numeric-literal Refl gaps | `Bits32`/`Bits64`/large-`Nat` literal equality | `Data.Bits` reflective tactic or `Integer`-backed bound |
| Covering-not-total reduction | e.g. `gcd n 0 = n` under `Data.Nat.gcd` declared `covering` (cf. SafeMath PR#46) | Upstream Idris2 stdlib promoting to `total`, or local total reimplementation |
| Foldl-predicate gaps | claims about `Data.List.all`/`any` on abstract lists | Cons-distribution lemmas inline (cf. proof-of-work PR#60) |
| Structurally OWED | e.g. SafeJWT `validatedJWTFromValidation` (record type lacks provenance proof field) | Type-side widening, not just FFI seam |

### Fork A scope vs. Fork B scope

Fork A (this campaign) = make every bodyless decl explicit with a justified
OWED note. Surfacing, not discharging. **Complete 2026-05-20** via PRs
hyperpolymath/proven#37-64.

Fork B (per-module discharge triage) is the next layer — selectively
proving the dischargeable subset. Quick-win candidates surfaced in the
audit: SafeCrypto `modernIsSecure`/`standardIsSecure` (3-line `isSecure`
refactor), SafePath already-Refl-able pairs (already done in PR#57). Other
modules require deeper triage.

## Honestly proven (carried forward, Proofs.idr re-audited 2026-05-20)

39 directories: SafeAPIKey, SafeArgs, SafeBase64, SafeCORS, SafeCSP, SafeCSRF,
SafeCSV, SafeContentType, SafeCookie, SafeCrypto, SafeEmail, SafeEnv, SafeFile,
SafeHSTS, SafeHTTP, SafeHeader, SafeHtml, SafeJWT, SafeJson, SafeMath,
SafeNetwork, SafeOTP, SafePassword, SafePath, SafeRBAC, SafeRateLimiter,
SafeRecord, SafeRedirect, SafeRegex, SafeSQL, SafeSRI, SafeSSRF, SafeSemVer,
SafeShell, SafeString, SafeTOML, SafeUrl, SafeXML, SafeYAML — **plus**
SafeOrdering (single-file, one discharged theorem). Their `Proofs.idr` bodies
were re-audited 2026-05-20 under standards#158 Fork A: of the 39 directory-form
modules, 6 are clean (zero bodyless decls), 28 had bodyless decls now
annotated as explicit OWED, 5 had only fully-discharged proofs already. No
silent postulates remain.

## Stubs — proof absence disguised as presence (CRITICAL) — *now empty*

Both audited stubs have been discharged (no remaining "header-only"
`Proofs.idr` in the audited set):

| Module | Was | Now |
|--------|-----|-----|
| SafeCommand | `Proofs.idr` 8 ln, header only — CRITICAL | proven#21 — 160 ln real injection-safety proofs, `idris2 --check` exit 0, no escapes |
| SafeDateTime | `Proofs.idr` 5 ln, header only — LOW | real `daysInMonth` band lemmas (28..31, non-zero) + `makeDate` smart-constructor soundness (`makeDate ... = Just dt -> dateGuard dt.year dt.month dt.day = True`), `idris2 --check` exit 0, no escapes — landed in this PR |

## What needs proving (priority)

1. **Stubs first** — done. Both audited stubs (`SafeCommand`,
   `SafeDateTime`) now carry genuine machine-checked theorems
   (proven#21 + this PR); the misleading "proof-absence-as-presence"
   class is closed for the audited set.
2. **Strip or discharge the security overclaims** — for every ✗ module either
   discharge the claimed theorem or downgrade the doc header to a non-promising
   description. Lead with **SafeDigest** ("formally verified" is actively
   false), then SafeArchive, SafeCBOR.
3. Sibling-audited security modules (SafeMCP, SafeOAuth, SafeWebAuthn,
   SafeWebSocket, SafeWebhook, SafeCapability, SafeAttestation, SafeJWK,
   SafeSecretShare) — see that audit; same OWED class.
4. Re-audit the 39 carried-forward `Proofs.idr` bodies (not done here).

## Recommended prover

**Idris2** — this *is* the Idris2 proof library. Use a non-stub directory
(`src/Proven/SafeSQL/Proofs.idr`, `SafeHTTP/Proofs.idr`) as the structural
template; mirror `SafeOrdering.seqTotalOrder` for inline single-file proofs.

## Priority

**CRITICAL** — proven is the estate-wide trust root. The dominant risk is not
"modules lacking proofs" but **modules asserting safety/verification in prose
that no theorem backs**. Until each ✗ is discharged or its doc claim retracted,
those headers must be treated as unverified marketing, not guarantees.
