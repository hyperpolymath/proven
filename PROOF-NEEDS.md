# PROOF-NEEDS.md — proven

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
| — of which are **stubs** (header only, no theorem) → treat as UNPROVEN | 2 (`SafeCommand` 8 ln, `SafeDateTime` 5 ln) |
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
SafeWebSocket, SafeWebhook, SafeCapability, SafeAttestation, SafeJWK,
SafeSecretShare.

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

> The remaining ~60 single-file modules in the 76 carrying a doc claim are
> owed on the same basis; the above are the audited exemplars. A full
> enumeration is mechanical: every `src/Proven/Safe*.idr` matching
> `Prevents:|formally verified|cannot crash|guarantee|no false negative|
> traversal|injection|attack` without a discharged theorem is OWED.

## Honestly proven (carried forward, pending Proofs.idr re-audit)

39 directories: SafeAPIKey, SafeArgs, SafeBase64, SafeCORS, SafeCSP, SafeCSRF,
SafeCSV, SafeContentType, SafeCookie, SafeCrypto, SafeEmail, SafeEnv, SafeFile,
SafeHSTS, SafeHTTP, SafeHeader, SafeHtml, SafeJWT, SafeJson, SafeMath,
SafeNetwork, SafeOTP, SafePassword, SafePath, SafeRBAC, SafeRateLimiter,
SafeRecord, SafeRedirect, SafeRegex, SafeSQL, SafeSRI, SafeSSRF, SafeSemVer,
SafeShell, SafeString, SafeTOML, SafeUrl, SafeXML, SafeYAML — **plus**
SafeOrdering (single-file, one discharged theorem). These are *claimed*
proven; their `Proofs.idr` bodies are not re-verified by this pass.

## Stubs — proof absence disguised as presence (CRITICAL)

| Module | State | Risk |
|--------|-------|------|
| **SafeCommand** | `Proofs.idr` 8 ln, header only; doc claims escaping/injection proofs that do not exist | CRITICAL — shell command construction; comment misrepresents verification |
| SafeDateTime | `Proofs.idr` 5 ln, header only | LOW — parse/format |

## What needs proving (priority)

1. **Stubs first** — populate or delete the misleading `SafeCommand` /
   `SafeDateTime` `Proofs.idr`; a stub is worse than an honest absence.
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
