# PROOF-NEEDS.md — proven

> Audit refresh 2026-05-18 (P1). The previous edition's "~30 modules / ~10 proofs"
> figure was inaccurate. Ground truth: **41 Safe\* modules carry a companion
> `Proofs.idr`**; **91 single-file `Safe*.idr` modules carry none**; and **2 of
> the 41 "proven" directories are empty stubs** (proof absence disguised as
> presence). This document is the corrected matrix.

## Method

- Proven = a `src/Proven/<Module>/Proofs.idr` exists *and* states at least one
  theorem (not just a `module` header).
- Stub = a `Proofs.idr` exists but contains only the module declaration and
  doc comment (no `: Type`, no theorem, no `Refl`). Counted as **UNPROVEN**.
- Unproven = single-file `src/Proven/<Module>.idr` with no `Proofs.idr` at all.
  Many such modules embed local `data ...Proof` witness types inside the
  source file; that is an in-module invariant, **not** a discharged proof
  obligation and does not count as proven.
- ABI/FFI (`src/Proven/FFI/Safe*.idr`, `bindings/idris2/...`) are wrappers over
  the core; they inherit (do not independently establish) proofs.

## Summary counts

| Bucket | Count |
|--------|------:|
| Safe\* modules with a real `Proofs.idr` | 39 |
| Safe\* "proven" dirs that are stubs (treat as unproven) | 2 (`SafeCommand`, `SafeDateTime`) |
| Single-file Safe\* modules with **no** `Proofs.idr` | 91 |
| `Safety.idr` (meta/aggregate, not a Safe\* unit) | excluded |

## Proven modules (real Proofs.idr)

SafeAPIKey, SafeArgs, SafeBase64, SafeCORS, SafeCSP, SafeCSRF, SafeCSV,
SafeContentType, SafeCookie, SafeCrypto, SafeEmail, SafeEnv, SafeFile,
SafeHSTS, SafeHTTP, SafeHeader, SafeHtml, SafeJWT, SafeJson, SafeMath,
SafeNetwork, SafeOTP, SafePassword, SafePath, SafeRBAC, SafeRateLimiter,
SafeRecord, SafeRedirect, SafeRegex, SafeSQL, SafeSRI, SafeSSRF, SafeSemVer,
SafeShell, SafeString, SafeTOML, SafeUrl, SafeXML, SafeYAML  *(39)*

> Note: the canonical high-blast-radius injection/authn surfaces
> (SafeSQL, SafeJWT, SafeCORS, SafeCSP, SafeHeader, SafeShell) are **already
> proven** with substantial files (SafeSQL 331 LOC, SafeJWT 386, SafeHeader
> 318). The audit task's hypothesis that these were unproven is **refuted**.

## Stub directories — proof absence disguised as presence (HIGH concern)

| Module | State | Risk |
|--------|-------|------|
| **SafeCommand** | `Proofs.idr` = 8 lines, module header only; doc claims escaping/injection proofs that **do not exist** | CRITICAL — shell command construction; the comment actively misrepresents verification status |
| SafeDateTime | `Proofs.idr` = 5 lines, header only | LOW — parsing/format, not a primary attack surface |

## Unproven single-file modules — priority worklist (by blast radius)

### P0 — injection / auth / command surfaces (do first)

1. **SafeCommand** — *stub*; shell command/argument construction. Escaping
   correctness is the whole point of the module and is unproven. Highest value.
2. **SafePromptInjection** — LLM prompt-injection detection (285 LOC, 36
   decls); no proofs that detection is sound/total or that sanitisation is
   idempotent. Rapidly-evolving threat, estate-wide LLM exposure.
3. **SafeSSH** — SSH key/algorithm validation; `validateKey`,
   `isWeakAlgorithm`, `parsePublicKey` unproven. Authn trust root.
4. **SafeOAuth** — OAuth flow/token handling; authn surface.
5. **SafeWebAuthn** — WebAuthn assertion handling; authn surface.
6. **SafeJWK** — JSON Web Key parsing; feeds SafeJWT trust.
7. **SafeCert** — X.509 certificate validation; chain-of-trust surface.
8. **SafeDNS** — DNS name handling; SSRF/rebinding adjacency.
9. **SafePolicy** — authorization policy evaluation; access-control surface.
10. **SafeCapability** — capability tokens; access-control surface.

### P1 — secrets / crypto material / supply chain

11. SafeHKDF, 12. SafePBKDF2, 13. SafeSecretShare, 14. SafeDigest,
15. SafeChecksum, 16. SafeCryptoAccel, 17. SafeAttestation,
18. SafeProvenance, 19. SafeSupplyChain, 20. SafeTrust,
21. SafeRegistry, 22. SafeGit, 23. SafeDocker

### P2 — protocol / transport / state surfaces

24. SafeWebSocket, 25. SafeWebhook, 26. SafeMCP, 27. SafeStateMachine,
28. SafeTransaction, 29. SafeProcess, 30. SafePipe, 31. SafeSignal,
32. SafeTerminal, 33. SafeResource, 34. SafeRetry, 35. SafeCircuitBreaker,
36. SafeRateWindow, 37. SafeConsensus, 38. SafeSameSite, 39. SafeMediaType,
40. SafeSchema, 41. SafeInput, 42. SafeLog, 43. SafeI18n, 44. SafeCron

### P3 — data structures / numeric / misc (lower blast radius)

SafeBitset, SafeSet, SafeHeap, SafeQueue, SafeTree, SafeGraph,
SafeUnionFind, SafeLRU, SafeBloom, SafeCycleDetect, SafeInterval,
SafeOrdering, SafeMonotonic, SafeSemaphore, SafeBuffer, SafeRational,
SafeComplex, SafeDecimal, SafeFloat, SafeFiniteField, SafeProbability,
SafeMatrix, SafeTensor, SafeUnit, SafeAngle, SafeColor, SafeGeo,
SafeCurrency, SafeUUID, SafeULID, SafeHex, SafeCBOR, SafeMarkdown,
SafeBibTeX, SafeTemplate, SafeArchive, SafeVersion, SafePhone,
SafeML, SafeISA, SafeFPGA, SafeGPU, SafeNPU, SafeTPU, SafeVPU,
SafeDSP, SafeCalculator  *(remaining)*

## Top target — proof-obligation sketch: SafeCommand

`SafeCommand` is the highest-value fix: it is a P0 shell-injection surface, its
`Proofs.idr` is a stub whose doc comment **claims** escaping/metacharacter
proofs that do not exist, and it already has property tests
(`tests/properties/SafeCommandProps.idr`) to cross-check intent. Model the file
on `src/Proven/SafeHTTP/Proofs.idr` (theorem-per-invariant, `%default total`,
`Refl` discharge of decidable claims, `export` signatures for the inductive
ones).

Proof obligations the populated `SafeCommand/Proofs.idr` must state (theorems
only — implementations are a separate task):

1. **Escape soundness (no quote breakout).**
   `escapedHasNoUnescapedQuote : (s : String) -> noUnescapedQuote (escapeArg s) = True`
   — the output of `escapeArg` contains no shell quote/metacharacter able to
   terminate the surrounding quoting context.

2. **Escape totality / no-loss.**
   `unescapeEscapeId : (s : String) -> unescape (escapeArg s) = s`
   — escaping is injective and round-trips; no argument content is dropped or
   reinterpreted (rules out smuggling via lossy escape).

3. **Command-name purity.**
   `validCommandHasNoMeta : (c : Command) -> isValidCommand c = True -> containsShellMeta (cmdName c) = False`
   — a constructed `Command`'s program name is free of `; | & $ \` ( ) < > newline`.

4. **Argument vector neutrality.**
   `builtArgsNeutral : (b : CommandBuilder) -> all (\a => containsShellMeta a = False) (renderArgs (build b)) = True`
   — every argument in the built vector, post-escape, carries no active
   metacharacter (closes the SafeHTTP-style "injection prevention" obligation).

5. **Builder monotonicity (no privilege/scope escalation).**
   `addArgPreservesValidity : (b : CommandBuilder) -> (a : String) -> isValidCommand (build b) = True -> isValidCommand (build (addArg b a)) = True`
   — appending an argument cannot turn a valid command invalid *or* alter the
   resolved program (prevents argument-injection changing the executed binary).

6. **Empty / degenerate rejection (decidable, `Refl`-discharged, mirrors
   `SafeHTTP.emptyHeaderNameRejected`).**
   `emptyCommandRejected : mkCommand "" = Nothing`
   `nullByteArgRejected : escapeArg (singleton '\0') = ... -- rejected/sanitised`

7. **Cross-check with property suite.** Each theorem above must subsume the
   corresponding generator in `tests/properties/SafeCommandProps.idr` (the
   tests become corollaries of the proofs, not independent assurance).

Stretch (state, do not necessarily discharge now): a `data SafeToExec :
Command -> Type` witness with a single constructor requiring (1)+(3)+(4), so
downstream estate code can demand `SafeToExec c` at the FFI boundary the same
way `SafeSSH` exposes `StrongKey`/`ValidKey`.

## Recommended prover

**Idris2** — this *is* the Idris2 proof library. Use `src/Proven/SafeHTTP/Proofs.idr`
and `src/Proven/SafeSQL/Proofs.idr` as the structural templates.

## Priority

**CRITICAL** — proven is the estate-wide trust root. The most urgent items are
not "unproven" modules but the **two stub `Proofs.idr` files** (`SafeCommand`,
`SafeDateTime`) whose presence falsely signals verification, and the P0
single-file injection/authn modules led by `SafePromptInjection` and `SafeSSH`.
