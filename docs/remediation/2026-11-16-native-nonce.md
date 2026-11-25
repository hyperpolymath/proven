<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->
# Nonce, entropy and native bridge remediation

Historical validated snapshot: subsequent lifecycle ABI edits are drafted but
unvalidated. See `2026-11-16-native-abi-draft.md`; the artifact and counts below
refer to the preceding source state.

Date: 2026-11-16. Proven baseline:
`c13619f9a67a1b9e8234e22e22dae0940d2480cb`.
Branch: `fix/critical-proof-remediation-20261116`.
These Proven changes remain local and uncommitted. The new private bridge is
published separately at the signed commit below. No Proven PR, merge or release
was made; existing PR #236 was applied locally, not merged remotely.

## Newly created bridge

The configured historical bridge could not be recovered. Repository searches,
the local developer tree and coordinator history did not identify usable source.
The coordinator's commit
[79f080133c0a533ce705f914b353f5514b1c1b3d](https://github.com/hyperpolymath/nextgen-languages/commit/79f080133c0a533ce705f914b353f5514b1c1b3d)
removed the nonexistent language-bridges submodule, whose old gitlink was
`f4b8c707f76449136b0202cf5186a2cd6d95efa8`. Portfolio entries and a stale
developer-ecosystem symlink were not recoverable implementations.

At the user's explicit request, created
[hyperpolymath/language-bridges](https://github.com/hyperpolymath/language-bridges)
as a **private repository with new source**, not a restoration. Proven pins
`8f4b082c52508ab6276946d61b660e7d9b97f35f` in its submodule and native workflow.
GitHub reports the commit signature `verified: true, reason: valid`.

The implemented package covers Bits64, Int64 and NUL-free UTF-8 values,
reference retention/release, and unary generated RefC calls with trampolines.
Application logic remains in Idris. Four native bridge tests cover integer
boundaries, strings, malformed encodings/types and 4,096 ownership-transfer
iterations. A deliberately corrupted generated C return value must fail runtime
assertions. Total Idris fixture identity theorems are checked separately.
These are meaningful native tests, **not a formal memory-safety proof of Zig**.
Callers must not copy owning Value structs or fabricate pointers; shared RefC
reference access must be serialized. Upstream allocation failure can abort.

Tested configuration: Debian Linux x86-64, Idris
`0.8.0-b6cbc25d5` at full commit
`b6cbc25d5cd2ef2a250556266fd7b68dfc360f8b`, Zig 0.15.2.
Other platforms, other historical language bridges and a complete public ABI
are outside the implemented scope. No hosted bridge CI run is claimed.

## Counter collision repair and proof scope

The baseline silently truncated a Bits64 counter into four bytes:
counter 0 and counter 2^32 generated identical nonces under the same prefix.
`tests/remediation/BeforeNonce.idr`, compiled against unchanged baseline
source, reproduces that collision and exits 1.

`counterNonce` retains the eight-byte-prefix/four-byte-counter format but now
returns `RandomResult (ByteVec 12)`. It rejects counters above 2^32-1 with
`CounterExhausted`, then checks that decoding the candidate nonce equals the
original counter. A failed check returns `CounterEncodingFailure`.
Callers must handle these errors. This is an API compatibility change.

Total checked theorems in the **production module** establish:

- `counterNonceRoundTrip`: every accepted output decodes to its original counter.
- `counterNonceInjective`: equal accepted nonces imply equal counters.
- `counterNonceExhausted`: a true overflow guard produces the typed failure.
- `zeroCounterAccepted` and `lastCounterAccepted`: the endpoints actually succeed
  for every eight-byte prefix, so the API is not vacuously rejecting all inputs.

`SafeCrypto.Proofs.counterNonceUnique` now derives distinct accepted outputs
from these theorems; the old false unrestricted declaration is removed.
The proof obtains equality from the production decision procedure, rather than
assuming an unproved bit-shift identity. Idris' trusted primitive equality,
compiler and runtime remain assumptions. Full-domain success is tested over
selected counters, not proved for every in-range input.

This is a pure codec fix. Persistent counter allocation, concurrency, crash
recovery, key/prefix rotation and prevention of repeated calls with the same
counter remain **pending lifecycle obligations**.

## Entropy containment

Four bodyless random primitives and the fake shuffle are removed. Operations
requiring entropy return explicit failure. Oversized byte requests and invalid
integer ranges retain distinct errors. Nontrivial shuffles fail; empty/singleton
shuffles, zero-element samples and whole-collection selection are deterministic.
Whole-collection selection does not promise randomized order.

`SafeCrypto.RandomContainment` supplies generic total no-success proofs for
bytes, naturals, integers, doubles, tokens, UUIDs, fresh nonces, seeds and
nontrivial shuffles, plus positive deterministic cases. Token/UUID/nonce/seed
wrappers propagate unavailability. This **does not implement a CSPRNG** and does
not repair other hash, signature, password or entropy-mixing obligations.

## Executed evidence

Evidence directory:
`/home/hyperpolymath/.cache/codex-audits/proven-remediation-evidence-2026-11-16/nonce-entropy`.

| Check | Observed result |
| --- | --- |
| Source-only focused proof/regression script | PASS: 519 earlier + 5,174 JWT/SQL + 6,259 nonce/entropy assertions = 11,952 distinct asserted cases |
| Same nonce/entropy runtime suite via RefC | PASS: the 6,259 cases repeated on a second backend; not additional distinct cases |
| Actual SQLite receiver | PASS: 405 exact-byte cases plus count and sentinel checks |
| Negative proof fixtures | PASS: all seven rejected, including forged entropy and counter-overflow acceptance |
| Baseline nonce probe | Expected failure: nonce(0) equals nonce(2^32) |
| Common entropy probe | Current source compiles without holes and passes five rejection checks; the unchanged baseline reports all four unimplemented primitive holes and is deliberately not executed |
| Main package | PASS: incremental build of the current 313-module package; new affected proofs also checked in fresh staging |
| New bridge native suite | PASS: four tests plus the corrupted-generated-result negative control |
| Actual Proven through RefC/Zig | PASS: two tests covering 11 nonce boundaries and six entropy sizes |
| Missing native inputs | PASS: absent options, empty sources and nonexistent sources rejected with expected diagnostics |
| Production RefC staging | PASS: one fresh 283,672-byte generated C file |
| Full native library | PASS: actual static artifact builds; nine existing Zig tests pass |
| PR #236 gate controls | PASS: 15 symbol controls and four RefC staging controls |
| Actual Rustler symbol gate | **FAIL as expected:** 66 required, 18 defined names with proven_ prefix, 61 missing; only five required names overlap |

The extra prefixed artifact name includes the renamed generated program entry;
it is not another implemented Rustler API. The static artifact SHA-256 is
`2e7a9fa02993cd022aee0f42c76e93f0e5347a9a4106745784d0e69433e0de2d`.
It is a local Debug artifact fingerprint, not a reproducible release attestation.

Authoritative logs: `regressions-passing.log`, `baseline-runtime.log`,
`library-build.log`, `proven-refc-build.log`,
`proven-native-regressions.log`, `proven-zig-build-current.log`,
`actual-native-abi.log`; bridge evidence is `bridge-native-tests.log`.
The additional common entropy probe is recorded in
`entropy-baseline-build.log`, `entropy-current-build.log` and
`entropy-current-runtime.log`; gate controls are in `native-gate-controls.log`.
Earlier unsuccessful compile logs are retained as history and are not PASS
evidence. The passing suite moves large test sizes to runtime to avoid compiler
constant-evaluation blowups. No assertion failures are ignored.

## Delivery limits and next work

The public production RefC package still covers Core/SafeUrl; the nonce adapter
is an explicit test target which imports the actual production Random module.
The public Zig operations still bypass Idris. The library's nine tests do not
prove Idris provenance. Rustler requests 61 absent functions, its proven_init
return type conflicts with Zig, and it requests a shared library while this
build emits a static archive. **Full native integration remains incomplete.**

PR #236's symbol/staging gates are incorporated locally. The native workflow
pins both the new bridge and tested compiler, exercises the real nonce/entropy
path, then builds and checks the genuine full artifact. The current ABI gate
will fail until the remaining interface is implemented. Existing
`LANGUAGE_BRIDGES_TOKEN` secret metadata is present; its contents/access were
not inspected or replaced. No hosted Proven CI pass is claimed.

Next priorities: one versioned native contract, correct initialization and
allocator ownership, Idris-backed exported functions and actual typed consumers;
then real OS entropy and cryptography with failure injection, vectors and
stateful nonce/authentication proofs. The broader SQL/374-entry audit worklist
remains in the remediation plan. No Rust code was added in this batch; planned
Creusot/Pearlite and complementary Verus wrapper work remains pending.

Reproduce with the pinned bridge checked out and the tool paths configured:

```sh
bash scripts/test-critical-remediation.sh
bash scripts/test-remediation-bridge.sh
bash tests/check-native-nif-exports.sh
bash tests/check-refc-staging.sh
bash scripts/build-refc.sh --verbose
# Full artifact build recipe: docs/ffi-ci-build.adoc
# Expected to reject the current incomplete artifact:
bash scripts/check-native-nif-exports.sh ffi/zig/zig-out/lib/libproven.a
```
