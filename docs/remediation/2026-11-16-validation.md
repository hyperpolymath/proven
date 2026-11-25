<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
# First-batch remediation validation

This is the retained first-batch record. For the later JWT/SQL changes and the
current 312-module package build, see `2026-11-16-jwt-sql.md`. The counts below
describe the earlier run, not the expanded current regression suite.

Date: 2026-11-16. Base commit:
`c13619f9a67a1b9e8234e22e22dae0940d2480cb`.
Toolchain: Idris 2 `0.8.0-b6cbc25d5`, Linux/WSL Debian, Chez backend.

## Checked propositions

| Production operation | Checked theorem | Exact scope |
| --- | --- | --- |
| `validateProof prf goal` | `ECHIDNA.Soundness.echidnaSoundness` | Actual IsValid acceptance implies GoalHolds of that exact goal. |
| `validateComprehensive prf goal` | `comprehensiveSoundness` | Same semantic guarantee for the comprehensive entrypoint. |
| `validateProof prf (Eq Zero (Succ Zero))` | `rejectZeroEqualsOne` | No proof term can cause acceptance of the audited false equality. |
| `constantTimeStringCompare a b` | `Hash.stringCompareSound` | Acceptance implies equality of the full unpacked character sequences. |
| Same string comparison | `stringCompareReflexive`; `Comparison.charsEqualComplete` | Equal inputs are accepted; the implementation does not merely reject all inputs. |
| `verifySignature key payload sig` | `unimplementedSignatureRejected` | Every key, payload, algorithm and signature is rejected while the backend is absent. |
| `verifyWebhook key event now tracker` | `webhookNeverVerified`; `webhookPreservesTracker` | No Verified result and no tracker mutation for any inputs. This is containment, not an authentication proof. |
| Idris webhook export | `FFI.SafeWebhook.webhookExportRejects` | Every header maps to rejection at the Idris export; no assertion about a linked/deployed ABI. |

All new proof definitions have bodies and are checked under `%default total`.
They do not use bodyless proof declarations, Unit conclusions, believe_me,
assert_total or assert_smaller. The kernel defines denotational relations for
closed Nat/Bool terms and constructs equality evidence; it does not assume an
external prover is sound. Character comparison relies on Idris's standard
Char decidable equality and String unpacking semantics. Compiler/runtime and
primitive correctness remain part of the trusted toolchain.

The old vacuous Soundness API has been removed. ECHIDNA functions outside the
supported ground-equality fragment reject certification. Syntax diagnostics use
a distinct result type. These are intentional API and behavior changes.

## Executed checks

- `IDRIS2=/home/hyperpolymath/.local/bin/idris2 bash scripts/test-critical-remediation.sh`:
  **PASS**, compiling the affected modules from a fresh source-only staging
  directory, checking generic proof signatures, executing **519 runtime
  assertions**, and rejecting **two negative compilation fixtures** for
  fabricated false-goal and webhook evidence.
- Regression sensitivity: all **519** runtime cases were compiled against
  an archive of the unchanged baseline and executed. **313 failed**, exit code
  **1**, including false-goal acceptance, Unicode truncation and forged webhook
  signatures/replay mutation. This is an executable before/after control.
  This includes the Idris webhook-export boundary checks. An intermediate
  WSL connection timeout was retried successfully.
- Full `idris2 --build proven.ipkg`: **PASS**, exit code 0, through build step
  **310/310**. The source-only checkout's first build was killed at step 208
  under concurrent host load; a resumed build completed using only that
  checkout's generated cache. Both logs are retained; this was not an
  uninterrupted single-process clean build.
- `git diff --check`: **PASS**.
- Gate negative control: **PASS**. A synthetic compiler printed an Error
  diagnostic but returned exit code 0; the runner rejected it with exit code 1.
  Positive compilation checks inspect diagnostics as well as process status.
- Legacy property suite: **still failing**. A fresh check of SafeMathProps with
  contrib/network exits 1 with stale/undefined APIs such as safeSub and invalid
  property types. This change does not claim that suite is repaired.
- Real Zig/native artifact: not validated by this batch; the audited
  language-bridges dependency and ABI discrepancies remain open.
- Hosted CI: configuration updated; no hosted run, push or release performed.

## Reproduction and evidence

Run `bash scripts/test-critical-remediation.sh` with Idris 2 0.8.0 and Chez
available. The script copies current source into an isolated temporary directory,
checks proof regressions, builds and executes the tests, and fails on any
unexpected success/failure in the negative fixtures. It does not consume an
installed proven library or a previous checkout's TTC cache.

The CI proof-check job invokes this script before its existing proof-module
checks. The main package now includes the kernel, validator, soundness module
and full-character comparator. CI path filters include the test runner.

Local detailed logs are retained outside the repository in
`/home/hyperpolymath/.cache/codex-audits/proven-remediation-evidence-2026-11-16/`.
The audit baseline and worklist are documented in
`2026-11-16-plan.md` and `proof-worklist-20261115.csv`.

Do not infer cryptographic authentication, timing resistance, production-native
refinement, complete logical expressiveness, or completion of the other audit
findings from these results.
