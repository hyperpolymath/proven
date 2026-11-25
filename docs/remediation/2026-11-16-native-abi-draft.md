<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->
# Native ABI lifecycle batch — validation pending

**Status: source/proof/test drafts only. No compiler, prover or native test has
run for this batch.** The user directed lightweight work while BotMania holds
heavy validation for the shared Windows/WSL memory queue. No slot has been
granted to this batch. Earlier nonce/entropy/native results are historical
evidence for the previous source snapshot, not a pass for these changes.

Base/branch remain `c13619f9a67a1b9e8234e22e22dae0940d2480cb` /
`fix/critical-proof-remediation-20261116`. The private bridge pin remains
`8f4b082c52508ab6276946d61b660e7d9b97f35f`; no bridge changes or publications
are part of this batch. All new Proven changes remain uncommitted.

## Concrete defects and drafted repairs

| Defect | Drafted production behavior | Evidence prepared; all execution pending |
| --- | --- | --- |
| C/Rust/Rustler declare init as int32_t, Zig returns a heap handle | Canonical init returns signed i32 status; legacy example allocation moves to proven_handle_create | Zig compile-time function type assignments; C consumer includes the owning header; Rust consumer includes the actual ffi.rs declarations |
| Consumers call readiness with no arguments, Zig dereferences a handle argument | Argument-free bool readiness and void deinit; serialized native state changes | Repeated init/deinit and four-thread C interleavings; exact ABI 2 check before typed calls |
| Native readiness had no executed Idris transition contract | Full build calls production Proven.ABI.Lifecycle.lifecycleCommand through the pinned bridge | Generic wire/state refinement, idempotence, start/stop and malformed-request proof bodies; runtime protocol cases and two forged-proof fixtures |
| Standalone library could imply native initialization succeeded | Standalone init returns -99 and readiness stays false | Same C consumer linked against the actual standalone archive in unavailable mode |
| last_error claimed borrowed storage while allocating each call | Only comptime NUL-terminated message literals are stored; getter returns their borrowed immutable pointers | Repeated-pointer and text assertions; no claim of a separate formal Zig heap/lifetime proof |
| Rustler requests a shared library but full build emits only an archive | Build graph installs actual static and shared libproven artifacts | Separate real C links/runs for each; Rust dynamic consumer; preserve full symbol gate on both |

ABI version is bumped to 2 because the old Zig signatures were incompatible.
The corrected signatures already match the intended public C/Rust/Rustler
declarations. Consumers of the old example handle API must migrate to distinct
handle names; proven_free still frees those legacy handles. Those example
operations, handle lifetime safety and placeholder successful operations are not
certified by this repair.

The lifecycle protocol accepts four state/command messages. It returns a stopped
or started state; all other requests return a rejection code. Zig marshals the
current state and command, calls the actual generated function, checks the reply
and updates readiness under a mutex. It releases RefC references before unlocking.
Invalid backend replies clear readiness and return validation failure. Deinit
has no C error channel and fails closed to unready. This is not a sweep of
caller-owned buffers, an implementation of OS entropy, or proof that all other
header declarations are supported.

## Proof scope and trust boundary

`src/Proven/ABI/Lifecycle.idr` contains total proof bodies, without new axioms:

- `wireRefinesTransition` quantifies over every state/command pair and targets
  the **same noinline function called from the production Zig adapter**.
- `transitionIdempotent`, `startMakesReady` and `stopMakesUnready` characterize
  the actual transition function.
- `malformedRequestRejected` connects four failed decoding tests to rejection.

These bodies are **not yet checked** and are not counted as established proofs.
After checking, their scope is still Idris policy/refinement. They do not prove
Zig mutex implementation, reference-count safety, the C calling convention or
the compiler. Those boundaries require the prepared real native tests and
explicit assumptions, rather than a disconnected substitute model.

No production Rust implementation was changed. The new Rust test imports the
existing raw declarations; it is not a Rust formal proof. Creusot implementation
verification with Pearlite contracts for the existing Rust lifecycle/error/
ownership wrappers remains pending. Complementary Verus state/handle obligations
also remain pending, with external C/Zig/RefC refinement explicitly assumed or
unresolved. No tool run or solver result is claimed for either verifier.

## Prepared validation

`scripts/test-native-lifecycle.sh` creates fresh source staging and sequences:

1. Total proof checking, executable Idris transition cases and two intentionally
   invalid proof fixtures.
2. Actual production package RefC generation, with error/hole diagnostics rejected.
3. One-worker static/shared builds and Zig tests; standalone build/tests.
4. Header-typed C calls against each real artifact and a Rust consumer using
   the real declarations. The C test includes four short concurrent callers;
   no separate validation jobs run alongside it.
5. A negative control that corrupts only the generated lifecycle result, rebuilds
   an isolated library, and requires the already-linked C consumer to fail its
   init-status assertion. A second run checks explicit backend failure/unready
   behavior. Loader/linker failures do not satisfy this control.

Toolchain identities, compiler diagnostics, test output, actual artifacts and
checksums are retained in a unique evidence run directory. No toolchain package
is installed. Temporary source trees are script-owned and regenerable.

The workflow is drafted to run these scoped checks, then the **separate full
Rustler symbol gate on both artifacts**. It still fails if any required symbols
are missing. There is no hosted run or publication.

Lightweight checks completed: shell syntax via bash -n and git diff --check.
Formatting, totality/type checks, code generation, C/Rust compilation, static/
shared linking, runtime tests, corruption controls and current artifact symbol
counts are all **NOT RUN**. The previous artifact had 61 missing Rustler symbols;
do not present that historical inventory as a count of the unbuilt draft.

## Next work and admission

The exact serial validation command, ownership, unknown memory estimate,
20-minute duration bound, cancellation behavior and pass criteria are recorded
in the handoff pack's CHECKPOINT.md. Wait for an explicit slot grant; another
process ending is not permission. Do not interrupt unrelated work or start a
replacement for an ambiguous validation run.

After the lifecycle batch passes, continue implementing Idris-backed public
operation families with typed result/error/ownership contracts. Outstanding
issues include Rustler initialization-failure propagation, enum-valued FFI
status fields, string decoding/error ownership, missing functions, broader
platforms and genuine cryptographic backends. Whole-query SQL and the broader
proof-debt worklist remain open.
