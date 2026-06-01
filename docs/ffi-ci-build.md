<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# FFI CI build: standalone surface (now) vs full Idris→Zig pipeline (deferred)

`proven`'s FFI layer compiles Idris 2 to C via the RefC backend and wraps it in
a stable C ABI with Zig (`Idris2 --codegen refc → .c → zig build → libproven`).
There are **two** Zig build entry points:

| File | Builds | External deps | Used by |
|------|--------|---------------|---------|
| `ffi/zig/build.zig` | full Idris→RefC→Zig `libproven` | `idris2_zig_ffi` (path dep on `nextgen-languages/language-bridges`) + generated RefC C | local dev, full integration |
| `ffi/zig/build_standalone.zig` | `libproven_ffi.a` from `src/main.zig` only (pure-Zig C-ABI surface) | none | **CI** (`e2e.yml`) |

## Why CI uses the standalone build (ADR-003)

`build.zig` declares its `idris2_zig_ffi` dependency as a Zig **`.path`**
dependency:

    .idris2_zig_ffi = .{ .path = "../../../nextgen-languages/language-bridges/bridges/idris2" }

That points at a **private, cross-owner sibling repo** which CI does not (and
cannot, without a credential) check out, so `zig build` fails at dependency
resolution. `build_standalone.zig` compiles `src/main.zig` alone — and that
file imports only `std`/`builtin` — so it needs neither the bridge nor the
generated RefC, giving CI a real (if narrower) Zig FFI build + unit-test signal.

`e2e.yml` therefore runs, with Zig provisioned by `mlugg/setup-zig@v1` at
`0.15.2` (the `build.zig.zon` minimum):

    zig build       --build-file build_standalone.zig      # E2E build
    zig build test  --build-file build_standalone.zig      # E2E tests
    zig build bench --build-file build_standalone.zig || … # Bench (soft)

## The full pipeline (recipe — Idris half verified locally)

The canonical recipe is the root `Justfile` (`build-refc` + `build-ffi`).
Verified locally on idris2-0.8.0 + contrib:

    # Step 1 — Idris → RefC C  (scripts/build-refc.sh, IPKG=proven-ffi.ipkg)
    idris2 --codegen refc --build proven-ffi.ipkg     # → build/exec/proven-ffi.c
    # staged to build/refc/ ; the script prints the four paths used below

    # Step 2 — Zig links the RefC C + Idris runtime/support into libproven
    PFX=$(idris2 --prefix)
    zig build \
      -Didris-refc="$PWD/build/refc" \
      -Didris-refc-runtime="$(find "$PFX" -path '*/support/refc' -type d | head -1)" \
      -Didris-c-support="$(find "$PFX" -path '*/support/c' -type d | head -1)" \
      -Didris-support-lib="$(find "$PFX" -path '*/lib/libidris2_support.*' | head -1 | xargs dirname)"

Requires `libgmp` (the RefC runtime links `gmp`/`pthread`/`m`).

## Enabling full integration in CI (deferred — #151)

The default `build.zig` needs `nextgen-languages/language-bridges` present.
Because it is **private** and **not** a submodule, CI cannot fetch it today.
To enable:

1. Add `language-bridges` as a **git submodule** inside `proven`, and point
   `ffi/zig/build.zig.zon`'s `.path` at the in-repo submodule location (the
   current `../../../…` escapes the repo, so it must change).
2. Add a **CI read credential** (deploy key / PAT) and use `actions/checkout`
   with `submodules: true`.
3. Add/extend a workflow that installs Idris2 + `libgmp`, runs
   `scripts/build-refc.sh`, then the `zig build -Didris-*` invocation above.

Tracked in #151. The interim standalone build is ADR-003.
