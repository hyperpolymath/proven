<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# FFI CI build: standalone surface and full Idris→Zig pipeline

`proven`'s FFI layer compiles Idris 2 to C via the RefC backend and wraps it in
a stable C ABI with Zig (`Idris2 --codegen refc → .c → zig build → libproven`).
There are **two** Zig build entry points:

| File | Builds | External deps | Used by |
|------|--------|---------------|---------|
| `ffi/zig/build.zig` | full Idris→RefC→Zig `libproven` | `idris2_zig_ffi` (path dep on `nextgen-languages/language-bridges`) + generated RefC C | local dev, full integration CI |
| `ffi/zig/build_standalone.zig` | `libproven_ffi.a` from `src/main.zig` only (pure-Zig C-ABI surface) | none | **CI** (`e2e.yml`, `zig-ffi.yml`) |

## Why E2E uses the standalone build (ADR-003)

`build.zig` declares its `idris2_zig_ffi` dependency as a Zig **`.path`**
dependency:

    .idris2_zig_ffi = .{ .path = "../../nextgen-languages/language-bridges/bridges/idris2" }

That points at a **private, cross-owner bridge repo**. `e2e.yml` stays on
`build_standalone.zig` so the broad E2E and benchmark jobs keep running without
private credentials. `build_standalone.zig` compiles `src/main.zig` alone — and
that file imports only `std`/`builtin` — so it needs neither the bridge nor the
generated RefC, giving CI a real (if narrower) Zig FFI build + unit-test signal.

`e2e.yml` and `zig-ffi.yml` therefore run, with Zig provisioned by
`mlugg/setup-zig@v1` at `0.15.2` (the `build.zig.zon` minimum):

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

## Full integration CI

`.github/workflows/ffi-full-integration.yml` restores the full pipeline:

1. Check out `proven`.
2. Check out `nextgen-languages/language-bridges` into
   `nextgen-languages/language-bridges`.
3. Install Idris2 0.8.0, Zig 0.15.2, and `libgmp`.
4. Run `scripts/build-refc.sh --verbose`.
5. Run `zig build` and `zig build test` with the generated RefC and Idris
   runtime/support paths.

The workflow runs the private full integration only when a repository secret
named `LANGUAGE_BRIDGES_TOKEN` is configured with read access to
`nextgen-languages/language-bridges`. If that secret is absent, the job emits a
notice and skips the private bridge steps instead of turning every unrelated PR
red. Pull requests from forks are skipped because that credential must not be
exposed to untrusted branches.

ClusterFuzzLite uses the standalone Zig FFI surface to build a dynamic
`libproven.so` fixture and fuzzes the Rust path traversal wrapper currently
backed by `src/main.zig`. Additional Rust fuzz targets should be enabled in
ClusterFuzzLite once their exported Zig symbols are available without the
private bridge.

If `language-bridges` is later converted to a real git submodule, keep the same
checkout location so `ffi/zig/build.zig.zon` continues to resolve the bridge via
`../../nextgen-languages/language-bridges/bridges/idris2`.
