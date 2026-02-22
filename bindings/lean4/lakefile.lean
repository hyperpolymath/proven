/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Lake build configuration for Proven Lean 4 bindings

Links against `libproven` (Idris 2 + Zig FFI bridge) and compiles the
C shim (`ffi_shim.c`) that bridges Lean 4 managed types to the libproven
C ABI.

## Prerequisites

- `libproven.so` (or `.a` / `.dylib`) must be in the library search path
- Set `LIBRARY_PATH` and/or `LD_LIBRARY_PATH` if libproven is not in a
  standard location
-/

import Lake
open Lake DSL

package «proven» where
  version := v!"0.9.0"
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

/-- C shim that bridges Lean 4 ByteArray/String to libproven raw pointers. -/
target ffi_shim pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi_shim.o"
  let srcFile := pkg.dir / "ffi_shim.c"
  let leancDir := (← getLeanIncludeDir)
  buildO oFile srcFile
    #["-I", leancDir.toString, "-fPIC", "-O2"]
    "cc"
  return oFile

@[default_target]
lean_lib «Proven» where
  srcDir := "."
  roots := #[`Proven]
  moreLinkArgs := #["-lproven"]
  moreLeankArgs := #["-lproven"]
