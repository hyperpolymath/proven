-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeGPU GPU programming surface.
|||
||| BUILD-BLOCKED: `Proven.SafeGPU` `import Data.Nat.Division` which
||| was retired from Idris2 0.8.0 stdlib. Pure-OWED until baseline
||| repaired (out of scope for #132 batch).
|||
||| OWED: GPUVendor enum self-equality, MemoryKind enum, validateGPU
||| range checks, kernel-launch boundary anchors.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeGPU.Proofs

%default total

public export
0 safeGPUProofsAwaitBaselineRepair : ()
