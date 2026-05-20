-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeNPU neural-processing-unit surface.
|||
||| BUILD-BLOCKED: imports Data.Nat.Division (retired Idris2 0.8.0).
||| Pure-OWED sentinel.
module Proven.SafeNPU.Proofs

%default total

public export
0 safeNPUProofsAwaitBaselineRepair : ()
