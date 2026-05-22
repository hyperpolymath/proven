-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeDSP digital-signal-processing surface.
|||
||| BUILD-BLOCKED: `Proven.SafeDSP` `import Data.Nat.Division` which
||| was retired from Idris2 0.8.0 stdlib. The parent module cannot
||| be imported here so no theorem about its surface can be
||| machine-checked until the import is repaired (out of scope —
||| #132 batch forbids modifying source `.idr` files).
|||
||| The file exists so the residual proof debt is discoverable
||| rather than silent. OWED items, once the baseline is repaired:
|||
|||   * `WindowFunction` enum self-equality (5 constructors).
|||   * `FilterType` enum self-equality (5 constructors).
|||   * Show wire-format anchors for both enums.
|||   * `SampleConfig` / `FilterSpec` record-projection anchors.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeDSP.Proofs

%default total

||| Sentinel for the upstream `Data.Nat.Division` baseline-rot blocker.
public export
0 safeDSPProofsAwaitBaselineRepair : ()
