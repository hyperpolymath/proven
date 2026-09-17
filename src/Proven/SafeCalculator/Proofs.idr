-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeCalculator expression evaluation.
|||
||| BUILD-BLOCKED: `Proven.SafeCalculator` transitively depends on
||| `Proven.SafeMath` which `import public`s the locally-broken
||| `Proven.SafeMath.Proofs` (the latter `import Data.Nat.Division`,
||| a module retired from Idris2 0.8.0 stdlib). Because the parent
||| module cannot be imported here, no theorem about its surface can
||| be machine-checked from this file under Idris2 0.8.0 without
||| repairing `Proven.SafeMath.Proofs` first (out of scope for the
||| standards#132 batch, which is forbidden from modifying source
||| `.idr` files).
|||
||| The file exists so the residual proof debt is *discoverable*
||| rather than silent (per the standards#132 closure framing — a
||| missing Proofs.idr is silent debt; a pure-OWED file with a named
||| blocker is discoverable debt and is the "I see you" half of the
||| #132 contract).
|||
||| OWED items (named, erased, no body), once the SafeMath baseline
||| is repaired and `Proven.SafeCalculator` becomes importable:
|||
|||   * `Op` Show wire-format anchors (`Add` -> "+", etc.).
|||   * Smart-constructor structural anchors (`add = Binary Add`).
|||   * `eval env (Lit x) = Right x` for any env (literal pass-through).
|||   * `getVar _ [] = Nothing` (empty env has no variables).
|||   * `evalUnary Sqrt x` rejects negative x with `NegativeSqrt`.
|||
||| Zero `believe_me`, zero `idris_crash`.
module Proven.SafeCalculator.Proofs

%default total

||| Sentinel for the upstream baseline-rot blocker. When the
||| `Proven.SafeMath.Proofs` baseline is repaired this file should be
||| rewritten with concrete proofs of the items above.
public export
0 safeCalculatorProofsAwaitBaselineRepair : ()
