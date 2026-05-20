-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeML probability / ML primitives surface.
|||
||| OWED: surface depends on Double-comparison validation and proof
||| objects (SumToOne, UnitVector) which require evidence-style proofs.
module Proven.SafeML.Proofs

import Proven.SafeML

%default total

||| Sentinel — surface depends on Double comparisons + So-proofs.
public export
0 safeMLProofsAwaitDoubleEqDecide : ()
