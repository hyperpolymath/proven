-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeCycleDetect three-colour DFS cycle detection.
|||
||| Discharges enum self-equality + Show wire-format anchors for Colour
||| (White/Grey/Black) and CycleResult (Acyclic/Cyclic).
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeCycleDetect.Proofs

import Proven.SafeCycleDetect

%default total

--------------------------------------------------------------------------------
-- Colour enum self-equality
--------------------------------------------------------------------------------

public export
whiteSelfEq : White == White = True
whiteSelfEq = Refl

public export
greySelfEq : Grey == Grey = True
greySelfEq = Refl

public export
blackSelfEq : Black == Black = True
blackSelfEq = Refl

public export
whiteNotGrey : White == Grey = False
whiteNotGrey = Refl

public export
greyNotBlack : Grey == Black = False
greyNotBlack = Refl

--------------------------------------------------------------------------------
-- CycleResult enum self-equality
--------------------------------------------------------------------------------

public export
acyclicSelfEq : Acyclic == Acyclic = True
acyclicSelfEq = Refl
