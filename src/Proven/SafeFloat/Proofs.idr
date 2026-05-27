-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeFloat IEEE-754 classification surface.
|||
||| Discharges enum self-equality for FloatClass (5 constructors).
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeFloat.Proofs

import Proven.SafeFloat

%default total

public export
normalSelfEq : Normal == Normal = True
normalSelfEq = Refl

public export
subnormalSelfEq : Subnormal == Subnormal = True
subnormalSelfEq = Refl

public export
zeroSelfEq : Proven.SafeFloat.Zero == Proven.SafeFloat.Zero = True
zeroSelfEq = Refl

public export
infiniteSelfEq : Infinite == Infinite = True
infiniteSelfEq = Refl

public export
nanSelfEq : NaN == NaN = True
nanSelfEq = Refl

public export
normalNotZero : Normal == Proven.SafeFloat.Zero = False
normalNotZero = Refl

public export
infiniteNotNaN : Infinite == NaN = False
infiniteNotNaN = Refl
