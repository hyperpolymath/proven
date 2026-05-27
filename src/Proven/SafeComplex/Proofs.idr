-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeComplex complex-number arithmetic.
|||
||| Discharges structural invariants of `Complex a` smart constructors
||| and ComplexError surface.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeComplex.Proofs

import Proven.SafeComplex

%default total

--------------------------------------------------------------------------------
-- `MkComplex` record projections
--------------------------------------------------------------------------------

public export
mkComplexReal : (r, i : a) -> (MkComplex r i).real = r
mkComplexReal r i = Refl

public export
mkComplexImag : (r, i : a) -> (MkComplex r i).imag = i
mkComplexImag r i = Refl

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

public export
complexExpansion : (r, i : a) -> complex r i = MkComplex r i
complexExpansion r i = Refl

public export
fromRealRealEq : Num a => (r : a) -> (fromReal r).real = r
fromRealRealEq r = Refl

public export
fromImagImagEq : Num a => (im : a) -> (fromImag im).imag = im
fromImagImagEq im = Refl

--------------------------------------------------------------------------------
-- ComplexError Show anchors
--------------------------------------------------------------------------------

public export
divisionByZeroShow : show DivisionByZero = "Division by zero in complex arithmetic"
divisionByZeroShow = Refl

public export
overflowShow : show Overflow = "Complex number overflow"
overflowShow = Refl

--------------------------------------------------------------------------------
-- Type alias
--------------------------------------------------------------------------------

||| `C` is just `Complex`.
public export
cIsComplex : C = Complex
cIsComplex = Refl
