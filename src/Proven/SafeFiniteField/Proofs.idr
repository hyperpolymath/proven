-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeFiniteField prime / binary field elements.
|||
||| Discharges record-projection theorems for the three field-element
||| types. `inRange` is erased and not projection-tractable.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeFiniteField.Proofs

import Proven.SafeFiniteField
import Data.So

%default total

public export
mkPolynomialCoefficients :
  (cs : List (PrimeFieldElement p))
  -> (MkPolynomial cs).coefficients = cs
mkPolynomialCoefficients cs = Refl
