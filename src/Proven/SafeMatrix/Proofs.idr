-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeMatrix linear-algebra surface.
module Proven.SafeMatrix.Proofs

import Proven.SafeMatrix
import Data.Vect

%default total

public export
dimensionMismatchShow : show DimensionMismatch = "Matrix dimension mismatch"
dimensionMismatchShow = Refl

public export
singularMatrixShow :
  show SingularMatrix = "Matrix is singular (not invertible)"
singularMatrixShow = Refl

public export
emptyMatrixShow : show EmptyMatrix = "Empty matrix"
emptyMatrixShow = Refl

public export
mkLULower :
  (l, u : Matrix n n a) -> (MkLU l u).lower = l
mkLULower l u = Refl

public export
mkLUUpper :
  (l, u : Matrix n n a) -> (MkLU l u).upper = u
mkLUUpper l u = Refl
