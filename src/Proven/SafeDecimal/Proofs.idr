-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeDecimal fixed-precision decimal arithmetic.
|||
||| Discharges `MkDecimal` projections + DecimalError Show anchors.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeDecimal.Proofs

import Proven.SafeDecimal

%default total

public export
mkDecimalMantissa : (m : Integer) -> (s : Nat) -> (MkDecimal m s).mantissa = m
mkDecimalMantissa m s = Refl

public export
mkDecimalScale : (m : Integer) -> (s : Nat) -> (MkDecimal m s).scale = s
mkDecimalScale m s = Refl

public export
divisionByZeroShow : show DivisionByZero = "Decimal division by zero"
divisionByZeroShow = Refl

public export
overflowShow : show Overflow = "Decimal overflow"
overflowShow = Refl

public export
precisionLossShow : show PrecisionLoss = "Decimal precision loss"
precisionLossShow = Refl
