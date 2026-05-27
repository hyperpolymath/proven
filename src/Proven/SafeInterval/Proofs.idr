-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeInterval interval-arithmetic surface.
module Proven.SafeInterval.Proofs

import Proven.SafeInterval

%default total

public export
mkIntervalLo : (l, h : a) -> (MkInterval l h).lo = l
mkIntervalLo l h = Refl

public export
mkIntervalHi : (l, h : a) -> (MkInterval l h).hi = h
mkIntervalHi l h = Refl
