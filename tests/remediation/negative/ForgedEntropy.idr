-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module ForgedEntropy
import Proven.SafeCrypto.Random
%default total
forged : randomBytes 1 = Right (MkByteVec [0])
forged = Refl
