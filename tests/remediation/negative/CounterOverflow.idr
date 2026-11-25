-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module CounterOverflow
import Proven.SafeCrypto.Random
%default total
forged : counterNonce (MkByteVec [0,0,0,0,0,0,0,0]) 4294967296 =
         Right (MkByteVec [0,0,0,0,0,0,0,0,0,0,0,0])
forged = Refl
