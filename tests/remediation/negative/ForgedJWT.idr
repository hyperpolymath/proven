-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module ForgedJWT
import Proven.SafeJWT
%default total
forged : createToken (minimalHeader HS256) emptyClaims (SecretKey []) = Ok "fake"
forged = Refl
