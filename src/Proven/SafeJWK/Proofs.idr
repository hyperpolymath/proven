-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeJWK JSON Web Key surface.
module Proven.SafeJWK.Proofs

import Proven.SafeJWK

%default total

-- KeyType
public export
rsaSelfEq : RSA == RSA = True
rsaSelfEq = Refl

public export
ecSelfEq : EC == EC = True
ecSelfEq = Refl

public export
okpSelfEq : OKP == OKP = True
okpSelfEq = Refl

public export
symmetricSelfEq : Symmetric == Symmetric = True
symmetricSelfEq = Refl

public export
rsaNotEC : RSA == EC = False
rsaNotEC = Refl

-- KeyStrength
public export
weakSelfEq : Weak == Weak = True
weakSelfEq = Refl

public export
acceptableSelfEq : Acceptable == Acceptable = True
acceptableSelfEq = Refl

public export
strongSelfEq : Strong == Strong = True
strongSelfEq = Refl

public export
veryStrongSelfEq : VeryStrong == VeryStrong = True
veryStrongSelfEq = Refl

public export
weakNotStrong : Weak == Strong = False
weakNotStrong = Refl

-- JWKAlgorithm representative self-equalities
public export
rs256SelfEq : RS256 == RS256 = True
rs256SelfEq = Refl

public export
ps256SelfEq : PS256 == PS256 = True
ps256SelfEq = Refl

public export
es256SelfEq : ES256 == ES256 = True
es256SelfEq = Refl

public export
eddsaSelfEq : EdDSA == EdDSA = True
eddsaSelfEq = Refl

public export
hs256SelfEq : HS256 == HS256 = True
hs256SelfEq = Refl
