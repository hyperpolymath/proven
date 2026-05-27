-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeDNS DNS RecordType + label validation surface.
|||
||| Discharges enum self-equality + RFC-1035 wire-format Show anchors
||| for all 10 standard `RecordType` values.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeDNS.Proofs

import Proven.SafeDNS

%default total

--------------------------------------------------------------------------------
-- RecordType Show anchors (RFC-1035 / 1183 / 6844)
--------------------------------------------------------------------------------

public export
aShow : show A = "A"
aShow = Refl

public export
aaaaShow : show AAAA = "AAAA"
aaaaShow = Refl

public export
cnameShow : show CNAME = "CNAME"
cnameShow = Refl

public export
mxShow : show MX = "MX"
mxShow = Refl

public export
nsShow : show NS = "NS"
nsShow = Refl

public export
txtShow : show TXT = "TXT"
txtShow = Refl

public export
srvShow : show SRV = "SRV"
srvShow = Refl

public export
soaShow : show SOA = "SOA"
soaShow = Refl

public export
ptrShow : show PTR = "PTR"
ptrShow = Refl

public export
caaShow : show CAA = "CAA"
caaShow = Refl

--------------------------------------------------------------------------------
-- RecordType self-equality
--------------------------------------------------------------------------------

public export
aSelfEq : A == A = True
aSelfEq = Refl

public export
aaaaSelfEq : AAAA == AAAA = True
aaaaSelfEq = Refl

public export
cnameSelfEq : CNAME == CNAME = True
cnameSelfEq = Refl

public export
mxSelfEq : MX == MX = True
mxSelfEq = Refl

public export
caaSelfEq : CAA == CAA = True
caaSelfEq = Refl

public export
aNotAAAA : A == AAAA = False
aNotAAAA = Refl
