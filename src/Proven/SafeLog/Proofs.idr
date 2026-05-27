-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeLog log-level surface.
module Proven.SafeLog.Proofs

import Proven.SafeLog

%default total

public export
traceSelfEq : Trace == Trace = True
traceSelfEq = Refl

public export
debugSelfEq : Debug == Debug = True
debugSelfEq = Refl

public export
infoSelfEq : Info == Info = True
infoSelfEq = Refl

public export
warnSelfEq : Warn == Warn = True
warnSelfEq = Refl

public export
errorSelfEq : Proven.SafeLog.Error == Proven.SafeLog.Error = True
errorSelfEq = Refl

public export
fatalSelfEq : Fatal == Fatal = True
fatalSelfEq = Refl

public export
traceNotDebug : Trace == Debug = False
traceNotDebug = Refl
