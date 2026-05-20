-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeMCP Model-Context-Protocol surface.
module Proven.SafeMCP.Proofs

import Proven.SafeMCP

%default total

-- MCPRole
public export
userSelfEq : Proven.SafeMCP.User == Proven.SafeMCP.User = True
userSelfEq = Refl

public export
assistantSelfEq : Assistant == Assistant = True
assistantSelfEq = Refl

public export
systemSelfEq : Proven.SafeMCP.System == Proven.SafeMCP.System = True
systemSelfEq = Refl

public export
toolSelfEq : Tool == Tool = True
toolSelfEq = Refl

-- InjectionConfidence
public export
noneSelfEq : None == None = True
noneSelfEq = Refl

public export
lowSelfEq : Low == Low = True
lowSelfEq = Refl

public export
mediumSelfEq : Medium == Medium = True
mediumSelfEq = Refl

public export
highSelfEq : High == High = True
highSelfEq = Refl

public export
criticalSelfEq : Critical == Critical = True
criticalSelfEq = Refl

public export
noneNotCritical : None == Critical = False
noneNotCritical = Refl
