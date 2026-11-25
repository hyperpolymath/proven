-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module FalseGoal
import Proven.ECHIDNA.Kernel
%default total
forged : GoalHolds (Eq Zero (Succ Zero))
forged = NatEqual ValueZero (ValueSucc ValueZero)
