-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module MalformedLifecycle
import Proven.ABI.Lifecycle
%default total
forged : lifecycleCommand 4 = 1
forged = Refl
