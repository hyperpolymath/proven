-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for proven-ctlog.
||| Re-exports CTLog.Types and provides protocol constants.
module CTLog

import public CTLog.Types

%default total

---------------------------------------------------------------------------
-- Protocol Constants (RFC 6962)
---------------------------------------------------------------------------

||| Default CT log HTTPS port.
public export
ctlogPort : Nat
ctlogPort = 443

||| Maximum certificate chain length accepted.
public export
maxChainLength : Nat
maxChainLength = 10

||| Maximum merge delay in seconds (24 hours).
public export
maxMergeDelay : Nat
maxMergeDelay = 86400
