-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for proven-tacacs.
||| Re-exports TACACS.Types and provides protocol constants.
module TACACS

import public TACACS.Types

%default total

---------------------------------------------------------------------------
-- Protocol Constants (RFC 8907)
---------------------------------------------------------------------------

||| Default TACACS+ port.
public export
tacacsPort : Nat
tacacsPort = 49
