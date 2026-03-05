-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for proven-telnet.
||| Re-exports Telnet.Types and provides protocol constants.
module Telnet

import public Telnet.Types

%default total

---------------------------------------------------------------------------
-- Protocol Constants (RFC 854)
---------------------------------------------------------------------------

||| Default Telnet port.
public export
telnetPort : Nat
telnetPort = 23

||| Maximum line length in bytes.
public export
maxLineLength : Nat
maxLineLength = 512
