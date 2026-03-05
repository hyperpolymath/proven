-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for proven-netconf.
||| Re-exports NETCONF.Types and provides protocol constants.
module NETCONF

import public NETCONF.Types

%default total

---------------------------------------------------------------------------
-- Protocol Constants (RFC 6241)
---------------------------------------------------------------------------

||| Default NETCONF SSH port.
public export
netconfPort : Nat
netconfPort = 830

||| Maximum message size in bytes (10 MiB).
public export
maxMessageSize : Nat
maxMessageSize = 10485760
