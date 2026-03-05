-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for proven-opcua.
||| Re-exports OPCUA.Types and provides protocol constants.
module OPCUA

import public OPCUA.Types

%default total

---------------------------------------------------------------------------
-- Protocol Constants (OPC UA)
---------------------------------------------------------------------------

||| Default OPC UA binary protocol port.
public export
opcuaPort : Nat
opcuaPort = 4840

||| Default OPC UA TLS port.
public export
opcuaTLSPort : Nat
opcuaTLSPort = 4843

||| Maximum nodes per Read request.
public export
maxNodesPerRead : Nat
maxNodesPerRead = 1000
