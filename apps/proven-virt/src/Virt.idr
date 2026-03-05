-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-virt virtualization server.
||| Re-exports core types and provides server constants.
module Virt

import public Virt.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default libvirt plaintext port.
public export
virtPort : Nat
virtPort = 16509

||| Default libvirt TLS port.
public export
virtTLSPort : Nat
virtTLSPort = 16514

||| Maximum number of virtual machines.
public export
maxVMs : Nat
maxVMs = 256

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-virt"
