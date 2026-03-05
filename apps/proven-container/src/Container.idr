-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-container management server.
||| Re-exports core types and provides server constants.
module Container

import public Container.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default plaintext API port.
public export
containerPort : Nat
containerPort = 2375

||| Default TLS API port.
public export
containerTLSPort : Nat
containerTLSPort = 2376

||| Maximum number of managed containers.
public export
maxContainers : Nat
maxContainers = 1000

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-container"
