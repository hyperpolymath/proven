-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-git Git protocol server.
||| Re-exports core types from Git.Types and defines server constants.
module Git

import public Git.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default listening port for the native Git protocol (git://).
public export
gitPort : Nat
gitPort = 9418

||| Default listening port for SSH transport.
public export
sshPort : Nat
sshPort = 22

||| Default listening port for smart HTTP(S) transport.
public export
httpPort : Nat
httpPort = 443

||| Maximum pack size accepted for a single push, in bytes (100 MiB).
public export
maxPackSize : Nat
maxPackSize = 104857600
