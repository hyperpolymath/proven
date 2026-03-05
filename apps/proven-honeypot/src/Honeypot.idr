-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Honeypot : Top-level module for the proven-honeypot server.
-- Re-exports Honeypot.Types and defines server constants.

module Honeypot

import public Honeypot.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Maximum concurrent connections the honeypot will accept.
public export
maxConnections : Nat
maxConnections = 1000

||| Log rotation threshold in bytes (10 MiB).
public export
logRotateSize : Nat
logRotateSize = 10485760
