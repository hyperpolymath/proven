-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- IDS : Top-level module for the proven-ids IDS/IPS server.
-- Re-exports IDS.Types and defines server constants.

module IDS

import public IDS.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Maximum number of detection rules the engine can load.
public export
maxRules : Nat
maxRules = 50000

||| Maximum IP packet size in bytes.
public export
maxPacketSize : Nat
maxPacketSize = 65535
