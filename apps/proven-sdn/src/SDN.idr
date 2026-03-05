-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SDN : Top-level module for the proven-sdn OpenFlow-style SDN server.
-- Re-exports SDN.Types and defines server constants.

module SDN

import public SDN.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Standard OpenFlow controller port (IANA assigned).
public export
sdnPort : Nat
sdnPort = 6653
