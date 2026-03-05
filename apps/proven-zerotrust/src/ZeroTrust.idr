-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- ZeroTrust : Top-level module for the proven-zerotrust server.
-- Re-exports ZeroTrust.Types and defines server constants.

module ZeroTrust

import public ZeroTrust.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Maximum session duration in seconds (1 hour).
public export
maxSessionDuration : Nat
maxSessionDuration = 3600

||| Re-authentication interval in seconds (15 minutes).
public export
reauthInterval : Nat
reauthInterval = 900
