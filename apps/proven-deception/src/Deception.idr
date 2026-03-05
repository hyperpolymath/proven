-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-deception: Deception/decoy server.
--
-- Architecture:
--   - Types: DecoyType, TriggerEvent, AlertPriority, DecoyState, ResponseAction
--
-- This module defines core deception constants and re-exports Deception.Types.

module Deception

import public Deception.Types

%default total

||| Maximum number of decoys that can be deployed simultaneously.
public export
maxDecoys : Nat
maxDecoys = 500

||| Minimum interval (in seconds) between alerts for the same decoy.
||| Prevents alert fatigue during sustained attacker interaction.
public export
alertThrottle : Nat
alertThrottle = 60
