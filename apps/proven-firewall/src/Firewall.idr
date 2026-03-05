-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level Firewall module. Re-exports Firewall.Types and defines constants.
module Firewall

import public Firewall.Types

%default total

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

||| Maximum number of firewall rules supported.
public export
maxRules : Nat
maxRules = 10000
