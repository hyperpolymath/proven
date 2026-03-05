-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-nts skeleton.
-- | Re-exports NTS.Types and defines protocol constants for
-- | RFC 8915 Network Time Security.

module NTS

import public NTS.Types

%default total

||| Default NTS-KE TCP port per RFC 8915 Section 4.
public export
ntskePort : Nat
ntskePort = 4460

||| Default number of cookies to request during NTS-KE.
public export
defaultCookieCount : Nat
defaultCookieCount = 8
