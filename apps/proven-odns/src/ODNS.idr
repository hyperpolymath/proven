-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level ODNS module. Re-exports ODNS.Types and defines protocol constants.
module ODNS

import public ODNS.Types

%default total

-------------------------------------------------------------------------------
-- Protocol Constants (draft-pauly-dprive-oblivious-doh)
-------------------------------------------------------------------------------

||| Default ODNS port (runs over HTTPS).
public export
odnsPort : Nat
odnsPort = 443

||| Standard ODNS query path.
public export
odnsPath : String
odnsPath = "/dns-query"
