-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-ptp skeleton.
-- | Re-exports PTP.Types and defines protocol constants for
-- | IEEE 1588 Precision Time Protocol.

module PTP

import public PTP.Types

%default total

||| UDP port for PTP event messages (Sync, Delay_Req, Pdelay_Req, Pdelay_Resp).
public export
ptpEventPort : Nat
ptpEventPort = 319

||| UDP port for PTP general messages (Announce, Follow_Up, Delay_Resp, etc.).
public export
ptpGeneralPort : Nat
ptpGeneralPort = 320

||| Default PTP domain number.
public export
ptpDomain : Nat
ptpDomain = 0
