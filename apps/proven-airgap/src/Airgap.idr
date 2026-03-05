-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Airgap : Top-level module for the proven-airgap server.
-- Re-exports Airgap.Types and defines server constants.

module Airgap

import public Airgap.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Maximum transfer size in bytes (4 GiB).
public export
maxTransferSize : Nat
maxTransferSize = 4294967296

||| Content scan timeout in seconds.
public export
scanTimeout : Nat
scanTimeout = 300
