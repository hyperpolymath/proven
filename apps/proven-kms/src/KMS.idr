-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- KMS : Top-level module for the proven-kms Key Management Server.
-- Re-exports KMS.Types and defines server constants.

module KMS

import public KMS.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Default KMIP server port (IANA assigned for KMIP over TLS).
public export
kmsPort : Nat
kmsPort = 5696

||| Maximum key material size in bytes (256 KiB).
public export
maxKeySize : Nat
maxKeySize = 32768
