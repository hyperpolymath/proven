-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- PQC : Top-level module for the proven-pqc Post-Quantum Cryptography server.
-- Re-exports PQC.Types and defines server constants.

module PQC

import public PQC.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Default KEM algorithm (NIST Security Level 3).
public export
defaultKEM : String
defaultKEM = "ML-KEM-768"

||| Default signature algorithm (NIST Security Level 3).
public export
defaultSig : String
defaultSig = "ML-DSA-65"
