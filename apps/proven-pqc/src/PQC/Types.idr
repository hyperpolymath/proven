-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- PQC.Types : Core types for the Post-Quantum Cryptography server.
-- Defines KEM algorithms (FIPS 203), signature algorithms (FIPS 204/205),
-- hybrid operation modes, and cryptographic operations.

module PQC.Types

%default total

---------------------------------------------------------------------------
-- KEMAlgorithm : NIST FIPS 203 ML-KEM parameter sets.
---------------------------------------------------------------------------

||| Module-Lattice Key Encapsulation Mechanism parameter sets (FIPS 203).
public export
data KEMAlgorithm : Type where
  ML_KEM_512  : KEMAlgorithm
  ML_KEM_768  : KEMAlgorithm
  ML_KEM_1024 : KEMAlgorithm

export
Show KEMAlgorithm where
  show ML_KEM_512  = "ML-KEM-512"
  show ML_KEM_768  = "ML-KEM-768"
  show ML_KEM_1024 = "ML-KEM-1024"

---------------------------------------------------------------------------
-- SignatureAlgorithm : NIST FIPS 204/205 signature schemes.
---------------------------------------------------------------------------

||| Post-quantum digital signature algorithms (FIPS 204 ML-DSA, FIPS 205 SLH-DSA).
public export
data SignatureAlgorithm : Type where
  ML_DSA_44   : SignatureAlgorithm
  ML_DSA_65   : SignatureAlgorithm
  ML_DSA_87   : SignatureAlgorithm
  SLH_DSA_128f : SignatureAlgorithm
  SLH_DSA_128s : SignatureAlgorithm
  SLH_DSA_192f : SignatureAlgorithm
  SLH_DSA_256f : SignatureAlgorithm

export
Show SignatureAlgorithm where
  show ML_DSA_44    = "ML-DSA-44"
  show ML_DSA_65    = "ML-DSA-65"
  show ML_DSA_87    = "ML-DSA-87"
  show SLH_DSA_128f = "SLH-DSA-128f"
  show SLH_DSA_128s = "SLH-DSA-128s"
  show SLH_DSA_192f = "SLH-DSA-192f"
  show SLH_DSA_256f = "SLH-DSA-256f"

---------------------------------------------------------------------------
-- HybridMode : Classical/PQC hybrid operation modes.
---------------------------------------------------------------------------

||| Whether to use classical, post-quantum, or hybrid cryptography.
public export
data HybridMode : Type where
  ClassicalOnly : HybridMode
  PQCOnly       : HybridMode
  Hybrid        : HybridMode

export
Show HybridMode where
  show ClassicalOnly = "ClassicalOnly"
  show PQCOnly       = "PQCOnly"
  show Hybrid        = "Hybrid"

---------------------------------------------------------------------------
-- Operation : Cryptographic operations the PQC server can perform.
---------------------------------------------------------------------------

||| Operations supported by the post-quantum cryptography server.
public export
data Operation : Type where
  KeyGen       : Operation
  Encapsulate  : Operation
  Decapsulate  : Operation
  Sign         : Operation
  Verify       : Operation

export
Show Operation where
  show KeyGen      = "KeyGen"
  show Encapsulate = "Encapsulate"
  show Decapsulate = "Decapsulate"
  show Sign        = "Sign"
  show Verify      = "Verify"
