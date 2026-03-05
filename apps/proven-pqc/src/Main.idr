-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-pqc. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import PQC

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allKEMAlgorithms : List KEMAlgorithm
allKEMAlgorithms = [ML_KEM_512, ML_KEM_768, ML_KEM_1024]

allSignatureAlgorithms : List SignatureAlgorithm
allSignatureAlgorithms =
  [ML_DSA_44, ML_DSA_65, ML_DSA_87, SLH_DSA_128f, SLH_DSA_128s, SLH_DSA_192f, SLH_DSA_256f]

allHybridModes : List HybridMode
allHybridModes = [ClassicalOnly, PQCOnly, Hybrid]

allOperations : List Operation
allOperations = [KeyGen, Encapsulate, Decapsulate, Sign, Verify]

main : IO ()
main = do
  putStrLn "proven-pqc : Post-Quantum Cryptography server"
  putStrLn $ "  Default KEM: " ++ defaultKEM
  putStrLn $ "  Default Sig: " ++ defaultSig
  putStrLn $ "  KEMAlgorithms:       " ++ show allKEMAlgorithms
  putStrLn $ "  SignatureAlgorithms:  " ++ show allSignatureAlgorithms
  putStrLn $ "  HybridModes:         " ++ show allHybridModes
  putStrLn $ "  Operations:          " ++ show allOperations
