-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for proven-ctlog skeleton.
module Main

import CTLog

%default total

||| Print server identification and type constructors.
covering
main : IO ()
main = do
  putStrLn "proven-ctlog — Certificate Transparency Log (RFC 6962) skeleton"
  putStrLn $ "  Port: " ++ show ctlogPort
  putStrLn $ "  Max Chain Length: " ++ show maxChainLength
  putStrLn $ "  Max Merge Delay: " ++ show maxMergeDelay ++ "s"
  putStrLn "Log Entry Types:"
  putStrLn $ "  " ++ show X509Entry
  putStrLn $ "  " ++ show PrecertEntry
  putStrLn "Signature Types:"
  putStrLn $ "  " ++ show CertificateTimestamp
  putStrLn $ "  " ++ show TreeHash
  putStrLn "Merkle Leaf Types:"
  putStrLn $ "  " ++ show TimestampedEntry
  putStrLn "Submission Status:"
  putStrLn $ "  " ++ show Accepted
  putStrLn $ "  " ++ show Duplicate
  putStrLn $ "  " ++ show RateLimited
  putStrLn $ "  " ++ show Rejected
  putStrLn $ "  " ++ show InvalidChain
  putStrLn $ "  " ++ show UnknownAnchor
  putStrLn "Verification Results:"
  putStrLn $ "  " ++ show ValidProof
  putStrLn $ "  " ++ show InvalidProof
  putStrLn $ "  " ++ show InconsistentTree
  putStrLn $ "  " ++ show StaleSTH
