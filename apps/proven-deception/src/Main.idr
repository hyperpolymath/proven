-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-deception: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Deception.Types.
--
-- Usage:
--   idris2 --build proven-deception.ipkg
--   ./build/exec/proven-deception

module Main

import Deception

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-deception v0.1.0 -- deception/decoy server"
  putStrLn ""
  putStrLn $ "Max decoys:       " ++ show maxDecoys
  putStrLn $ "Alert throttle:   " ++ show alertThrottle ++ " seconds"

  putStrLn "\n--- DecoyType ---"
  putStrLn $ "  " ++ show Service
  putStrLn $ "  " ++ show Credential
  putStrLn $ "  " ++ show File
  putStrLn $ "  " ++ show Network
  putStrLn $ "  " ++ show Token
  putStrLn $ "  " ++ show Breadcrumb

  putStrLn "\n--- TriggerEvent ---"
  putStrLn $ "  " ++ show Access
  putStrLn $ "  " ++ show Login
  putStrLn $ "  " ++ show Read
  putStrLn $ "  " ++ show Write
  putStrLn $ "  " ++ show Execute
  putStrLn $ "  " ++ show Scan

  putStrLn "\n--- AlertPriority ---"
  putStrLn $ "  " ++ show Low
  putStrLn $ "  " ++ show Medium
  putStrLn $ "  " ++ show High
  putStrLn $ "  " ++ show Critical

  putStrLn "\n--- DecoyState ---"
  putStrLn $ "  " ++ show Active
  putStrLn $ "  " ++ show Triggered
  putStrLn $ "  " ++ show Disabled
  putStrLn $ "  " ++ show Expired

  putStrLn "\n--- ResponseAction ---"
  putStrLn $ "  " ++ show Alert
  putStrLn $ "  " ++ show Redirect
  putStrLn $ "  " ++ show Delay
  putStrLn $ "  " ++ show Fingerprint
  putStrLn $ "  " ++ show Isolate
