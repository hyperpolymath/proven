-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-hardened: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Hardened.Types.
--
-- Usage:
--   idris2 --build proven-hardened.ipkg
--   ./build/exec/proven-hardened

module Main

import Hardened

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-hardened v0.1.0 -- hardened application server"
  putStrLn ""
  putStrLn $ "Audit buffer size:       " ++ show auditBufferSize ++ " events"
  putStrLn $ "Max concurrent requests: " ++ show maxConcurrentRequests

  putStrLn "\n--- HardeningLevel ---"
  putStrLn $ "  " ++ show Minimal
  putStrLn $ "  " ++ show Standard
  putStrLn $ "  " ++ show High
  putStrLn $ "  " ++ show Maximum

  putStrLn "\n--- SecurityControl ---"
  putStrLn $ "  " ++ show ASLR
  putStrLn $ "  " ++ show DEP
  putStrLn $ "  " ++ show StackCanary
  putStrLn $ "  " ++ show CFI
  putStrLn $ "  " ++ show Sandboxing
  putStrLn $ "  " ++ show SecureBoot
  putStrLn $ "  " ++ show AuditLog

  putStrLn "\n--- ComplianceStandard ---"
  putStrLn $ "  " ++ show CIS
  putStrLn $ "  " ++ show STIG
  putStrLn $ "  " ++ show NIST80053
  putStrLn $ "  " ++ show PCI_DSS
  putStrLn $ "  " ++ show FIPS140

  putStrLn "\n--- AuditEvent ---"
  putStrLn $ "  " ++ show ProcessStart
  putStrLn $ "  " ++ show FileAccess
  putStrLn $ "  " ++ show NetworkConn
  putStrLn $ "  " ++ show PrivilegeEscalation
  putStrLn $ "  " ++ show ConfigChange
  putStrLn $ "  " ++ show AuthAttempt

  putStrLn "\n--- HealthStatus ---"
  putStrLn $ "  " ++ show Healthy
  putStrLn $ "  " ++ show Degraded
  putStrLn $ "  " ++ show Compromised
  putStrLn $ "  " ++ show Unresponsive
