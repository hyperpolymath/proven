-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-sandbox: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Sandbox.Types.
--
-- Usage:
--   idris2 --build proven-sandbox.ipkg
--   ./build/exec/proven-sandbox

module Main

import Sandbox

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-sandbox v0.1.0 -- sandbox execution server"
  putStrLn ""
  putStrLn $ "Default timeout:  " ++ show defaultTimeout ++ " seconds"
  putStrLn $ "Max memory:       " ++ show maxMemoryMB ++ " MB"
  putStrLn $ "Max CPU seconds:  " ++ show maxCPUSeconds

  putStrLn "\n--- ExecutionPolicy ---"
  putStrLn $ "  " ++ show Unrestricted
  putStrLn $ "  " ++ show ReadOnly
  putStrLn $ "  " ++ show NetworkDenied
  putStrLn $ "  " ++ show Isolated
  putStrLn $ "  " ++ show Ephemeral

  putStrLn "\n--- ResourceLimit ---"
  putStrLn $ "  " ++ show CPUTime
  putStrLn $ "  " ++ show Memory
  putStrLn $ "  " ++ show DiskIO
  putStrLn $ "  " ++ show NetworkIO
  putStrLn $ "  " ++ show FileDescriptors
  putStrLn $ "  " ++ show Processes

  putStrLn "\n--- SandboxState ---"
  putStrLn $ "  " ++ show Creating
  putStrLn $ "  " ++ show Ready
  putStrLn $ "  " ++ show Running
  putStrLn $ "  " ++ show Suspended
  putStrLn $ "  " ++ show Terminated
  putStrLn $ "  " ++ show Destroyed

  putStrLn "\n--- ExitReason ---"
  putStrLn $ "  " ++ show Normal
  putStrLn $ "  " ++ show Timeout
  putStrLn $ "  " ++ show MemoryExceeded
  putStrLn $ "  " ++ show PolicyViolation
  putStrLn $ "  " ++ show Killed
  putStrLn $ "  " ++ show Error

  putStrLn "\n--- SyscallPolicy ---"
  putStrLn $ "  " ++ show Allow
  putStrLn $ "  " ++ show Deny
  putStrLn $ "  " ++ show Log
  putStrLn $ "  " ++ show Trap
