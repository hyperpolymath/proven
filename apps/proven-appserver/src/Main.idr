-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-appserver: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Appserver.Types.
--
-- Usage:
--   idris2 --build proven-appserver.ipkg
--   ./build/exec/proven-appserver

module Main

import Appserver

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-appserver v0.1.0 -- application server"
  putStrLn ""
  putStrLn $ "App port:          " ++ show appPort
  putStrLn $ "Shutdown grace:    " ++ show shutdownGrace ++ " seconds"
  putStrLn $ "Max request size:  " ++ show maxRequestSize ++ " bytes"

  putStrLn "\n--- RequestType ---"
  putStrLn $ "  " ++ show HTTP
  putStrLn $ "  " ++ show WebSocket
  putStrLn $ "  " ++ show GRPC
  putStrLn $ "  " ++ show GraphQL

  putStrLn "\n--- LifecycleState ---"
  putStrLn $ "  " ++ show Initializing
  putStrLn $ "  " ++ show Starting
  putStrLn $ "  " ++ show Running
  putStrLn $ "  " ++ show Draining
  putStrLn $ "  " ++ show Stopping
  putStrLn $ "  " ++ show Stopped

  putStrLn "\n--- HealthCheck ---"
  putStrLn $ "  " ++ show Liveness
  putStrLn $ "  " ++ show Readiness
  putStrLn $ "  " ++ show Startup

  putStrLn "\n--- DeployStrategy ---"
  putStrLn $ "  " ++ show RollingUpdate
  putStrLn $ "  " ++ show BlueGreen
  putStrLn $ "  " ++ show Canary
  putStrLn $ "  " ++ show Recreate

  putStrLn "\n--- ErrorCategory ---"
  putStrLn $ "  " ++ show ClientError
  putStrLn $ "  " ++ show ServerError
  putStrLn $ "  " ++ show Timeout
  putStrLn $ "  " ++ show CircuitOpen
  putStrLn $ "  " ++ show RateLimited
