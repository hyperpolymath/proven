-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-apiserver: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Apiserver.Types.
--
-- Usage:
--   idris2 --build proven-apiserver.ipkg
--   ./build/exec/proven-apiserver

module Main

import Apiserver

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-apiserver v0.1.0 -- API gateway server"
  putStrLn ""
  putStrLn $ "API port:           " ++ show apiPort
  putStrLn $ "Default rate limit: " ++ show defaultRateLimit ++ " req/min"
  putStrLn $ "Max payload size:   " ++ show maxPayloadSize ++ " bytes"

  putStrLn "\n--- AuthScheme ---"
  putStrLn $ "  " ++ show APIKey
  putStrLn $ "  " ++ show Bearer
  putStrLn $ "  " ++ show Basic
  putStrLn $ "  " ++ show OAuth2
  putStrLn $ "  " ++ show HMAC
  putStrLn $ "  " ++ show MTLS

  putStrLn "\n--- RateLimitStrategy ---"
  putStrLn $ "  " ++ show FixedWindow
  putStrLn $ "  " ++ show SlidingWindow
  putStrLn $ "  " ++ show TokenBucket
  putStrLn $ "  " ++ show LeakyBucket

  putStrLn "\n--- APIVersion ---"
  putStrLn $ "  " ++ show V1
  putStrLn $ "  " ++ show V2
  putStrLn $ "  " ++ show V3
  putStrLn $ "  " ++ show Latest
  putStrLn $ "  " ++ show Deprecated

  putStrLn "\n--- ResponseFormat ---"
  putStrLn $ "  " ++ show JSON
  putStrLn $ "  " ++ show XML
  putStrLn $ "  " ++ show Protobuf
  putStrLn $ "  " ++ show MessagePack

  putStrLn "\n--- GatewayError ---"
  putStrLn $ "  " ++ show Unauthorized
  putStrLn $ "  " ++ show RateLimited
  putStrLn $ "  " ++ show NotFound
  putStrLn $ "  " ++ show BadRequest
  putStrLn $ "  " ++ show ServiceUnavailable
  putStrLn $ "  " ++ show CircuitOpen
