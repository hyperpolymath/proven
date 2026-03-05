-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-odns. Prints version info and demonstrates types.
module Main

import ODNS

%default total

covering
main : IO ()
main = do
  putStrLn "proven-odns v0.1.0 -- Formally verified Oblivious DNS types (draft-pauly-dprive-oblivious-doh)"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "Roles:"
  putStrLn $ "  " ++ show Client ++ ", " ++ show Proxy ++ ", " ++ show Target
  putStrLn "Message Types:"
  putStrLn $ "  " ++ show Query ++ ", " ++ show Response
  putStrLn "Error Reasons:"
  putStrLn $ "  " ++ show ProxyError ++ ", " ++ show TargetError
            ++ ", " ++ show DecryptionFailed ++ ", " ++ show InvalidConfig
  putStrLn "Encapsulation:"
  putStrLn $ "  " ++ show HPKE
  putStrLn ""
  putStrLn $ "ODNS port: " ++ show odnsPort
  putStrLn $ "ODNS path: " ++ odnsPath
