-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-doh. Prints version info and demonstrates types.
module Main

import DoH

%default total

covering
main : IO ()
main = do
  putStrLn "proven-doh v0.1.0 -- Formally verified DNS over HTTPS types (RFC 8484)"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "Content Types:"
  putStrLn $ "  " ++ show DNSMessage ++ ", " ++ show DNSJson
  putStrLn "Request Methods:"
  putStrLn $ "  " ++ show Get ++ ", " ++ show Post
  putStrLn "Wire Formats:"
  putStrLn $ "  " ++ show Binary ++ ", " ++ show Json
  putStrLn "Error Reasons:"
  putStrLn $ "  " ++ show BadContentType ++ ", " ++ show BadMethod
            ++ ", " ++ show PayloadTooLarge
  putStrLn ""
  putStrLn $ "DoH port: " ++ show dohPort
  putStrLn $ "Max payload: " ++ show maxPayload
  putStrLn $ "DoH path: " ++ dohPath
