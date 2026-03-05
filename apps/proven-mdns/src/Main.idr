-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-mdns. Prints version info and demonstrates types.
module Main

import MDNS

%default total

covering
main : IO ()
main = do
  putStrLn "proven-mdns v0.1.0 -- Formally verified mDNS/DNS-SD protocol types (RFC 6762)"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "Record Types:"
  putStrLn $ "  " ++ show A ++ ", " ++ show AAAA ++ ", " ++ show PTR
            ++ ", " ++ show SRV ++ ", " ++ show TXT
  putStrLn "Query Types:"
  putStrLn $ "  " ++ show Standard ++ ", " ++ show OneShot ++ ", " ++ show Continuous
  putStrLn "Conflict Actions:"
  putStrLn $ "  " ++ show Probe ++ ", " ++ show Defend ++ ", " ++ show Withdraw
  putStrLn "Service Flags:"
  putStrLn $ "  " ++ show Unique ++ ", " ++ show Shared
  putStrLn ""
  putStrLn $ "mDNS port: " ++ show mdnsPort
  putStrLn $ "mDNS IPv4: " ++ mdnsAddr
  putStrLn $ "mDNS IPv6: " ++ mdnsAddr6
  putStrLn $ "Max record TTL: " ++ show maxRecordTTL
