-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for proven-netconf skeleton.
module Main

import NETCONF

%default total

||| Print server identification and type constructors.
covering
main : IO ()
main = do
  putStrLn "proven-netconf — NETCONF (RFC 6241) skeleton"
  putStrLn $ "  Port: " ++ show netconfPort
  putStrLn $ "  Max Message Size: " ++ show maxMessageSize ++ " bytes"
  putStrLn "Operations:"
  putStrLn $ "  " ++ show Get
  putStrLn $ "  " ++ show GetConfig
  putStrLn $ "  " ++ show EditConfig
  putStrLn $ "  " ++ show CopyConfig
  putStrLn $ "  " ++ show DeleteConfig
  putStrLn $ "  " ++ show Lock
  putStrLn $ "  " ++ show Unlock
  putStrLn $ "  " ++ show CloseSession
  putStrLn $ "  " ++ show KillSession
  putStrLn $ "  " ++ show Commit
  putStrLn $ "  " ++ show Validate
  putStrLn $ "  " ++ show DiscardChanges
  putStrLn "Datastores:"
  putStrLn $ "  " ++ show Running
  putStrLn $ "  " ++ show Startup
  putStrLn $ "  " ++ show Candidate
  putStrLn "Edit Operations:"
  putStrLn $ "  " ++ show Merge
  putStrLn $ "  " ++ show Replace
  putStrLn $ "  " ++ show Create
  putStrLn $ "  " ++ show Delete
  putStrLn $ "  " ++ show Remove
  putStrLn "Error Types:"
  putStrLn $ "  " ++ show Transport
  putStrLn $ "  " ++ show RPC
  putStrLn $ "  " ++ show Protocol
  putStrLn $ "  " ++ show Application
  putStrLn "Error Severities:"
  putStrLn $ "  " ++ show Error
  putStrLn $ "  " ++ show Warning
