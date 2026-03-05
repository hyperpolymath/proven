-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-doq. Prints version info and demonstrates types.
module Main

import DoQ

%default total

covering
main : IO ()
main = do
  putStrLn "proven-doq v0.1.0 -- Formally verified DNS over QUIC types (RFC 9250)"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "Stream Types:"
  putStrLn $ "  " ++ show Unidirectional ++ ", " ++ show Bidirectional
  putStrLn "Error Codes:"
  putStrLn $ "  " ++ show NoError ++ ", " ++ show InternalError
            ++ ", " ++ show ExcessiveLoad ++ ", " ++ show ProtocolError
  putStrLn "Session States:"
  putStrLn $ "  " ++ show Initial ++ ", " ++ show Handshaking
            ++ ", " ++ show Ready ++ ", " ++ show Draining
            ++ ", " ++ show Closed
  putStrLn ""
  putStrLn $ "DoQ port: " ++ show doqPort
  putStrLn $ "Max streams: " ++ show maxStreams
