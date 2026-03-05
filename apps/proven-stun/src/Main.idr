-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-stun. Prints version info and demonstrates types.
module Main

import STUN

%default total

covering
main : IO ()
main = do
  putStrLn "proven-stun v0.1.0 -- Formally verified STUN/TURN protocol types (RFC 8489)"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "Message Types:"
  putStrLn $ "  " ++ show BindingRequest ++ ", " ++ show BindingResponse
            ++ ", " ++ show AllocateRequest ++ ", " ++ show ChannelBind
  putStrLn "Transport Protocols:"
  putStrLn $ "  " ++ show UDP ++ ", " ++ show TCP ++ ", " ++ show TLS
            ++ ", " ++ show DTLS
  putStrLn "Error Codes:"
  putStrLn $ "  " ++ show BadRequest ++ ", " ++ show Unauthorized
            ++ ", " ++ show ServerError
  putStrLn ""
  putStrLn $ "STUN port: " ++ show stunPort
  putStrLn $ "STUNS port: " ++ show stunsPort
