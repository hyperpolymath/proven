-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-vpn. Prints version info and demonstrates types.
module Main

import VPN

%default total

covering
main : IO ()
main = do
  putStrLn "proven-vpn v0.1.0 -- Formally verified WireGuard-style VPN protocol types"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "Message Types:"
  putStrLn $ "  " ++ show Handshake ++ ", " ++ show HandshakeResponse
            ++ ", " ++ show CookieReply ++ ", " ++ show Transport
  putStrLn "Handshake States:"
  putStrLn $ "  " ++ show Empty ++ ", " ++ show InitSent
            ++ ", " ++ show InitReceived ++ ", " ++ show Established
  putStrLn "Peer States:"
  putStrLn $ "  " ++ show Connected ++ ", " ++ show Disconnected
            ++ ", " ++ show Expired
  putStrLn "Error Reasons:"
  putStrLn $ "  " ++ show InvalidMAC ++ ", " ++ show DecryptionFailed
            ++ ", " ++ show ReplayDetected ++ ", " ++ show HandshakeTimeout
  putStrLn ""
  putStrLn $ "VPN port: " ++ show vpnPort
  putStrLn $ "Keepalive interval: " ++ show keepaliveInterval ++ "s"
  putStrLn $ "Handshake timeout: " ++ show handshakeTimeout ++ "s"
