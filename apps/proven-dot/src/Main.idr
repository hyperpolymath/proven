-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-dot. Prints version info and demonstrates types.
module Main

import DoT

%default total

covering
main : IO ()
main = do
  putStrLn "proven-dot v0.1.0 -- Formally verified DNS over TLS types (RFC 7858)"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "Session States:"
  putStrLn $ "  " ++ show Connecting ++ ", " ++ show Handshaking
            ++ ", " ++ show Established ++ ", " ++ show Closing
            ++ ", " ++ show Closed
  putStrLn "Padding Strategies:"
  putStrLn $ "  " ++ show NoPadding ++ ", " ++ show BlockPadding
            ++ ", " ++ show RandomPadding
  putStrLn "Error Reasons:"
  putStrLn $ "  " ++ show HandshakeFailed ++ ", " ++ show CertificateInvalid
            ++ ", " ++ show Timeout
  putStrLn ""
  putStrLn $ "DoT port: " ++ show dotPort
  putStrLn $ "Idle timeout: " ++ show idleTimeout ++ "s"
