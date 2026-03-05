-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-voip. Prints version info and demonstrates types.
module Main

import VoIP

%default total

covering
main : IO ()
main = do
  putStrLn "proven-voip v0.1.0 -- Formally verified SIP/VoIP protocol types (RFC 3261)"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "SIP Methods:"
  putStrLn $ "  " ++ show Invite ++ ", " ++ show Ack ++ ", " ++ show Bye
            ++ ", " ++ show Register ++ ", " ++ show Subscribe
  putStrLn "Response Codes:"
  putStrLn $ "  " ++ show Trying ++ ", " ++ show Ringing ++ ", " ++ show OK
            ++ ", " ++ show BusyHere
  putStrLn "Dialog States:"
  putStrLn $ "  " ++ show Early ++ ", " ++ show Confirmed ++ ", " ++ show Terminated
  putStrLn ""
  putStrLn $ "SIP port: " ++ show sipPort
  putStrLn $ "SIPS port: " ++ show sipsPort
