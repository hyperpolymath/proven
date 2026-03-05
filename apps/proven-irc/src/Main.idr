-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for proven-irc. Prints version info and demonstrates types.
module Main

import IRC

%default total

covering
main : IO ()
main = do
  putStrLn "proven-irc v0.1.0 -- Formally verified IRC protocol types (RFC 2812)"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn $ "  " ++ show Nick ++ ", " ++ show Join ++ ", " ++ show Privmsg
            ++ ", " ++ show Quit ++ ", " ++ show Ping
  putStrLn "Numeric Replies:"
  putStrLn $ "  " ++ show Welcome ++ ", " ++ show NickInUse
            ++ ", " ++ show NoSuchChannel
  putStrLn "Channel Modes:"
  putStrLn $ "  " ++ show Op ++ ", " ++ show Voice ++ ", " ++ show Ban
            ++ ", " ++ show Secret
  putStrLn ""
  putStrLn $ "IRC port: " ++ show ircPort
  putStrLn $ "IRCS port: " ++ show ircsPort
  putStrLn $ "Max nick length: " ++ show maxNickLength
  putStrLn $ "Max line length: " ++ show maxLineLength
