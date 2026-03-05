-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for proven-telnet skeleton.
module Main

import Telnet

%default total

||| Print server identification and type constructors.
covering
main : IO ()
main = do
  putStrLn "proven-telnet — Telnet (RFC 854) skeleton"
  putStrLn $ "  Port: " ++ show telnetPort
  putStrLn $ "  Max Line Length: " ++ show maxLineLength
  putStrLn "Commands:"
  putStrLn $ "  " ++ show SE
  putStrLn $ "  " ++ show NOP
  putStrLn $ "  " ++ show DataMark
  putStrLn $ "  " ++ show Break
  putStrLn $ "  " ++ show InterruptProcess
  putStrLn $ "  " ++ show AbortOutput
  putStrLn $ "  " ++ show AreYouThere
  putStrLn $ "  " ++ show EraseChar
  putStrLn $ "  " ++ show EraseLine
  putStrLn $ "  " ++ show GoAhead
  putStrLn $ "  " ++ show SB
  putStrLn $ "  " ++ show Will
  putStrLn $ "  " ++ show Wont
  putStrLn $ "  " ++ show Do
  putStrLn $ "  " ++ show Dont
  putStrLn $ "  " ++ show IAC
  putStrLn "Options:"
  putStrLn $ "  " ++ show Echo
  putStrLn $ "  " ++ show SuppressGoAhead
  putStrLn $ "  " ++ show Status
  putStrLn $ "  " ++ show TimingMark
  putStrLn $ "  " ++ show TerminalType
  putStrLn $ "  " ++ show WindowSize
  putStrLn $ "  " ++ show TerminalSpeed
  putStrLn $ "  " ++ show RemoteFlowControl
  putStrLn $ "  " ++ show Linemode
  putStrLn $ "  " ++ show Environment
  putStrLn "Negotiation States:"
  putStrLn $ "  " ++ show Inactive
  putStrLn $ "  " ++ show WillSent
  putStrLn $ "  " ++ show DoSent
  putStrLn $ "  " ++ show Active
