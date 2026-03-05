-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for proven-tacacs skeleton.
module Main

import TACACS

%default total

||| Print server identification and type constructors.
covering
main : IO ()
main = do
  putStrLn "proven-tacacs — TACACS+ (RFC 8907) skeleton"
  putStrLn $ "  Port: " ++ show tacacsPort
  putStrLn "Packet Types:"
  putStrLn $ "  " ++ show Authentication
  putStrLn $ "  " ++ show Authorization
  putStrLn $ "  " ++ show Accounting
  putStrLn "Authentication Types:"
  putStrLn $ "  " ++ show ASCII
  putStrLn $ "  " ++ show PAP
  putStrLn $ "  " ++ show CHAP
  putStrLn $ "  " ++ show MSCHAPv1
  putStrLn $ "  " ++ show MSCHAPv2
  putStrLn "Authentication Actions:"
  putStrLn $ "  " ++ show Login
  putStrLn $ "  " ++ show ChangePass
  putStrLn $ "  " ++ show SendAuth
  putStrLn "Authentication Status:"
  putStrLn $ "  " ++ show Pass
  putStrLn $ "  " ++ show Fail
  putStrLn $ "  " ++ show GetData
  putStrLn $ "  " ++ show GetUser
  putStrLn $ "  " ++ show GetPass
  putStrLn $ "  " ++ show Restart
  putStrLn $ "  " ++ show AuthenError
  putStrLn $ "  " ++ show Follow
  putStrLn "Authorization Status:"
  putStrLn $ "  " ++ show PassAdd
  putStrLn $ "  " ++ show PassRepl
  putStrLn $ "  " ++ show AuthorFail
  putStrLn $ "  " ++ show AuthorError
  putStrLn $ "  " ++ show AuthorFollow
  putStrLn "Accounting Status:"
  putStrLn $ "  " ++ show AcctSuccess
  putStrLn $ "  " ++ show AcctError
  putStrLn $ "  " ++ show AcctFollow
  putStrLn "Accounting Flags:"
  putStrLn $ "  " ++ show Start
  putStrLn $ "  " ++ show Stop
  putStrLn $ "  " ++ show Watchdog
