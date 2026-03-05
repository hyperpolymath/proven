-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for proven-rtsp skeleton.
module Main

import RTSP

%default total

||| Print server identification and type constructors.
covering
main : IO ()
main = do
  putStrLn "proven-rtsp — RTSP (RFC 7826) skeleton"
  putStrLn $ "  Port: " ++ show rtspPort
  putStrLn $ "  TLS Port: " ++ show rtspsPort
  putStrLn $ "  Session Timeout: " ++ show defaultSessionTimeout ++ "s"
  putStrLn "Methods:"
  putStrLn $ "  " ++ show Describe
  putStrLn $ "  " ++ show Setup
  putStrLn $ "  " ++ show Play
  putStrLn $ "  " ++ show Pause
  putStrLn $ "  " ++ show Teardown
  putStrLn $ "  " ++ show GetParameter
  putStrLn $ "  " ++ show SetParameter
  putStrLn $ "  " ++ show Options
  putStrLn $ "  " ++ show Announce
  putStrLn $ "  " ++ show Record
  putStrLn $ "  " ++ show Redirect
  putStrLn "Transport Protocols:"
  putStrLn $ "  " ++ show RTP_AVP_UDP
  putStrLn $ "  " ++ show RTP_AVP_TCP
  putStrLn $ "  " ++ show RTP_AVP_UDP_Multicast
  putStrLn "Session States:"
  putStrLn $ "  " ++ show Init
  putStrLn $ "  " ++ show Ready
  putStrLn $ "  " ++ show Playing
  putStrLn $ "  " ++ show Recording
  putStrLn "Status Codes:"
  putStrLn $ "  " ++ show OK
  putStrLn $ "  " ++ show MovedPermanently
  putStrLn $ "  " ++ show MovedTemporarily
  putStrLn $ "  " ++ show BadRequest
  putStrLn $ "  " ++ show Unauthorized
  putStrLn $ "  " ++ show NotFound
  putStrLn $ "  " ++ show MethodNotAllowed
  putStrLn $ "  " ++ show NotAcceptable
  putStrLn $ "  " ++ show SessionNotFound
  putStrLn $ "  " ++ show InternalServerError
  putStrLn $ "  " ++ show NotImplemented
  putStrLn $ "  " ++ show ServiceUnavailable
