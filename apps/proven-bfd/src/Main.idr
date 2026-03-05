-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for proven-bfd skeleton.
module Main

import BFD

%default total

||| Print server identification and type constructors.
covering
main : IO ()
main = do
  putStrLn "proven-bfd — BFD (RFC 5880) skeleton"
  putStrLn $ "  Control Port: " ++ show bfdPort
  putStrLn $ "  Echo Port: " ++ show bfdEchoPort
  putStrLn $ "  Desired Min TX: " ++ show defaultDesiredMinTx ++ " us"
  putStrLn $ "  Required Min RX: " ++ show defaultRequiredMinRx ++ " us"
  putStrLn $ "  Detect Multiplier: " ++ show defaultDetectMult
  putStrLn "States:"
  putStrLn $ "  " ++ show AdminDown
  putStrLn $ "  " ++ show Down
  putStrLn $ "  " ++ show Init
  putStrLn $ "  " ++ show Up
  putStrLn "Diagnostics:"
  putStrLn $ "  " ++ show NoDiagnostic
  putStrLn $ "  " ++ show ControlDetectionTimeExpired
  putStrLn $ "  " ++ show EchoFunctionFailed
  putStrLn $ "  " ++ show NeighborSignaledSessionDown
  putStrLn $ "  " ++ show ForwardingPlaneReset
  putStrLn $ "  " ++ show PathDown
  putStrLn $ "  " ++ show ConcatenatedPathDown
  putStrLn $ "  " ++ show AdministrativelyDown
  putStrLn $ "  " ++ show ReverseConcatenatedPathDown
  putStrLn "Session Modes:"
  putStrLn $ "  " ++ show AsyncMode
  putStrLn $ "  " ++ show DemandMode
