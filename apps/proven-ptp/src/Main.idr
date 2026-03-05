-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-ptp skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import PTP

%default total

||| All PTP message type constructors for demonstration.
allMessageTypes : List MessageType
allMessageTypes =
  [ Sync, DelayReq, PdelayReq, PdelayResp, FollowUp
  , DelayResp, PdelayRespFollowUp, Announce, Signaling, Management ]

||| All PTP clock class constructors for demonstration.
allClockClasses : List ClockClass
allClockClasses = [PrimaryClock, ApplicationSpecific, SlaveOnly, DefaultClass]

||| All PTP port state constructors for demonstration.
allPortStates : List PortState
allPortStates =
  [ Initializing, Faulty, Disabled, Listening, PreMaster
  , Master, Passive, Uncalibrated, Slave ]

||| All PTP delay mechanism constructors for demonstration.
allDelayMechanisms : List DelayMechanism
allDelayMechanisms = [E2E, P2P, DMDisabled]

main : IO ()
main = do
  putStrLn "proven-ptp: IEEE 1588 Precision Time Protocol"
  putStrLn $ "  Event port:   " ++ show ptpEventPort
  putStrLn $ "  General port: " ++ show ptpGeneralPort
  putStrLn $ "  Domain:       " ++ show ptpDomain
  putStrLn $ "  Message types:    " ++ show allMessageTypes
  putStrLn $ "  Clock classes:    " ++ show allClockClasses
  putStrLn $ "  Port states:      " ++ show allPortStates
  putStrLn $ "  Delay mechanisms: " ++ show allDelayMechanisms
