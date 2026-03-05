-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-ids. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import IDS

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allDetectionModes : List DetectionMode
allDetectionModes = [Signature, Anomaly, Hybrid]

allActions : List Action
allActions = [Alert, Drop, Reject, Log, Pass]

allProtocols : List Protocol
allProtocols = [TCP, UDP, ICMP, DNS, HTTP, TLS, SSH]

allDirections : List Direction
allDirections = [Inbound, Outbound, Both]

allThreatLevels : List ThreatLevel
allThreatLevels = [TLInfo, TLLow, TLMedium, TLHigh, TLCritical]

allRuleMatches : List RuleMatch
allRuleMatches = [SrcAddr, DstAddr, SrcPort, DstPort, Content, Regex, Threshold]

main : IO ()
main = do
  putStrLn "proven-ids : Intrusion Detection/Prevention System"
  putStrLn $ "  Max rules: " ++ show maxRules
  putStrLn $ "  Max packet size: " ++ show maxPacketSize ++ " bytes"
  putStrLn $ "  DetectionModes:  " ++ show allDetectionModes
  putStrLn $ "  Actions:         " ++ show allActions
  putStrLn $ "  Protocols:       " ++ show allProtocols
  putStrLn $ "  Directions:      " ++ show allDirections
  putStrLn $ "  ThreatLevels:    " ++ show allThreatLevels
  putStrLn $ "  RuleMatches:     " ++ show allRuleMatches
