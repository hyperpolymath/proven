-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-honeypot. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import Honeypot

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allServiceEmulations : List ServiceEmulation
allServiceEmulations = [SSH, HTTP, FTP, SMTP, Telnet, MySQL, RDP]

allInteractionLevels : List InteractionLevel
allInteractionLevels = [Low, Medium, High]

allAlertSeverities : List AlertSeverity
allAlertSeverities = [Info, ASLow, ASMedium, ASHigh, Critical]

allAttackerActions : List AttackerAction
allAttackerActions = [Scan, BruteForce, Exploit, Payload, Lateral, Exfiltration]

main : IO ()
main = do
  putStrLn "proven-honeypot : Network honeypot server"
  putStrLn $ "  Max connections: " ++ show maxConnections
  putStrLn $ "  Log rotate size: " ++ show logRotateSize ++ " bytes"
  putStrLn $ "  ServiceEmulations:  " ++ show allServiceEmulations
  putStrLn $ "  InteractionLevels:  " ++ show allInteractionLevels
  putStrLn $ "  AlertSeverities:    " ++ show allAlertSeverities
  putStrLn $ "  AttackerActions:    " ++ show allAttackerActions
