-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-monitor monitoring server.
||| Prints the server identity, port, and enumerates all type constructors
||| to verify the type definitions are correctly linked.
module Main

import Monitor

%default total

||| Print all constructors of a type as a comma-separated list.
||| Used to verify that the closed sum types are correctly defined
||| and their Show instances produce the expected labels.
printConstructors : String -> List String -> IO ()
printConstructors label vals = putStrLn $ "  " ++ label ++ ": " ++ showList vals
  where
    showList : List String -> String
    showList []        = "(none)"
    showList [x]       = x
    showList (x :: xs) = x ++ ", " ++ showList xs

||| All CheckType constructors.
allCheckTypes : List CheckType
allCheckTypes = [HTTP, TCP, UDP, ICMP, DNS, Certificate, Disk, CPU, Memory, Process, Custom]

||| All Status constructors.
allStatuses : List Status
allStatuses = [Up, Down, Degraded, Unknown, Maintenance]

||| All AlertChannel constructors.
allAlertChannels : List AlertChannel
allAlertChannels = [Email, SMS, Webhook, Slack, PagerDuty]

||| All Severity constructors.
allSeverities : List Severity
allSeverities = [Info, Warning, Error, Critical]

||| All CheckState constructors.
allCheckStates : List CheckState
allCheckStates = [Pending, Running, Passed, Failed, Timeout, CSError]

||| Entry point. Prints server name, default port, and all type constructors.
main : IO ()
main = do
  putStrLn "proven-monitor — Monitoring Server"
  putStrLn $ "Default port: " ++ show monitorPort
  putStrLn "Types:"
  printConstructors "CheckType" (map show allCheckTypes)
  printConstructors "Status" (map show allStatuses)
  printConstructors "AlertChannel" (map show allAlertChannels)
  printConstructors "Severity" (map show allSeverities)
  printConstructors "CheckState" (map show allCheckStates)
