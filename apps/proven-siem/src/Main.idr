-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-siem. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import SIEM

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allEventSeverities : List EventSeverity
allEventSeverities = [Info, Low, Medium, High, Critical]

allEventCategories : List EventCategory
allEventCategories =
  [ Authentication, NetworkTraffic, FileActivity
  , ProcessExecution, PolicyViolation, Malware, DataExfiltration
  ]

allCorrelationRules : List CorrelationRule
allCorrelationRules = [Threshold, Sequence, Aggregation, Absence, Statistical]

allAlertStates : List AlertState
allAlertStates = [New, Acknowledged, InProgress, Resolved, FalsePositive]

main : IO ()
main = do
  putStrLn "proven-siem : Security Information and Event Management server"
  putStrLn $ "  Max events/sec: " ++ show maxEventsPerSecond
  putStrLn $ "  Retention days: " ++ show retentionDays
  putStrLn $ "  EventSeverities:   " ++ show allEventSeverities
  putStrLn $ "  EventCategories:   " ++ show allEventCategories
  putStrLn $ "  CorrelationRules:  " ++ show allCorrelationRules
  putStrLn $ "  AlertStates:       " ++ show allAlertStates
