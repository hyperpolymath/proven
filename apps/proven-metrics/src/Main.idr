-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-metrics telemetry server.
||| Prints the server identity, port, and enumerates all type constructors
||| to verify the type definitions are correctly linked.
module Main

import Metrics

%default total

||| Print all constructors of a type as a comma-separated list.
printConstructors : String -> List String -> IO ()
printConstructors label vals = putStrLn $ "  " ++ label ++ ": " ++ showList vals
  where
    showList : List String -> String
    showList []        = "(none)"
    showList [x]       = x
    showList (x :: xs) = x ++ ", " ++ showList xs

||| All MetricType constructors.
allMetricTypes : List MetricType
allMetricTypes = [Counter, Gauge, Histogram, Summary, Info, StateSet]

||| All ScrapeResult constructors.
allScrapeResults : List ScrapeResult
allScrapeResults = [Success, ScrapeTimeout, ConnectionRefused, InvalidResponse]

||| All AlertState constructors.
allAlertStates : List AlertState
allAlertStates = [Inactive, Pending, Firing, Resolved]

||| All AggregationOp constructors.
allAggregationOps : List AggregationOp
allAggregationOps = [Sum, Avg, Min, Max, Count, Rate, Increase, P50, P90, P95, P99]

||| All QueryError constructors.
allQueryErrors : List QueryError
allQueryErrors = [ParseError, ExecutionError, QueryTimeout, TooManySeries]

||| Entry point. Prints server name, default port, and all type constructors.
main : IO ()
main = do
  putStrLn "proven-metrics — Metrics/Telemetry Server"
  putStrLn $ "Default port: " ++ show metricsPort
  putStrLn "Types:"
  printConstructors "MetricType" (map show allMetricTypes)
  printConstructors "ScrapeResult" (map show allScrapeResults)
  printConstructors "AlertState" (map show allAlertStates)
  printConstructors "AggregationOp" (map show allAggregationOps)
  printConstructors "QueryError" (map show allQueryErrors)
