-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-logcollector structured log ingestion server.
||| Prints the server identity, ports, and enumerates all type constructors
||| to verify the type definitions are correctly linked.
module Main

import Logcollector

%default total

||| Print all constructors of a type as a comma-separated list.
printConstructors : String -> List String -> IO ()
printConstructors label vals = putStrLn $ "  " ++ label ++ ": " ++ showList vals
  where
    showList : List String -> String
    showList []        = "(none)"
    showList [x]       = x
    showList (x :: xs) = x ++ ", " ++ showList xs

||| All LogLevel constructors.
allLogLevels : List LogLevel
allLogLevels = [Trace, Debug, Info, Warn, Error, Fatal]

||| All InputFormat constructors.
allInputFormats : List InputFormat
allInputFormats = [JSON, Logfmt, Syslog, CEF, GELF, Raw]

||| All OutputTarget constructors.
allOutputTargets : List OutputTarget
allOutputTargets = [File, Elasticsearch, S3, Kafka, Stdout]

||| All FilterOp constructors.
allFilterOps : List FilterOp
allFilterOps = [Include, Exclude, Transform, Redact, Sample]

||| All PipelineStage constructors.
allPipelineStages : List PipelineStage
allPipelineStages = [Input, Parse, Filter, PTransform, Output]

||| Entry point. Prints server name, default ports, and all type constructors.
main : IO ()
main = do
  putStrLn "proven-logcollector — Structured Log Ingestion Server"
  putStrLn $ "Default syslog port: " ++ show logPort
  putStrLn $ "Default gRPC port: " ++ show grpcPort
  putStrLn $ "Default HTTP port: " ++ show httpPort
  putStrLn "Types:"
  printConstructors "LogLevel" (map show allLogLevels)
  printConstructors "InputFormat" (map show allInputFormats)
  printConstructors "OutputTarget" (map show allOutputTargets)
  printConstructors "FilterOp" (map show allFilterOps)
  printConstructors "PipelineStage" (map show allPipelineStages)
