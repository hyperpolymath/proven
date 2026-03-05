-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-loadbalancer load balancer.
||| Prints the server identity, ports, and enumerates all type constructors
||| to verify the type definitions are correctly linked.
module Main

import Loadbalancer

%default total

||| Print all constructors of a type as a comma-separated list.
printConstructors : String -> List String -> IO ()
printConstructors label vals = putStrLn $ "  " ++ label ++ ": " ++ showList vals
  where
    showList : List String -> String
    showList []        = "(none)"
    showList [x]       = x
    showList (x :: xs) = x ++ ", " ++ showList xs

||| All Algorithm constructors.
allAlgorithms : List Algorithm
allAlgorithms = [RoundRobin, LeastConnections, IPHash, Random, WeightedRoundRobin, LeastResponseTime]

||| All HealthCheckType constructors.
allHealthCheckTypes : List HealthCheckType
allHealthCheckTypes = [HcHTTP, HcTCP, HcGRPC, HcScript]

||| All BackendState constructors.
allBackendStates : List BackendState
allBackendStates = [Healthy, Unhealthy, Draining, BDisabled]

||| All SessionPersistence constructors.
allSessionPersistences : List SessionPersistence
allSessionPersistences = [None, Cookie, SourceIP, Header]

||| All Protocol constructors.
allProtocols : List Protocol
allProtocols = [HTTP, HTTPS, TCP, UDP, GRPC]

||| Entry point. Prints server name, default ports, and all type constructors.
main : IO ()
main = do
  putStrLn "proven-loadbalancer — Load Balancer"
  putStrLn $ "Default HTTP port: " ++ show lbPort
  putStrLn $ "Default TLS port: " ++ show lbTLSPort
  putStrLn "Types:"
  printConstructors "Algorithm" (map show allAlgorithms)
  printConstructors "HealthCheckType" (map show allHealthCheckTypes)
  printConstructors "BackendState" (map show allBackendStates)
  printConstructors "SessionPersistence" (map show allSessionPersistences)
  printConstructors "Protocol" (map show allProtocols)
