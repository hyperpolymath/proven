-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-graphdb graph database server.
||| Prints server identification and enumerates core type constructors.
module Main

import Graphdb

%default total

||| Print server name, ports, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (Bolt port " ++ show boltPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "HTTP port: " ++ show httpPort
  putStrLn $ "Max node properties: " ++ show maxNodeProperties
  putStrLn ""
  putStrLn "--- ElementType ---"
  printLn Node
  printLn Edge
  printLn Property
  printLn Label
  printLn Index
  putStrLn ""
  putStrLn "--- QueryLanguage ---"
  printLn Cypher
  printLn Gremlin
  printLn SPARQL
  printLn GraphQL
  putStrLn ""
  putStrLn "--- TraversalStrategy ---"
  printLn BFS
  printLn DFS
  printLn Dijkstra
  printLn AStar
  printLn Random
  putStrLn ""
  putStrLn "--- Consistency ---"
  printLn Strong
  printLn Eventual
  printLn Session
  printLn Causal
  putStrLn ""
  putStrLn "--- ErrorCode ---"
  printLn SyntaxError
  printLn NodeNotFound
  printLn EdgeNotFound
  printLn ConstraintViolation
  printLn IndexExists
  printLn TransactionConflict
  printLn OutOfMemory
