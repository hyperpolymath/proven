-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-triplestore RDF triple store server.
||| Prints server identification and enumerates core type constructors.
module Main

import Triplestore

%default total

||| Print server name, port, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (port " ++ show triplestorePort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "Max graphs: " ++ show maxGraphs
  putStrLn $ "Batch import size: " ++ show batchImportSize
  putStrLn ""
  putStrLn "--- Statement ---"
  printLn Triple
  printLn Quad
  putStrLn ""
  putStrLn "--- IndexOrder ---"
  printLn SPO
  printLn POS
  printLn OSP
  printLn GSPO
  printLn GPOS
  printLn GOSP
  putStrLn ""
  putStrLn "--- StorageBackend ---"
  printLn InMemory
  printLn BTree
  printLn LSM
  printLn Persistent
  putStrLn ""
  putStrLn "--- ImportFormat ---"
  printLn NTriples
  printLn Turtle
  printLn RDFxml
  printLn JSONLD
  printLn NQuads
  printLn Trig
  putStrLn ""
  putStrLn "--- TransactionIsolation ---"
  printLn ReadCommitted
  printLn Serializable
  printLn Snapshot
