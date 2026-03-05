-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-sparql endpoint.
||| Prints server identification and enumerates core type constructors.
module Main

import Sparql

%default total

||| Print server name, port, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (port " ++ show sparqlPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "Default timeout: " ++ show defaultTimeout ++ "s"
  putStrLn $ "Max result size: " ++ show maxResultSize
  putStrLn ""
  putStrLn "--- QueryType ---"
  printLn Select
  printLn Construct
  printLn Ask
  printLn Describe
  putStrLn ""
  putStrLn "--- UpdateType ---"
  printLn Insert
  printLn Delete
  printLn Load
  printLn Clear
  printLn Create
  printLn Drop
  putStrLn ""
  putStrLn "--- ResultFormat ---"
  printLn XML
  printLn JSON
  printLn CSV
  printLn TSV
  putStrLn ""
  putStrLn "--- ErrorType ---"
  printLn ParseError
  printLn QueryTimeout
  printLn ResultsTooLarge
  printLn UnknownGraph
  printLn AccessDenied
