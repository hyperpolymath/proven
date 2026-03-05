-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-semweb semantic web server.
||| Prints server identification and enumerates core type constructors.
module Main

import Semweb

%default total

||| Print server name, port, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (port " ++ show semwebPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "Max triples: " ++ show maxTriples
  putStrLn $ "Default page size: " ++ show defaultPageSize
  putStrLn ""
  putStrLn "--- Format ---"
  printLn RDFxml
  printLn Turtle
  printLn NTriples
  printLn NQuads
  printLn JSONLD
  printLn Trig
  putStrLn ""
  putStrLn "--- ResourceType ---"
  printLn Class
  printLn Property
  printLn Individual
  printLn Ontology
  printLn NamedGraph
  putStrLn ""
  putStrLn "--- HTTPMethod ---"
  printLn Get
  printLn Post
  printLn Put
  printLn Patch
  printLn Delete
  putStrLn ""
  putStrLn "--- ContentNegotiation ---"
  printLn NegRDFxml
  printLn NegTurtle
  printLn NegJSONLD
  printLn NegHTML
  putStrLn ""
  putStrLn "--- ErrorCode ---"
  printLn NotFound
  printLn InvalidURI
  printLn MalformedRDF
  printLn UnsupportedFormat
  printLn ConflictingTriples
