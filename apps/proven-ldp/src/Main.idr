-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-ldp Linked Data Platform server.
||| Prints server identification and enumerates core type constructors.
module Main

import Ldp

%default total

||| Print server name, port, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (port " ++ show ldpPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "Max resource size: " ++ show maxResourceSize ++ " bytes"
  putStrLn ""
  putStrLn "--- ContainerType ---"
  printLn Basic
  printLn Direct
  printLn Indirect
  putStrLn ""
  putStrLn "--- ResourceType ---"
  printLn RDFSource
  printLn NonRDFSource
  printLn Container
  putStrLn ""
  putStrLn "--- Preference ---"
  printLn MinimalContainer
  printLn IncludeContainment
  printLn IncludeMembership
  printLn OmitContainment
  printLn OmitMembership
  putStrLn ""
  putStrLn "--- InteractionModel ---"
  printLn LDPR
  printLn LDPC
  printLn LDPBasicContainer
  printLn LDPDirectContainer
  printLn LDPIndirectContainer
  putStrLn ""
  putStrLn "--- ConstraintViolation ---"
  printLn MembershipConstant
  printLn ContainsTriplesModified
  printLn ServerManaged
  printLn TypeConflict
