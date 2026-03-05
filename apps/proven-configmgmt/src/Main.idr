-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-configmgmt configuration management server.
||| Prints the server identity, port, and enumerates all type constructors
||| to verify the type definitions are correctly linked.
module Main

import Configmgmt

%default total

||| Print all constructors of a type as a comma-separated list.
printConstructors : String -> List String -> IO ()
printConstructors label vals = putStrLn $ "  " ++ label ++ ": " ++ showList vals
  where
    showList : List String -> String
    showList []        = "(none)"
    showList [x]       = x
    showList (x :: xs) = x ++ ", " ++ showList xs

||| All ResourceType constructors.
allResourceTypes : List ResourceType
allResourceTypes = [File, Package, Service, User, Group, Cron, Mount, Firewall, Registry]

||| All ResourceState constructors.
allResourceStates : List ResourceState
allResourceStates = [Present, Absent, Running, Stopped, Enabled, Disabled]

||| All ChangeAction constructors.
allChangeActions : List ChangeAction
allChangeActions = [Create, Modify, Delete, Restart, Reload, Skip]

||| All DriftStatus constructors.
allDriftStatuses : List DriftStatus
allDriftStatuses = [InSync, Drifted, DUnknown, Unmanaged]

||| All ApplyMode constructors.
allApplyModes : List ApplyMode
allApplyModes = [Enforce, DryRun, Audit]

||| Entry point. Prints server name, default port, and all type constructors.
main : IO ()
main = do
  putStrLn "proven-configmgmt — Configuration Management Server"
  putStrLn $ "Default port: " ++ show configPort
  putStrLn "Types:"
  printConstructors "ResourceType" (map show allResourceTypes)
  printConstructors "ResourceState" (map show allResourceStates)
  printConstructors "ChangeAction" (map show allChangeActions)
  printConstructors "DriftStatus" (map show allDriftStatuses)
  printConstructors "ApplyMode" (map show allApplyModes)
