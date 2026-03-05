-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-cache key/value caching server.
||| Prints the server identity, port, and enumerates all type constructors
||| to verify the type definitions are correctly linked.
module Main

import Cache

%default total

||| Print all constructors of a type as a comma-separated list.
printConstructors : String -> List String -> IO ()
printConstructors label vals = putStrLn $ "  " ++ label ++ ": " ++ showList vals
  where
    showList : List String -> String
    showList []        = "(none)"
    showList [x]       = x
    showList (x :: xs) = x ++ ", " ++ showList xs

||| All Command constructors.
allCommands : List Command
allCommands = [Get, Set, Delete, Exists, Expire, TTL, Keys, Flush, Incr, Decr, Append, Prepend, CAS]

||| All EvictionPolicy constructors.
allEvictionPolicies : List EvictionPolicy
allEvictionPolicies = [LRU, LFU, Random, EvictTTL, NoEviction]

||| All DataType constructors.
allDataTypes : List DataType
allDataTypes = [StringVal, IntVal, ListVal, SetVal, HashVal]

||| All ErrorCode constructors.
allErrorCodes : List ErrorCode
allErrorCodes = [NotFound, TypeMismatch, OutOfMemory, KeyTooLong, ValueTooLarge, CASConflict]

||| All ReplicationMode constructors.
allReplicationModes : List ReplicationMode
allReplicationModes = [RNone, Primary, Replica, Sentinel]

||| Entry point. Prints server name, default port, and all type constructors.
main : IO ()
main = do
  putStrLn "proven-cache — Caching Key/Value Store"
  putStrLn $ "Default port: " ++ show cachePort
  putStrLn "Types:"
  printConstructors "Command" (map show allCommands)
  printConstructors "EvictionPolicy" (map show allEvictionPolicies)
  printConstructors "DataType" (map show allDataTypes)
  printConstructors "ErrorCode" (map show allErrorCodes)
  printConstructors "ReplicationMode" (map show allReplicationModes)
