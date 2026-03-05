-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-git Git protocol server.
||| Prints the server identity, ports, and enumerates all type constructors
||| to verify the type definitions are correctly linked.
module Main

import Git

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
allCommands = [UploadPack, ReceivePack, UploadArchive]

||| All PacketType constructors.
allPacketTypes : List PacketType
allPacketTypes = [Flush, Delimiter, ResponseEnd, Data, PktError, SidebandData, SidebandProgress, SidebandError]

||| All RefType constructors.
allRefTypes : List RefType
allRefTypes = [Branch, Tag, Head, Remote, Note]

||| All Capability constructors.
allCapabilities : List Capability
allCapabilities = [MultiAck, ThinPack, SideBand64k, OFSDelta, Shallow, DeepenSince, DeepenNot, FilterSpec, ObjectFormat]

||| All HookResult constructors.
allHookResults : List HookResult
allHookResults = [Accept, Reject]

||| Entry point. Prints server name, default ports, and all type constructors.
main : IO ()
main = do
  putStrLn "proven-git — Git Protocol Server"
  putStrLn $ "Default git:// port: " ++ show gitPort
  putStrLn $ "Default SSH port: " ++ show sshPort
  putStrLn $ "Default HTTPS port: " ++ show httpPort
  putStrLn "Types:"
  printConstructors "Command" (map show allCommands)
  printConstructors "PacketType" (map show allPacketTypes)
  printConstructors "RefType" (map show allRefTypes)
  printConstructors "Capability" (map show allCapabilities)
  printConstructors "HookResult" (map show allHookResults)
