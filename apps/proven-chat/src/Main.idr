-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-chat server.
||| Prints server identification and enumerates core type constructors.
module Main

import Chat

%default total

||| Print server name, port, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (port " ++ show chatPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "Max message length: " ++ show maxMessageLength ++ " bytes"
  putStrLn $ "Max room members: " ++ show maxRoomMembers
  putStrLn $ "Max file size: " ++ show maxFileSize ++ " bytes"
  putStrLn ""
  putStrLn "--- MessageType ---"
  printLn Text
  printLn Image
  printLn File
  printLn System
  printLn Reaction
  printLn Edit
  printLn Delete
  printLn Reply
  printLn Thread
  putStrLn ""
  putStrLn "--- PresenceStatus ---"
  printLn Online
  printLn Away
  printLn DND
  printLn Invisible
  printLn Offline
  putStrLn ""
  putStrLn "--- RoomType ---"
  printLn Direct
  printLn Group
  printLn Channel
  printLn Broadcast
  putStrLn ""
  putStrLn "--- Permission ---"
  printLn Read
  printLn Write
  printLn Admin
  printLn Invite
  printLn Kick
  printLn Ban
  printLn Pin
  printLn DeleteOthers
  putStrLn ""
  putStrLn "--- Event ---"
  printLn MessageSent
  printLn MessageDelivered
  printLn MessageRead
  printLn UserJoined
  printLn UserLeft
  printLn Typing
  printLn RoomCreated
