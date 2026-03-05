-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-gameserver: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Gameserver.Types.
--
-- Usage:
--   idris2 --build proven-gameserver.ipkg
--   ./build/exec/proven-gameserver

module Main

import Gameserver

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-gameserver v0.1.0 -- game server"
  putStrLn ""
  putStrLn $ "Game port:       " ++ show gamePort
  putStrLn $ "Tick rate:       " ++ show tickRate ++ " ticks/sec"
  putStrLn $ "Max players:     " ++ show maxPlayers
  putStrLn $ "Input buffer:    " ++ show inputBufferMs ++ " ms"

  putStrLn "\n--- PacketType ---"
  putStrLn $ "  " ++ show Connect
  putStrLn $ "  " ++ show Disconnect
  putStrLn $ "  " ++ show Input
  putStrLn $ "  " ++ show StateUpdate
  putStrLn $ "  " ++ show Chat
  putStrLn $ "  " ++ show Ping
  putStrLn $ "  " ++ show Pong
  putStrLn $ "  " ++ show Sync
  putStrLn $ "  " ++ show Event

  putStrLn "\n--- PlayerState ---"
  putStrLn $ "  " ++ show Connecting
  putStrLn $ "  " ++ show Lobby
  putStrLn $ "  " ++ show InGame
  putStrLn $ "  " ++ show Spectating
  putStrLn $ "  " ++ show Disconnected

  putStrLn "\n--- GameState ---"
  putStrLn $ "  " ++ show Waiting
  putStrLn $ "  " ++ show Starting
  putStrLn $ "  " ++ show Running
  putStrLn $ "  " ++ show Paused
  putStrLn $ "  " ++ show Ending
  putStrLn $ "  " ++ show Finished

  putStrLn "\n--- SyncStrategy ---"
  putStrLn $ "  " ++ show Lockstep
  putStrLn $ "  " ++ show Rollback
  putStrLn $ "  " ++ show ServerAuth
  putStrLn $ "  " ++ show ClientPrediction

  putStrLn "\n--- DisconnectReason ---"
  putStrLn $ "  " ++ show Timeout
  putStrLn $ "  " ++ show Kicked
  putStrLn $ "  " ++ show Quit
  putStrLn $ "  " ++ show Error
  putStrLn $ "  " ++ show ServerShutdown
