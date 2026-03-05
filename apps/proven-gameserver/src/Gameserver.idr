-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-gameserver: Game server.
--
-- Architecture:
--   - Types: PacketType, PlayerState, GameState, SyncStrategy, DisconnectReason
--
-- This module defines core game server constants and re-exports Gameserver.Types.

module Gameserver

import public Gameserver.Types

%default total

||| Default game server UDP port (Valve-style).
public export
gamePort : Nat
gamePort = 27015

||| Server tick rate in ticks per second.
public export
tickRate : Nat
tickRate = 64

||| Maximum number of players per game session.
public export
maxPlayers : Nat
maxPlayers = 64

||| Client input buffer size in milliseconds (jitter tolerance).
public export
inputBufferMs : Nat
inputBufferMs = 100
