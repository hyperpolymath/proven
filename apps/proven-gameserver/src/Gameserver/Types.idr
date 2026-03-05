-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-gameserver: Core protocol types for game server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Gameserver.Types

%default total

-- ============================================================================
-- PacketType
-- ============================================================================

||| Types of network packets exchanged between game clients and server.
public export
data PacketType : Type where
  ||| Client requests to join the game session.
  Connect     : PacketType
  ||| Client or server signals disconnection.
  Disconnect  : PacketType
  ||| Client sends player input (movement, actions).
  Input       : PacketType
  ||| Server sends authoritative game state to clients.
  StateUpdate : PacketType
  ||| Chat message between players.
  Chat        : PacketType
  ||| Client-to-server latency probe.
  Ping        : PacketType
  ||| Server-to-client latency response.
  Pong        : PacketType
  ||| Full state synchronisation (e.g. on rejoin).
  Sync        : PacketType
  ||| Game event notification (score, kill, objective, etc.).
  Event       : PacketType

export
Show PacketType where
  show Connect     = "Connect"
  show Disconnect  = "Disconnect"
  show Input       = "Input"
  show StateUpdate = "StateUpdate"
  show Chat        = "Chat"
  show Ping        = "Ping"
  show Pong        = "Pong"
  show Sync        = "Sync"
  show Event       = "Event"

-- ============================================================================
-- PlayerState
-- ============================================================================

||| State of a player within the game server.
public export
data PlayerState : Type where
  ||| Player is establishing a connection.
  Connecting   : PlayerState
  ||| Player is in the lobby waiting for a match.
  Lobby        : PlayerState
  ||| Player is actively playing.
  InGame       : PlayerState
  ||| Player is observing without participating.
  Spectating   : PlayerState
  ||| Player has disconnected (may reconnect).
  Disconnected : PlayerState

export
Show PlayerState where
  show Connecting   = "Connecting"
  show Lobby        = "Lobby"
  show InGame       = "InGame"
  show Spectating   = "Spectating"
  show Disconnected = "Disconnected"

-- ============================================================================
-- GameState
-- ============================================================================

||| State of the game session as a whole.
public export
data GameState : Type where
  ||| Waiting for enough players to start.
  Waiting  : GameState
  ||| Countdown before the game begins.
  Starting : GameState
  ||| Game is actively in progress.
  Running  : GameState
  ||| Game is temporarily paused.
  Paused   : GameState
  ||| Game is wrapping up (final score, replays).
  Ending   : GameState
  ||| Game session is complete.
  Finished : GameState

export
Show GameState where
  show Waiting  = "Waiting"
  show Starting = "Starting"
  show Running  = "Running"
  show Paused   = "Paused"
  show Ending   = "Ending"
  show Finished = "Finished"

-- ============================================================================
-- SyncStrategy
-- ============================================================================

||| Network synchronisation strategy for multiplayer state.
public export
data SyncStrategy : Type where
  ||| Deterministic lockstep: all clients advance in unison.
  Lockstep         : SyncStrategy
  ||| Rollback netcode: predict locally, correct on mismatch.
  Rollback         : SyncStrategy
  ||| Server is authoritative; clients receive corrections.
  ServerAuth       : SyncStrategy
  ||| Client-side prediction with server reconciliation.
  ClientPrediction : SyncStrategy

export
Show SyncStrategy where
  show Lockstep         = "Lockstep"
  show Rollback         = "Rollback"
  show ServerAuth       = "ServerAuth"
  show ClientPrediction = "ClientPrediction"

-- ============================================================================
-- DisconnectReason
-- ============================================================================

||| Reason a player was disconnected from the game server.
public export
data DisconnectReason : Type where
  ||| Connection timed out (no packets received).
  Timeout        : DisconnectReason
  ||| Player was kicked by an administrator or anti-cheat.
  Kicked         : DisconnectReason
  ||| Player voluntarily quit.
  Quit           : DisconnectReason
  ||| Network or protocol error.
  Error          : DisconnectReason
  ||| Server is shutting down.
  ServerShutdown : DisconnectReason

export
Show DisconnectReason where
  show Timeout        = "Timeout"
  show Kicked         = "Kicked"
  show Quit           = "Quit"
  show Error          = "Error"
  show ServerShutdown = "ServerShutdown"
