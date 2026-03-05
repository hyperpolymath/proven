-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-chat server.
||| Re-exports core types and provides server constants.
module Chat

import public Chat.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default TLS port for the chat server.
public export
chatPort : Nat
chatPort = 8443

||| Maximum message length in bytes (4 KiB).
public export
maxMessageLength : Nat
maxMessageLength = 4096

||| Maximum number of members per chat room.
public export
maxRoomMembers : Nat
maxRoomMembers = 10000

||| Maximum file upload size in bytes (100 MiB).
public export
maxFileSize : Nat
maxFileSize = 104857600

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-chat"
