-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-chat server.
||| Defines closed sum types for message types, presence, rooms,
||| permissions, and chat events.
module Chat.Types

%default total

---------------------------------------------------------------------------
-- Message type: the kind of chat message
---------------------------------------------------------------------------

||| Classification of a chat message.
public export
data MessageType : Type where
  Text     : MessageType
  Image    : MessageType
  File     : MessageType
  System   : MessageType
  Reaction : MessageType
  Edit     : MessageType
  Delete   : MessageType
  Reply    : MessageType
  Thread   : MessageType

export
Show MessageType where
  show Text     = "Text"
  show Image    = "Image"
  show File     = "File"
  show System   = "System"
  show Reaction = "Reaction"
  show Edit     = "Edit"
  show Delete   = "Delete"
  show Reply    = "Reply"
  show Thread   = "Thread"

---------------------------------------------------------------------------
-- Presence status: user online status
---------------------------------------------------------------------------

||| User presence / availability status.
public export
data PresenceStatus : Type where
  Online    : PresenceStatus
  Away      : PresenceStatus
  DND       : PresenceStatus
  Invisible : PresenceStatus
  Offline   : PresenceStatus

export
Show PresenceStatus where
  show Online    = "Online"
  show Away      = "Away"
  show DND       = "DND"
  show Invisible = "Invisible"
  show Offline   = "Offline"

---------------------------------------------------------------------------
-- Room type: the kind of chat room
---------------------------------------------------------------------------

||| Classification of a chat room.
public export
data RoomType : Type where
  Direct    : RoomType
  Group     : RoomType
  Channel   : RoomType
  Broadcast : RoomType

export
Show RoomType where
  show Direct    = "Direct"
  show Group     = "Group"
  show Channel   = "Channel"
  show Broadcast = "Broadcast"

---------------------------------------------------------------------------
-- Permission: access control for chat operations
---------------------------------------------------------------------------

||| Permission flags for chat room access control.
public export
data Permission : Type where
  Read         : Permission
  Write        : Permission
  Admin        : Permission
  Invite       : Permission
  Kick         : Permission
  Ban          : Permission
  Pin          : Permission
  DeleteOthers : Permission

export
Show Permission where
  show Read         = "Read"
  show Write        = "Write"
  show Admin        = "Admin"
  show Invite       = "Invite"
  show Kick         = "Kick"
  show Ban          = "Ban"
  show Pin          = "Pin"
  show DeleteOthers = "DeleteOthers"

---------------------------------------------------------------------------
-- Event: chat system events
---------------------------------------------------------------------------

||| Events emitted by the chat system.
public export
data Event : Type where
  MessageSent      : Event
  MessageDelivered : Event
  MessageRead      : Event
  UserJoined       : Event
  UserLeft         : Event
  Typing           : Event
  RoomCreated      : Event

export
Show Event where
  show MessageSent      = "MessageSent"
  show MessageDelivered = "MessageDelivered"
  show MessageRead      = "MessageRead"
  show UserJoined       = "UserJoined"
  show UserLeft         = "UserLeft"
  show Typing           = "Typing"
  show RoomCreated      = "RoomCreated"
