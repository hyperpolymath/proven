-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- IMAP.Types: Core protocol types for IMAP4rev1 (RFC 3501).
--
-- Defines closed sum types for IMAP commands (the 14 standard commands),
-- protocol states (the 4-state machine from RFC 3501 Section 3), and
-- system flags defined in RFC 3501 Section 2.3.2.

module IMAP.Types

%default total

-- ============================================================================
-- IMAP commands (RFC 3501 Section 6)
-- ============================================================================

||| Standard IMAP4rev1 commands from RFC 3501.
||| Organised by the state in which they are valid: any state, not-authenticated,
||| authenticated, or selected.
public export
data Command : Type where
  ||| Authenticate with username and password (not-authenticated state).
  Login      : Command
  ||| End the session (any state).
  Logout     : Command
  ||| Select a mailbox for read/write access (authenticated state).
  Select     : Command
  ||| Select a mailbox for read-only access (authenticated state).
  Examine    : Command
  ||| Create a new mailbox (authenticated state).
  Create     : Command
  ||| Delete an existing mailbox (authenticated state).
  Delete     : Command
  ||| Rename a mailbox (authenticated state).
  Rename     : Command
  ||| List mailboxes matching a pattern (authenticated state).
  List       : Command
  ||| Retrieve message data (selected state).
  Fetch      : Command
  ||| Alter message flags (selected state).
  Store      : Command
  ||| Search for messages matching criteria (selected state).
  Search     : Command
  ||| Copy messages to another mailbox (selected state).
  Copy       : Command
  ||| No-operation keepalive (any state).
  Noop       : Command
  ||| Request server capabilities (any state).
  Capability : Command

public export
Eq Command where
  Login      == Login      = True
  Logout     == Logout     = True
  Select     == Select     = True
  Examine    == Examine    = True
  Create     == Create     = True
  Delete     == Delete     = True
  Rename     == Rename     = True
  List       == List       = True
  Fetch      == Fetch      = True
  Store      == Store      = True
  Search     == Search     = True
  Copy       == Copy       = True
  Noop       == Noop       = True
  Capability == Capability = True
  _          == _          = False

public export
Show Command where
  show Login      = "LOGIN"
  show Logout     = "LOGOUT"
  show Select     = "SELECT"
  show Examine    = "EXAMINE"
  show Create     = "CREATE"
  show Delete     = "DELETE"
  show Rename     = "RENAME"
  show List       = "LIST"
  show Fetch      = "FETCH"
  show Store      = "STORE"
  show Search     = "SEARCH"
  show Copy       = "COPY"
  show Noop       = "NOOP"
  show Capability = "CAPABILITY"

-- ============================================================================
-- IMAP connection states (RFC 3501 Section 3)
-- ============================================================================

||| The four states of an IMAP4rev1 connection.
||| Transitions are governed by successful command execution.
public export
data State : Type where
  ||| Initial state before authentication.
  NotAuthenticated : State
  ||| Authenticated but no mailbox selected.
  Authenticated    : State
  ||| A mailbox has been selected for access.
  Selected         : State
  ||| Connection is being closed.
  LogoutState      : State

public export
Eq State where
  NotAuthenticated == NotAuthenticated = True
  Authenticated    == Authenticated    = True
  Selected         == Selected         = True
  LogoutState      == LogoutState      = True
  _                == _                = False

public export
Show State where
  show NotAuthenticated = "NotAuthenticated"
  show Authenticated    = "Authenticated"
  show Selected         = "Selected"
  show LogoutState      = "Logout"

-- ============================================================================
-- Message flags (RFC 3501 Section 2.3.2)
-- ============================================================================

||| System flags defined in RFC 3501 Section 2.3.2.
||| These are the standard flags that an IMAP server MUST support.
public export
data Flag : Type where
  ||| Message has been read (\Seen).
  Seen     : Flag
  ||| Message has been answered (\Answered).
  Answered : Flag
  ||| Message is flagged for urgent/special attention (\Flagged).
  Flagged  : Flag
  ||| Message is marked for deletion (\Deleted).
  Deleted  : Flag
  ||| Message is a draft (\Draft).
  Draft    : Flag
  ||| Message is recently arrived (\Recent) -- server-managed, read-only.
  Recent   : Flag

public export
Eq Flag where
  Seen     == Seen     = True
  Answered == Answered = True
  Flagged  == Flagged  = True
  Deleted  == Deleted  = True
  Draft    == Draft    = True
  Recent   == Recent   = True
  _        == _        = False

public export
Show Flag where
  show Seen     = "\\Seen"
  show Answered = "\\Answered"
  show Flagged  = "\\Flagged"
  show Deleted  = "\\Deleted"
  show Draft    = "\\Draft"
  show Recent   = "\\Recent"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All IMAP commands.
public export
allCommands : List Command
allCommands = [Login, Logout, Select, Examine, Create, Delete, Rename,
               List, Fetch, Store, Search, Copy, Noop, Capability]

||| All IMAP states.
public export
allStates : List State
allStates = [NotAuthenticated, Authenticated, Selected, LogoutState]

||| All message flags.
public export
allFlags : List Flag
allFlags = [Seen, Answered, Flagged, Deleted, Draft, Recent]
