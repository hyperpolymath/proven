-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- POP3.Types: Core protocol types for POP3 (RFC 1939).
--
-- Defines closed sum types for POP3 commands (the 11 commands from RFC 1939
-- and RFC 2449), protocol states (the 3-state machine), and response
-- indicators (+OK / -ERR).

module POP3.Types

%default total

-- ============================================================================
-- POP3 commands (RFC 1939 Section 5, RFC 2449)
-- ============================================================================

||| POP3 commands as defined in RFC 1939 and RFC 2449.
||| Each command is valid only in certain states (Authorization or Transaction).
public export
data Command : Type where
  ||| Identify the mailbox owner (Authorization state).
  User : Command
  ||| Provide the mailbox password (Authorization state, after USER).
  Pass : Command
  ||| Request mailbox statistics: count and total size (Transaction state).
  Stat : Command
  ||| List messages and their sizes (Transaction state).
  List : Command
  ||| Retrieve a specific message by number (Transaction state).
  Retr : Command
  ||| Mark a message for deletion (Transaction state).
  Dele : Command
  ||| No-operation keepalive (Transaction state).
  Noop : Command
  ||| Unmark all messages marked for deletion (Transaction state).
  Rset : Command
  ||| End the session, commit deletions (any state).
  Quit : Command
  ||| Retrieve only the header and first N lines of a message (Transaction).
  Top  : Command
  ||| List unique identifiers for messages (Transaction state).
  Uidl : Command

public export
Eq Command where
  User == User = True
  Pass == Pass = True
  Stat == Stat = True
  List == List = True
  Retr == Retr = True
  Dele == Dele = True
  Noop == Noop = True
  Rset == Rset = True
  Quit == Quit = True
  Top  == Top  = True
  Uidl == Uidl = True
  _    == _    = False

public export
Show Command where
  show User = "USER"
  show Pass = "PASS"
  show Stat = "STAT"
  show List = "LIST"
  show Retr = "RETR"
  show Dele = "DELE"
  show Noop = "NOOP"
  show Rset = "RSET"
  show Quit = "QUIT"
  show Top  = "TOP"
  show Uidl = "UIDL"

-- ============================================================================
-- POP3 session states (RFC 1939 Section 3)
-- ============================================================================

||| The three states of a POP3 session (RFC 1939 Section 3).
||| Authorization -> Transaction -> Update (on QUIT).
public export
data State : Type where
  ||| Client must authenticate with USER/PASS before accessing messages.
  Authorization : State
  ||| Client may access and manipulate messages.
  Transaction   : State
  ||| Session is ending; server commits deletions and releases lock.
  Update        : State

public export
Eq State where
  Authorization == Authorization = True
  Transaction   == Transaction   = True
  Update        == Update        = True
  _             == _             = False

public export
Show State where
  show Authorization = "AUTHORIZATION"
  show Transaction   = "TRANSACTION"
  show Update        = "UPDATE"

-- ============================================================================
-- POP3 response indicators (RFC 1939 Section 3)
-- ============================================================================

||| POP3 response status indicators.
||| Every POP3 response begins with either "+OK" or "-ERR".
public export
data Response : Type where
  ||| Positive response: command succeeded.
  Ok  : Response
  ||| Negative response: command failed.
  Err : Response

public export
Eq Response where
  Ok  == Ok  = True
  Err == Err = True
  Ok  == Err = False
  Err == Ok  = False

public export
Show Response where
  show Ok  = "+OK"
  show Err = "-ERR"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All POP3 commands.
public export
allCommands : List Command
allCommands = [User, Pass, Stat, List, Retr, Dele, Noop, Rset, Quit, Top, Uidl]

||| All POP3 states.
public export
allStates : List State
allStates = [Authorization, Transaction, Update]

||| All POP3 response indicators.
public export
allResponses : List Response
allResponses = [Ok, Err]
