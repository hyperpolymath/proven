-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- FTP Session State Machine (RFC 959 Section 4.1)
--
-- The FTP session progresses through states:
-- Connected -> UserOk -> Authenticated -> Quit.
-- Each state constrains which commands are valid.

module FTP.Session

import FTP.Command
import FTP.Reply
import FTP.Transfer
import FTP.Path

%default total

-- ============================================================================
-- Session states
-- ============================================================================

||| FTP session states.
public export
data SessionState : Type where
  ||| Control connection established, awaiting USER.
  Connected     : SessionState
  ||| USER accepted, awaiting PASS.
  UserOk        : SessionState
  ||| Authenticated, ready for file operations.
  Authenticated : SessionState
  ||| Awaiting RNTO after RNFR.
  Renaming      : SessionState
  ||| QUIT received, session ending.
  Quit          : SessionState

public export
Eq SessionState where
  Connected     == Connected     = True
  UserOk        == UserOk        = True
  Authenticated == Authenticated = True
  Renaming      == Renaming      = True
  Quit          == Quit          = True
  _             == _             = False

public export
Show SessionState where
  show Connected     = "Connected"
  show UserOk        = "UserOk"
  show Authenticated = "Authenticated"
  show Renaming      = "Renaming"
  show Quit          = "Quit"

-- ============================================================================
-- Transition validation
-- ============================================================================

||| Check whether a command is valid in the given session state.
public export
isValidTransition : SessionState -> Command -> Bool
-- Connected: only USER, QUIT, NOOP, SYST
isValidTransition Connected (USER _) = True
isValidTransition Connected QUIT     = True
isValidTransition Connected NOOP     = True
isValidTransition Connected SYST     = True
isValidTransition Connected _        = False
-- UserOk: only PASS, QUIT, NOOP
isValidTransition UserOk (PASS _)    = True
isValidTransition UserOk (USER _)    = True
isValidTransition UserOk QUIT        = True
isValidTransition UserOk NOOP        = True
isValidTransition UserOk _           = False
-- Authenticated: most commands except PASS
isValidTransition Authenticated (PASS _) = False
isValidTransition Authenticated _        = True
-- Renaming: only RNTO allowed
isValidTransition Renaming (RNTO _) = True
isValidTransition Renaming QUIT     = True
isValidTransition Renaming NOOP     = True
isValidTransition Renaming _        = False
-- Quit: nothing valid
isValidTransition Quit _ = False

-- ============================================================================
-- State transitions
-- ============================================================================

||| Apply a command to the current session state.
||| Returns the new state and the reply to send.
public export
applyCommand : SessionState -> Command -> (SessionState, Reply)
-- Connected
applyCommand Connected (USER u) = (UserOk, customReply NeedPassword ("User " ++ u ++ " okay, need password."))
applyCommand Connected QUIT     = (Quit, defaultReply ServiceClosing)
applyCommand Connected NOOP     = (Connected, defaultReply CommandOk)
applyCommand Connected SYST     = (Connected, customReply SystemType "UNIX Type: L8")
applyCommand Connected _        = (Connected, defaultReply NotLoggedIn)
-- UserOk
applyCommand UserOk (PASS _)    = (Authenticated, defaultReply UserLoggedIn)
applyCommand UserOk (USER u)    = (UserOk, customReply NeedPassword ("User " ++ u ++ " okay, need password."))
applyCommand UserOk QUIT        = (Quit, defaultReply ServiceClosing)
applyCommand UserOk NOOP        = (UserOk, defaultReply CommandOk)
applyCommand UserOk _           = (UserOk, defaultReply BadSequence)
-- Authenticated
applyCommand Authenticated (CWD p)     = (Authenticated, customReply FileActionOk ("Directory changed to " ++ p))
applyCommand Authenticated CDUP        = (Authenticated, customReply FileActionOk "Directory changed to parent.")
applyCommand Authenticated PWD         = (Authenticated, customReply PathnameCreated "\"/\"")
applyCommand Authenticated (MKD p)     = (Authenticated, customReply PathnameCreated ("\"" ++ p ++ "\" created."))
applyCommand Authenticated (RMD _)     = (Authenticated, defaultReply FileActionOk)
applyCommand Authenticated (DELE _)    = (Authenticated, defaultReply FileActionOk)
applyCommand Authenticated PASV        = (Authenticated, defaultReply EnteringPassive)
applyCommand Authenticated (PORT _)    = (Authenticated, defaultReply CommandOk)
applyCommand Authenticated (TYPE t)    = case parseType t of
  Just _  => (Authenticated, customReply CommandOk ("Type set to " ++ t ++ "."))
  Nothing => (Authenticated, customReply ParamNotImplemented ("Unknown type: " ++ t))
applyCommand Authenticated (RETR _)    = (Authenticated, defaultReply FileStatusOk)
applyCommand Authenticated (STOR _)    = (Authenticated, defaultReply FileStatusOk)
applyCommand Authenticated (LIST _)    = (Authenticated, defaultReply FileStatusOk)
applyCommand Authenticated (NLST _)    = (Authenticated, defaultReply FileStatusOk)
applyCommand Authenticated (STAT p)    = (Authenticated, defaultReply SystemStatus)
applyCommand Authenticated (SIZE _)    = (Authenticated, defaultReply FileStatus)
applyCommand Authenticated SYST        = (Authenticated, customReply SystemType "UNIX Type: L8")
applyCommand Authenticated (RNFR _)    = (Renaming, defaultReply PendingInfo)
applyCommand Authenticated (USER u)    = (UserOk, customReply NeedPassword ("User " ++ u ++ " okay, need password."))
applyCommand Authenticated QUIT        = (Quit, defaultReply ServiceClosing)
applyCommand Authenticated NOOP        = (Authenticated, defaultReply CommandOk)
applyCommand Authenticated _           = (Authenticated, defaultReply BadSequence)
-- Renaming
applyCommand Renaming (RNTO _) = (Authenticated, defaultReply FileActionOk)
applyCommand Renaming QUIT     = (Quit, defaultReply ServiceClosing)
applyCommand Renaming NOOP     = (Renaming, defaultReply CommandOk)
applyCommand Renaming _        = (Renaming, defaultReply BadSequence)
-- Quit
applyCommand Quit _ = (Quit, defaultReply ServiceClosing)

-- ============================================================================
-- Session record
-- ============================================================================

||| Complete FTP session state.
public export
record FTPSession where
  constructor MkSession
  ||| Current session state.
  state        : SessionState
  ||| Username from USER command.
  username     : String
  ||| Current working directory.
  workingDir   : SafePath
  ||| Current transfer type.
  transferType : TransferType
  ||| Current data connection mode (Nothing = not set).
  dataMode     : Maybe DataMode
  ||| Transfer state.
  transfer     : TransferState
  ||| Number of files transferred this session.
  fileCount    : Nat

||| Create a new session.
public export
newSession : FTPSession
newSession = MkSession
  { state        = Connected
  , username     = ""
  , workingDir   = rootPath
  , transferType = ASCII
  , dataMode     = Nothing
  , transfer     = Idle
  , fileCount    = 0
  }

||| Process a command in the session, updating state and generating a reply.
public export
processCommand : FTPSession -> Command -> (FTPSession, Reply)
processCommand session cmd =
  let (newSt, reply) = applyCommand session.state cmd
      updated = updateSession session cmd newSt
  in (updated, reply)
  where
    updateSession : FTPSession -> Command -> SessionState -> FTPSession
    updateSession s (USER u)  newSt = { state := newSt, username := u } s
    updateSession s (PASS _)  newSt = { state := newSt } s
    updateSession s (CWD p)  newSt  = case validatePath p of
      Right sp => { state := newSt, workingDir := resolvePath s.workingDir sp } s
      Left _   => { state := newSt } s
    updateSession s CDUP     newSt  = { state := newSt } s
    updateSession s (TYPE t) newSt  = case parseType t of
      Just tt => { state := newSt, transferType := tt } s
      Nothing => { state := newSt } s
    updateSession s PASV     newSt  = { state := newSt, dataMode := Just (Passive "0.0.0.0" 0) } s
    updateSession s (PORT h) newSt  = { state := newSt, dataMode := Just (Active h 0) } s
    updateSession s _        newSt  = { state := newSt } s
