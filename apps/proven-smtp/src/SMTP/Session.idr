-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SMTP Session State Machine (RFC 5321 Section 4.1.4)
--
-- The SMTP session progresses through a sequence of states:
-- Connected -> Greeted -> MailFrom -> RcptTo -> Data -> Quit.
-- Each state constrains which commands are valid. Invalid transitions
-- produce typed error values rather than crashing the server.

module SMTP.Session

import SMTP.Command
import SMTP.Reply

%default total

-- ============================================================================
-- Session states (RFC 5321 Section 4.1.4)
-- ============================================================================

||| SMTP session states. The session moves through these states in order.
||| RSET returns to Greeted; QUIT moves to Quit from any post-greeting state.
public export
data SessionState : Type where
  ||| Connection established, awaiting HELO/EHLO.
  Connected : SessionState
  ||| HELO/EHLO received, ready for MAIL FROM.
  Greeted   : SessionState
  ||| MAIL FROM received, ready for RCPT TO.
  MailFrom  : SessionState
  ||| At least one RCPT TO received, ready for more RCPT TO or DATA.
  RcptTo    : SessionState
  ||| DATA command received, reading message body.
  InData    : SessionState
  ||| QUIT received, session is ending.
  Quit      : SessionState

public export
Eq SessionState where
  Connected == Connected = True
  Greeted   == Greeted   = True
  MailFrom  == MailFrom  = True
  RcptTo    == RcptTo    = True
  InData    == InData    = True
  Quit      == Quit      = True
  _         == _         = False

public export
Show SessionState where
  show Connected = "Connected"
  show Greeted   = "Greeted"
  show MailFrom  = "MailFrom"
  show RcptTo    = "RcptTo"
  show InData    = "Data"
  show Quit      = "Quit"

-- ============================================================================
-- Transition validation
-- ============================================================================

||| Check whether a command is valid in the given session state.
||| This function is total: every (state, command) pair is handled.
public export
isValidTransition : SessionState -> Command -> Bool
-- Connected: only HELO, EHLO, QUIT, NOOP allowed
isValidTransition Connected (HELO _)  = True
isValidTransition Connected (EHLO _)  = True
isValidTransition Connected QUIT      = True
isValidTransition Connected NOOP      = True
isValidTransition Connected _         = False
-- Greeted: MAIL FROM, RSET, QUIT, NOOP, VRFY allowed
isValidTransition Greeted (MAIL_FROM _) = True
isValidTransition Greeted (HELO _)      = True
isValidTransition Greeted (EHLO _)      = True
isValidTransition Greeted RSET          = True
isValidTransition Greeted QUIT          = True
isValidTransition Greeted NOOP          = True
isValidTransition Greeted (VRFY _)      = True
isValidTransition Greeted _             = False
-- MailFrom: RCPT TO, RSET, QUIT, NOOP allowed
isValidTransition MailFrom (RCPT_TO _) = True
isValidTransition MailFrom RSET        = True
isValidTransition MailFrom QUIT        = True
isValidTransition MailFrom NOOP        = True
isValidTransition MailFrom _           = False
-- RcptTo: more RCPT TO, DATA, RSET, QUIT, NOOP allowed
isValidTransition RcptTo (RCPT_TO _) = True
isValidTransition RcptTo DATA        = True
isValidTransition RcptTo RSET        = True
isValidTransition RcptTo QUIT        = True
isValidTransition RcptTo NOOP        = True
isValidTransition RcptTo _           = False
-- InData: no commands accepted (reading raw body)
isValidTransition InData _ = False
-- Quit: nothing valid (session is over)
isValidTransition Quit _ = False

-- ============================================================================
-- State transitions
-- ============================================================================

||| The result of applying a command to a session state.
public export
record TransitionResult where
  constructor MkTransition
  ||| The new session state after the transition.
  newState : SessionState
  ||| The reply to send to the client.
  reply    : Reply

||| Apply a command to the current session state.
||| Returns the new state and the reply to send.
||| This function is total: every input combination produces a result.
public export
applyCommand : SessionState -> Command -> TransitionResult
-- Connected state
applyCommand Connected (HELO d) = MkTransition Greeted
  (customReply ActionOK ("Hello " ++ d))
applyCommand Connected (EHLO d) = MkTransition Greeted
  (customReply ActionOK ("Hello " ++ d ++ ", pleased to meet you"))
applyCommand Connected QUIT = MkTransition Quit
  (defaultReply ServiceClosing)
applyCommand Connected NOOP = MkTransition Connected
  (defaultReply ActionOK)
applyCommand Connected _ = MkTransition Connected
  (defaultReply BadSequence)
-- Greeted state
applyCommand Greeted (HELO d) = MkTransition Greeted
  (customReply ActionOK ("Hello " ++ d))
applyCommand Greeted (EHLO d) = MkTransition Greeted
  (customReply ActionOK ("Hello " ++ d ++ ", pleased to meet you"))
applyCommand Greeted (MAIL_FROM p) = MkTransition MailFrom
  (customReply ActionOK ("Sender <" ++ p ++ "> OK"))
applyCommand Greeted RSET = MkTransition Greeted
  (defaultReply ActionOK)
applyCommand Greeted QUIT = MkTransition Quit
  (defaultReply ServiceClosing)
applyCommand Greeted NOOP = MkTransition Greeted
  (defaultReply ActionOK)
applyCommand Greeted (VRFY u) = MkTransition Greeted
  (customReply ActionOK ("User " ++ u))
applyCommand Greeted _ = MkTransition Greeted
  (defaultReply BadSequence)
-- MailFrom state
applyCommand MailFrom (RCPT_TO p) = MkTransition RcptTo
  (customReply ActionOK ("Recipient <" ++ p ++ "> OK"))
applyCommand MailFrom RSET = MkTransition Greeted
  (defaultReply ActionOK)
applyCommand MailFrom QUIT = MkTransition Quit
  (defaultReply ServiceClosing)
applyCommand MailFrom NOOP = MkTransition MailFrom
  (defaultReply ActionOK)
applyCommand MailFrom _ = MkTransition MailFrom
  (defaultReply BadSequence)
-- RcptTo state
applyCommand RcptTo (RCPT_TO p) = MkTransition RcptTo
  (customReply ActionOK ("Recipient <" ++ p ++ "> OK"))
applyCommand RcptTo DATA = MkTransition InData
  (defaultReply StartMailInput)
applyCommand RcptTo RSET = MkTransition Greeted
  (defaultReply ActionOK)
applyCommand RcptTo QUIT = MkTransition Quit
  (defaultReply ServiceClosing)
applyCommand RcptTo NOOP = MkTransition RcptTo
  (defaultReply ActionOK)
applyCommand RcptTo _ = MkTransition RcptTo
  (defaultReply BadSequence)
-- InData: commands are not processed (raw body reading)
applyCommand InData _ = MkTransition InData
  (defaultReply BadSequence)
-- Quit: session is over, nothing to do
applyCommand Quit _ = MkTransition Quit
  (defaultReply ServiceClosing)

-- ============================================================================
-- Session record
-- ============================================================================

||| Complete SMTP session state at runtime.
public export
record SMTPSession where
  constructor MkSession
  ||| Current session state.
  state        : SessionState
  ||| The client's domain (from HELO/EHLO).
  clientDomain : String
  ||| The sender address (from MAIL FROM).
  sender       : String
  ||| The list of recipients (from RCPT TO).
  recipients   : List String
  ||| Number of messages processed in this session.
  messageCount : Nat

||| Create a new session in the Connected state.
public export
newSession : SMTPSession
newSession = MkSession
  { state        = Connected
  , clientDomain = ""
  , sender       = ""
  , recipients   = []
  , messageCount = 0
  }

||| Process a command in the session, updating state and generating a reply.
public export
processCommand : SMTPSession -> Command -> (SMTPSession, Reply)
processCommand session cmd =
  let result = applyCommand session.state cmd
      updated = updateSession session cmd result.newState
  in (updated, result.reply)
  where
    updateSession : SMTPSession -> Command -> SessionState -> SMTPSession
    updateSession s (HELO d)      newSt = { state := newSt, clientDomain := d
                                          , sender := "", recipients := [] } s
    updateSession s (EHLO d)      newSt = { state := newSt, clientDomain := d
                                          , sender := "", recipients := [] } s
    updateSession s (MAIL_FROM p) newSt = { state := newSt, sender := p } s
    updateSession s (RCPT_TO p)   newSt = { state := newSt
                                          , recipients $= (p ::) } s
    updateSession s RSET          newSt = { state := newSt, sender := ""
                                          , recipients := [] } s
    updateSession s DATA          newSt = { state := newSt } s
    updateSession s QUIT          newSt = { state := newSt } s
    updateSession s _             newSt = { state := newSt } s

||| Complete a DATA phase (message body received), returning to Greeted.
public export
completeData : SMTPSession -> SMTPSession
completeData s = { state := Greeted
                 , sender := ""
                 , recipients := []
                 , messageCount $= (+ 1)
                 } s
