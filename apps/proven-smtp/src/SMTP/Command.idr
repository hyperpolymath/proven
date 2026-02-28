-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SMTP Commands (RFC 5321 Section 4.1)
--
-- All SMTP commands are represented as a closed sum type. Each command
-- carries its required parameters. Unrecognised command strings parse
-- to Nothing rather than crashing the server.

module SMTP.Command

%default total

-- ============================================================================
-- SMTP Commands (RFC 5321 Section 4.1)
-- ============================================================================

||| SMTP commands as defined in RFC 5321.
||| Each constructor carries the command's required parameter, if any.
public export
data Command : Type where
  ||| HELO: Identify the client to the server (RFC 5321 Section 4.1.1.1).
  HELO : (domain : String) -> Command
  ||| EHLO: Extended HELLO for ESMTP (RFC 5321 Section 4.1.1.1).
  EHLO : (domain : String) -> Command
  ||| MAIL FROM: Specify the sender's mailbox (RFC 5321 Section 4.1.1.2).
  MAIL_FROM : (reversePath : String) -> Command
  ||| RCPT TO: Specify a recipient mailbox (RFC 5321 Section 4.1.1.3).
  RCPT_TO : (forwardPath : String) -> Command
  ||| DATA: Begin message body transmission (RFC 5321 Section 4.1.1.4).
  DATA : Command
  ||| QUIT: Close the connection (RFC 5321 Section 4.1.1.10).
  QUIT : Command
  ||| RSET: Reset the session state (RFC 5321 Section 4.1.1.5).
  RSET : Command
  ||| NOOP: No operation (RFC 5321 Section 4.1.1.9).
  NOOP : Command
  ||| VRFY: Verify a user name or mailbox (RFC 5321 Section 4.1.1.6).
  VRFY : (user : String) -> Command

public export
Eq Command where
  (HELO a)      == (HELO b)      = a == b
  (EHLO a)      == (EHLO b)      = a == b
  (MAIL_FROM a) == (MAIL_FROM b) = a == b
  (RCPT_TO a)   == (RCPT_TO b)   = a == b
  DATA          == DATA          = True
  QUIT          == QUIT          = True
  RSET          == RSET          = True
  NOOP          == NOOP          = True
  (VRFY a)      == (VRFY b)      = a == b
  _             == _             = False

public export
Show Command where
  show (HELO d)      = "HELO " ++ d
  show (EHLO d)      = "EHLO " ++ d
  show (MAIL_FROM p) = "MAIL FROM:<" ++ p ++ ">"
  show (RCPT_TO p)   = "RCPT TO:<" ++ p ++ ">"
  show DATA          = "DATA"
  show QUIT          = "QUIT"
  show RSET          = "RSET"
  show NOOP          = "NOOP"
  show (VRFY u)      = "VRFY " ++ u

-- ============================================================================
-- Command classification
-- ============================================================================

||| The verb (keyword) of a command, without parameters.
public export
commandVerb : Command -> String
commandVerb (HELO _)      = "HELO"
commandVerb (EHLO _)      = "EHLO"
commandVerb (MAIL_FROM _) = "MAIL"
commandVerb (RCPT_TO _)   = "RCPT"
commandVerb DATA          = "DATA"
commandVerb QUIT          = "QUIT"
commandVerb RSET          = "RSET"
commandVerb NOOP          = "NOOP"
commandVerb (VRFY _)      = "VRFY"

||| Whether a command requires an active session (post-HELO/EHLO).
public export
requiresSession : Command -> Bool
requiresSession (HELO _)      = False
requiresSession (EHLO _)      = False
requiresSession QUIT          = False
requiresSession NOOP          = False
requiresSession RSET          = False
requiresSession _             = True

||| Whether a command resets the mail transaction state.
public export
resetsTransaction : Command -> Bool
resetsTransaction RSET      = True
resetsTransaction (HELO _)  = True
resetsTransaction (EHLO _)  = True
resetsTransaction _         = False

-- ============================================================================
-- Command parsing
-- ============================================================================

||| Parse errors for SMTP commands.
public export
data CommandParseError : Type where
  ||| The command line is empty.
  EmptyCommand    : CommandParseError
  ||| The command verb is not recognised.
  UnknownCommand  : (verb : String) -> CommandParseError
  ||| A required parameter is missing.
  MissingParam    : (verb : String) -> CommandParseError
  ||| The command line exceeds the maximum length.
  LineTooLong     : (len : Nat) -> CommandParseError

public export
Show CommandParseError where
  show EmptyCommand       = "Empty command line"
  show (UnknownCommand v) = "Unknown command: " ++ v
  show (MissingParam v)   = "Missing parameter for " ++ v
  show (LineTooLong n)    = "Command line too long: " ++ show n ++ " chars"

||| Extract the parameter portion after the verb in a command line.
||| Returns the verb and the rest of the line (trimmed).
extractVerbAndParam : String -> (String, String)
extractVerbAndParam s =
  let parts = break (== ' ') s
  in (toUpper (fst parts), ltrim (snd parts))

||| Parse an SMTP command line into a typed Command.
||| Returns Left for malformed or unrecognised commands (no crash).
public export
parseCommand : String -> Either CommandParseError Command
parseCommand s =
  if length s == 0 then Left EmptyCommand
  else
    let (verb, param) = extractVerbAndParam s
    in case verb of
         "HELO" => if length param == 0
                      then Left (MissingParam "HELO")
                      else Right (HELO param)
         "EHLO" => if length param == 0
                      then Left (MissingParam "EHLO")
                      else Right (EHLO param)
         "MAIL" => Right (MAIL_FROM param)
         "RCPT" => Right (RCPT_TO param)
         "DATA" => Right DATA
         "QUIT" => Right QUIT
         "RSET" => Right RSET
         "NOOP" => Right NOOP
         "VRFY" => if length param == 0
                      then Left (MissingParam "VRFY")
                      else Right (VRFY param)
         _      => Left (UnknownCommand verb)

||| Serialise a command to its wire format (no CRLF appended).
public export
serialiseCommand : Command -> String
serialiseCommand = show
