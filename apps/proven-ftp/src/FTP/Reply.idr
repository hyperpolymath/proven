-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- FTP Reply Codes (RFC 959 Section 4.2)
--
-- FTP uses 3-digit reply codes. The first digit indicates the category:
--   1xx - Positive preliminary (action started, expect another reply)
--   2xx - Positive completion (action completed successfully)
--   3xx - Positive intermediate (need more information)
--   4xx - Transient negative (try again later)
--   5xx - Permanent negative (do not retry)

module FTP.Reply

%default total

-- ============================================================================
-- Reply categories
-- ============================================================================

||| FTP reply code categories (RFC 959 Section 4.2.1).
public export
data ReplyCategory : Type where
  ||| Action started, expect another reply before sending next command.
  Preliminary   : ReplyCategory
  ||| Action completed successfully.
  Completion    : ReplyCategory
  ||| Command accepted but need more information.
  Intermediate  : ReplyCategory
  ||| Temporary failure — the action may succeed if retried.
  TransientNeg  : ReplyCategory
  ||| Permanent failure — do not retry this command.
  PermanentNeg  : ReplyCategory

public export
Show ReplyCategory where
  show Preliminary  = "Preliminary"
  show Completion   = "Completion"
  show Intermediate = "Intermediate"
  show TransientNeg = "TransientNegative"
  show PermanentNeg = "PermanentNegative"

-- ============================================================================
-- Reply codes
-- ============================================================================

||| FTP reply codes from RFC 959 and RFC 2228.
public export
data ReplyCode : Type where
  ||| 110 - Restart marker reply.
  RestartMarker       : ReplyCode
  ||| 120 - Service ready in N minutes.
  ServiceReadySoon    : ReplyCode
  ||| 125 - Data connection already open, transfer starting.
  DataConnOpen        : ReplyCode
  ||| 150 - File status okay, about to open data connection.
  FileStatusOk        : ReplyCode
  ||| 200 - Command okay.
  CommandOk           : ReplyCode
  ||| 211 - System status or help reply.
  SystemStatus        : ReplyCode
  ||| 212 - Directory status.
  DirectoryStatus     : ReplyCode
  ||| 213 - File status.
  FileStatus          : ReplyCode
  ||| 214 - Help message.
  HelpMessage         : ReplyCode
  ||| 215 - NAME system type.
  SystemType          : ReplyCode
  ||| 220 - Service ready for new user.
  ServiceReady        : ReplyCode
  ||| 221 - Service closing control connection.
  ServiceClosing      : ReplyCode
  ||| 225 - Data connection open; no transfer in progress.
  DataConnIdle        : ReplyCode
  ||| 226 - Closing data connection. Transfer complete.
  TransferComplete    : ReplyCode
  ||| 227 - Entering Passive Mode (h1,h2,h3,h4,p1,p2).
  EnteringPassive     : ReplyCode
  ||| 230 - User logged in, proceed.
  UserLoggedIn        : ReplyCode
  ||| 250 - Requested file action okay, completed.
  FileActionOk        : ReplyCode
  ||| 257 - "PATHNAME" created.
  PathnameCreated     : ReplyCode
  ||| 331 - User name okay, need password.
  NeedPassword        : ReplyCode
  ||| 332 - Need account for login.
  NeedAccount         : ReplyCode
  ||| 350 - Requested file action pending further information.
  PendingInfo         : ReplyCode
  ||| 421 - Service not available, closing control connection.
  ServiceUnavailable  : ReplyCode
  ||| 425 - Can't open data connection.
  CantOpenData        : ReplyCode
  ||| 426 - Connection closed; transfer aborted.
  TransferAborted     : ReplyCode
  ||| 450 - Requested file action not taken. File unavailable.
  FileUnavailable     : ReplyCode
  ||| 451 - Requested action aborted. Local error in processing.
  LocalError          : ReplyCode
  ||| 452 - Requested action not taken. Insufficient storage space.
  InsufficientStorage : ReplyCode
  ||| 500 - Syntax error, command unrecognised.
  SyntaxError         : ReplyCode
  ||| 501 - Syntax error in parameters or arguments.
  ParamSyntaxError    : ReplyCode
  ||| 502 - Command not implemented.
  NotImplemented      : ReplyCode
  ||| 503 - Bad sequence of commands.
  BadSequence         : ReplyCode
  ||| 504 - Command not implemented for that parameter.
  ParamNotImplemented : ReplyCode
  ||| 530 - Not logged in.
  NotLoggedIn         : ReplyCode
  ||| 550 - Requested action not taken. File unavailable.
  ActionNotTaken      : ReplyCode
  ||| 553 - Requested action not taken. File name not allowed.
  FileNameNotAllowed  : ReplyCode

||| Numeric value of a reply code.
public export
replyNumber : ReplyCode -> Nat
replyNumber RestartMarker       = 110
replyNumber ServiceReadySoon    = 120
replyNumber DataConnOpen        = 125
replyNumber FileStatusOk        = 150
replyNumber CommandOk           = 200
replyNumber SystemStatus        = 211
replyNumber DirectoryStatus     = 212
replyNumber FileStatus          = 213
replyNumber HelpMessage         = 214
replyNumber SystemType          = 215
replyNumber ServiceReady        = 220
replyNumber ServiceClosing      = 221
replyNumber DataConnIdle        = 225
replyNumber TransferComplete    = 226
replyNumber EnteringPassive     = 227
replyNumber UserLoggedIn        = 230
replyNumber FileActionOk        = 250
replyNumber PathnameCreated     = 257
replyNumber NeedPassword        = 331
replyNumber NeedAccount         = 332
replyNumber PendingInfo         = 350
replyNumber ServiceUnavailable  = 421
replyNumber CantOpenData        = 425
replyNumber TransferAborted     = 426
replyNumber FileUnavailable     = 450
replyNumber LocalError          = 451
replyNumber InsufficientStorage = 452
replyNumber SyntaxError         = 500
replyNumber ParamSyntaxError    = 501
replyNumber NotImplemented      = 502
replyNumber BadSequence         = 503
replyNumber ParamNotImplemented = 504
replyNumber NotLoggedIn         = 530
replyNumber ActionNotTaken      = 550
replyNumber FileNameNotAllowed  = 553

||| Classify a reply code into its category.
public export
replyCategory : ReplyCode -> ReplyCategory
replyCategory RestartMarker       = Preliminary
replyCategory ServiceReadySoon    = Preliminary
replyCategory DataConnOpen        = Preliminary
replyCategory FileStatusOk        = Preliminary
replyCategory CommandOk           = Completion
replyCategory SystemStatus        = Completion
replyCategory DirectoryStatus     = Completion
replyCategory FileStatus          = Completion
replyCategory HelpMessage         = Completion
replyCategory SystemType          = Completion
replyCategory ServiceReady        = Completion
replyCategory ServiceClosing      = Completion
replyCategory DataConnIdle        = Completion
replyCategory TransferComplete    = Completion
replyCategory EnteringPassive     = Completion
replyCategory UserLoggedIn        = Completion
replyCategory FileActionOk        = Completion
replyCategory PathnameCreated     = Completion
replyCategory NeedPassword        = Intermediate
replyCategory NeedAccount         = Intermediate
replyCategory PendingInfo         = Intermediate
replyCategory ServiceUnavailable  = TransientNeg
replyCategory CantOpenData        = TransientNeg
replyCategory TransferAborted     = TransientNeg
replyCategory FileUnavailable     = TransientNeg
replyCategory LocalError          = TransientNeg
replyCategory InsufficientStorage = TransientNeg
replyCategory SyntaxError         = PermanentNeg
replyCategory ParamSyntaxError    = PermanentNeg
replyCategory NotImplemented      = PermanentNeg
replyCategory BadSequence         = PermanentNeg
replyCategory ParamNotImplemented = PermanentNeg
replyCategory NotLoggedIn         = PermanentNeg
replyCategory ActionNotTaken      = PermanentNeg
replyCategory FileNameNotAllowed  = PermanentNeg

public export
Show ReplyCode where
  show c = show (replyNumber c)

||| Whether a reply indicates success (1xx, 2xx, 3xx).
public export
isPositive : ReplyCode -> Bool
isPositive c = case replyCategory c of
  Preliminary  => True
  Completion   => True
  Intermediate => True
  TransientNeg => False
  PermanentNeg => False

-- ============================================================================
-- Reply record
-- ============================================================================

||| A complete FTP reply: code plus human-readable message.
public export
record Reply where
  constructor MkReply
  code    : ReplyCode
  message : String

public export
Show Reply where
  show r = show (replyNumber r.code) ++ " " ++ r.message

||| Format a reply for the wire (code + space + message, no CRLF).
public export
serialiseReply : Reply -> String
serialiseReply = show

||| Build a reply from a code with a default message.
public export
defaultReply : ReplyCode -> Reply
defaultReply ServiceReady        = MkReply ServiceReady "Service ready."
defaultReply ServiceClosing      = MkReply ServiceClosing "Goodbye."
defaultReply CommandOk           = MkReply CommandOk "Command okay."
defaultReply UserLoggedIn        = MkReply UserLoggedIn "User logged in, proceed."
defaultReply NeedPassword        = MkReply NeedPassword "User name okay, need password."
defaultReply NotLoggedIn         = MkReply NotLoggedIn "Not logged in."
defaultReply BadSequence         = MkReply BadSequence "Bad sequence of commands."
defaultReply SyntaxError         = MkReply SyntaxError "Syntax error, command unrecognised."
defaultReply TransferComplete    = MkReply TransferComplete "Transfer complete."
defaultReply EnteringPassive     = MkReply EnteringPassive "Entering Passive Mode."
defaultReply FileActionOk        = MkReply FileActionOk "Requested file action okay."
defaultReply PathnameCreated     = MkReply PathnameCreated "Pathname created."
defaultReply FileStatusOk        = MkReply FileStatusOk "File status okay."
defaultReply c                   = MkReply c (show (replyNumber c))

||| Build a reply with a custom message.
public export
customReply : ReplyCode -> String -> Reply
customReply = MkReply
