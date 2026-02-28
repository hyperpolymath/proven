-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SMTP Reply Codes (RFC 5321 Section 4.2)
--
-- Reply codes are represented as a sum type with category classification.
-- The first digit determines the category: 2xx positive, 3xx intermediate,
-- 4xx transient negative, 5xx permanent negative. All codes carry a
-- human-readable text message.

module SMTP.Reply

%default total

-- ============================================================================
-- Reply code categories (RFC 5321 Section 4.2.1)
-- ============================================================================

||| SMTP reply code categories based on the first digit.
public export
data ReplyCategory : Type where
  ||| 2xx: Positive completion. The action was successfully completed.
  Positive           : ReplyCategory
  ||| 3xx: Positive intermediate. The command was accepted but the server
  ||| is waiting for further information.
  Intermediate       : ReplyCategory
  ||| 4xx: Transient negative. The command was not accepted but the error
  ||| condition is temporary; retry may succeed.
  TransientNegative  : ReplyCategory
  ||| 5xx: Permanent negative. The command was not accepted and should
  ||| not be retried without changes.
  PermanentNegative  : ReplyCategory

public export
Eq ReplyCategory where
  Positive          == Positive          = True
  Intermediate      == Intermediate      = True
  TransientNegative == TransientNegative = True
  PermanentNegative == PermanentNegative = True
  _                 == _                 = False

public export
Show ReplyCategory where
  show Positive          = "Positive"
  show Intermediate      = "Intermediate"
  show TransientNegative = "TransientNegative"
  show PermanentNegative = "PermanentNegative"

-- ============================================================================
-- SMTP Reply Codes (RFC 5321 Section 4.2.2, 4.2.3)
-- ============================================================================

||| Standard SMTP reply codes.
public export
data ReplyCode : Type where
  ||| 220: Service ready. Sent as greeting when connection opens.
  ServiceReady       : ReplyCode
  ||| 221: Service closing transmission channel.
  ServiceClosing     : ReplyCode
  ||| 250: Requested mail action okay, completed.
  ActionOK           : ReplyCode
  ||| 251: User not local; will forward.
  WillForward        : ReplyCode
  ||| 354: Start mail input; end with <CRLF>.<CRLF>.
  StartMailInput     : ReplyCode
  ||| 421: Service not available, closing transmission channel.
  ServiceUnavailable : ReplyCode
  ||| 450: Requested mail action not taken: mailbox unavailable
  ||| (busy or temporarily blocked).
  MailboxBusy        : ReplyCode
  ||| 451: Requested action aborted: local error in processing.
  LocalError         : ReplyCode
  ||| 452: Requested action not taken: insufficient system storage.
  InsufficientStorage : ReplyCode
  ||| 500: Syntax error, command unrecognised.
  SyntaxError        : ReplyCode
  ||| 501: Syntax error in parameters or arguments.
  ParamSyntaxError   : ReplyCode
  ||| 502: Command not implemented.
  NotImplemented     : ReplyCode
  ||| 503: Bad sequence of commands.
  BadSequence        : ReplyCode
  ||| 504: Command parameter not implemented.
  ParamNotImplemented : ReplyCode
  ||| 550: Requested action not taken: mailbox unavailable
  ||| (not found or no access).
  MailboxUnavailable : ReplyCode
  ||| 553: Requested action not taken: mailbox name not allowed.
  MailboxNameInvalid : ReplyCode
  ||| 554: Transaction failed.
  TransactionFailed  : ReplyCode

public export
Eq ReplyCode where
  ServiceReady        == ServiceReady        = True
  ServiceClosing      == ServiceClosing      = True
  ActionOK            == ActionOK            = True
  WillForward         == WillForward         = True
  StartMailInput      == StartMailInput      = True
  ServiceUnavailable  == ServiceUnavailable  = True
  MailboxBusy         == MailboxBusy         = True
  LocalError          == LocalError          = True
  InsufficientStorage == InsufficientStorage = True
  SyntaxError         == SyntaxError         = True
  ParamSyntaxError    == ParamSyntaxError    = True
  NotImplemented      == NotImplemented      = True
  BadSequence         == BadSequence         = True
  ParamNotImplemented == ParamNotImplemented = True
  MailboxUnavailable  == MailboxUnavailable  = True
  MailboxNameInvalid  == MailboxNameInvalid  = True
  TransactionFailed   == TransactionFailed   = True
  _                   == _                   = False

-- ============================================================================
-- Numeric codes and messages
-- ============================================================================

||| Convert a reply code to its numeric value.
public export
replyToCode : ReplyCode -> Nat
replyToCode ServiceReady        = 220
replyToCode ServiceClosing      = 221
replyToCode ActionOK            = 250
replyToCode WillForward         = 251
replyToCode StartMailInput      = 354
replyToCode ServiceUnavailable  = 421
replyToCode MailboxBusy         = 450
replyToCode LocalError          = 451
replyToCode InsufficientStorage = 452
replyToCode SyntaxError         = 500
replyToCode ParamSyntaxError    = 501
replyToCode NotImplemented      = 502
replyToCode BadSequence         = 503
replyToCode ParamNotImplemented = 504
replyToCode MailboxUnavailable  = 550
replyToCode MailboxNameInvalid  = 553
replyToCode TransactionFailed   = 554

||| Default message text for each reply code.
public export
defaultMessage : ReplyCode -> String
defaultMessage ServiceReady        = "Service ready"
defaultMessage ServiceClosing      = "Service closing transmission channel"
defaultMessage ActionOK            = "OK"
defaultMessage WillForward         = "User not local; will forward"
defaultMessage StartMailInput      = "Start mail input; end with <CRLF>.<CRLF>"
defaultMessage ServiceUnavailable  = "Service not available"
defaultMessage MailboxBusy         = "Mailbox busy"
defaultMessage LocalError          = "Local error in processing"
defaultMessage InsufficientStorage = "Insufficient system storage"
defaultMessage SyntaxError         = "Syntax error, command unrecognised"
defaultMessage ParamSyntaxError    = "Syntax error in parameters"
defaultMessage NotImplemented      = "Command not implemented"
defaultMessage BadSequence         = "Bad sequence of commands"
defaultMessage ParamNotImplemented = "Command parameter not implemented"
defaultMessage MailboxUnavailable  = "Mailbox unavailable"
defaultMessage MailboxNameInvalid  = "Mailbox name not allowed"
defaultMessage TransactionFailed   = "Transaction failed"

public export
Show ReplyCode where
  show code = show (replyToCode code) ++ " " ++ defaultMessage code

-- ============================================================================
-- Category classification
-- ============================================================================

||| Determine the category of a reply code.
public export
categorise : ReplyCode -> ReplyCategory
categorise ServiceReady        = Positive
categorise ServiceClosing      = Positive
categorise ActionOK            = Positive
categorise WillForward         = Positive
categorise StartMailInput      = Intermediate
categorise ServiceUnavailable  = TransientNegative
categorise MailboxBusy         = TransientNegative
categorise LocalError          = TransientNegative
categorise InsufficientStorage = TransientNegative
categorise SyntaxError         = PermanentNegative
categorise ParamSyntaxError    = PermanentNegative
categorise NotImplemented      = PermanentNegative
categorise BadSequence         = PermanentNegative
categorise ParamNotImplemented = PermanentNegative
categorise MailboxUnavailable  = PermanentNegative
categorise MailboxNameInvalid  = PermanentNegative
categorise TransactionFailed   = PermanentNegative

||| Whether the reply indicates success (2xx).
public export
isPositive : ReplyCode -> Bool
isPositive code = categorise code == Positive

||| Whether the reply indicates a permanent error (5xx).
public export
isPermanentError : ReplyCode -> Bool
isPermanentError code = categorise code == PermanentNegative

||| Whether the reply indicates a transient error (4xx, worth retrying).
public export
isTransientError : ReplyCode -> Bool
isTransientError code = categorise code == TransientNegative

-- ============================================================================
-- Reply construction
-- ============================================================================

||| A complete SMTP reply: code + message text.
public export
record Reply where
  constructor MkReply
  ||| The reply code.
  code    : ReplyCode
  ||| The human-readable message text.
  message : String

public export
Show Reply where
  show r = show (replyToCode r.code) ++ " " ++ r.message

||| Build a reply with the default message for the given code.
public export
defaultReply : ReplyCode -> Reply
defaultReply code = MkReply code (defaultMessage code)

||| Build a reply with a custom message.
public export
customReply : ReplyCode -> String -> Reply
customReply = MkReply

||| Serialise a reply to wire format (code + space + message).
public export
serialiseReply : Reply -> String
serialiseReply r = show (replyToCode r.code) ++ " " ++ r.message
